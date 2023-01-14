library(shiny)
library(rgdal)
library(shinyjs)
library(plotly)
library(igraph)
library(shinyalert)
library(surveillance)
library(openxlsx)

MonteCarlo.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            disable("start.sim")
            disable("save_sim")
            
            result = reactiveValues(
                params = data.frame(Probabilities = 0, Queen = F, numIter = 1000),
                adj.mat = NA,
                sim.down = NA,
                sim.up = NA
            )
            
            sim.data = reactiveVal(NULL)
            
            success.parse = reactiveVal(F)
            
            observe({
                toggleState(id = "start.sim", condition = !is.null(sim.data()))
                toggleState(id = "save_sim", condition = !is.na(result$adj.mat))
            })
            
            observeEvent(input$ogr_dir, {
                    tryCatch({
                        shpdf <- input$ogr_dir
                        
                        # The files are uploaded with names
                        # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
                        # (path/names are in column datapath)
                        # We need to rename the files with the actual names:
                        # fe_2007_39_county.dbf, etc.
                        # (these are in column name)
                        
                        # Name of the temporary directory where files are uploaded
                        
                        tempdirname <- dirname(shpdf$datapath[1])
                        
                        # Rename files
                        for (i in 1:nrow(shpdf)) {
                            file.rename(
                                shpdf$datapath[i],
                                paste0(tempdirname, "/", shpdf$name[i])
                            )
                        }
                        print(shpdf)
                        
                        sim.data(tryCatch(expr = {
                            polygons = readOGR(dsn = paste(tempdirname,
                                                           shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                                           sep = "/"))   
                            success.parse(T)
                            polygons
                        },
                        error = function(e){
                            shinyalert("Error", 
                                       paste("Не могу считать данные, потому что",e),
                                       type = "error"
                            )
                            NULL
                        }))
                        # Now we read the shapefile with readOGR() of rgdal package
                        # passing the name of the file with .shp extension.
                        
                        # We use the function grep() to search the pattern "*.shp$"
                        # within each element of the character vector shpdf$name.
                        # grep(pattern="*.shp$", shpdf$name)
                        # ($ at the end denote files that finish with .shp,
                        # not only that contain .shp)
                    },
                    error = function(e){
                       shinyalert("Error",
                                   paste("Не могу считать данные, потому что ",e),
                                   type = "error"
                        )
                    })
                # }
            })
            
            observeEvent(input$start.sim, {
                disable("start.sim")
                disable("ogr_dir")
                disable("save_sim")
                if(success.parse()){
                    n.iter = isolate(input$number.iteration)
                    p.down = isolate(input$mark.limits[1])
                    p.up = isolate(input$mark.limits[2])
                    queen = isolate(input$queen)
                    
                    
                    if ((p.up == 0 || p.up == 1) && (p.down == 1 || p.down == 0))
                    {
                        shinyalert("Warning", 
                                   "Выберите корректные пределы!",
                                   type = "warning"
                        )
                    }
                    else{
                        result$params[1,"Probabilities"] = p.down
                        result$params[2,"Probabilities"] = p.up
                        result$params$Queen = queen
                        result$params$numIter = n.iter
                        
                        cat("Start simulation\n")
                        
                        withProgress(message = "Симуляция", {
                            histogram = data.frame(Size = 0,SumProbabilities = 0, MeanProbabilities = 0,ExpectedNn = 0,PMaxNGreaterNi = 0)
                            max.clust.size = data.frame()
                            incProgress(0.2,message = "Построение матрицы смежности")
                            #print(sim.data$data)
                            adjacencyMatrix = poly2adjmat(sim.data(), queen = queen, zero.policy = T,row.names = sim.data()@data$GID_1)
                            result$adj.mat = adjacencyMatrix
                            incProgress(0.1)
                            if(p.down != 0){
                                print("Down simulation")
                                p.mark = p.down
                                for (i in 1:n.iter) {
                                    print(paste("------------------","\n","Iteration #",i))
                                    #mark random regions with probability marks.probs
                                    itendif = sample(0:1,nrow(adjacencyMatrix),replace = T,prob = c(1 - p.mark, p.mark))
                                    select = which(itendif != 0, arr.ind = T)
                                    print(paste("Selection", "Length =",length(select)))
                                    #find connected(neibourhood AND marked) regions - clusters and this sizes
                                    graph = graph.adjacency(adjacencyMatrix[select, select])
                                    comps <- components(graph)
                                    clusters = igraph::groups(comps)
                                    sizes = sapply(clusters, length)
                                    print(paste("Num discharges =",length(sizes)))
                                    
                                    if (length(sizes) == 0) {
                                        max.clust.size[nrow(max.clust.size) + 1, 1] = 0
                                        size.probs = 0
                                        names(size.probs) = c("0")
                                    } 
                                    else{
                                        max.clust.size[nrow(max.clust.size)+1,1] = max(sizes)
                                        num.sizes = table(sizes)
                                        size.probs = num.sizes / length(sizes)
                                    }
                                    
                                    size.probs = size.probs[order(as.numeric(names(size.probs)))]
                                    print(paste("Sizes = ",size.probs))
                                    k = 1
                                    for (j in names(size.probs)) {
                                        id = as.numeric(j)
                                        
                                        print(paste("Size =",id))
                                        
                                        if(!(id %in% histogram$Size)){
                                            histogram[nrow(histogram)+1,] = c(id,0,0,num.sizes[j],0)
                                        }
                                        
                                        histogram[histogram$Size == id, "SumProbabilities"] = histogram[histogram$Size == id, "SumProbabilities"] + size.probs[j]
                                        print(paste("SumProbabilities = ",histogram[histogram$Size == id, "SumProbabilities"]))
                                        
                                        histogram[histogram$Size == id, "ExpectedNn"] = histogram[histogram$Size == id, "ExpectedNn"] + num.sizes[j]
                                        print(paste("ExpectedNn = ",histogram[histogram$Size == id, "ExpectedNn"]))
                                        
                                        revCDF = ifelse(length(size.probs) == 1, size.probs, sum(size.probs[k:nrow(size.probs)]))
                                        k = k + 1
                                        print(paste("revCDF = ",revCDF))
                                        
                                        histogram[histogram$Size == id, "PMaxNGreaterNi"] = histogram[histogram$Size == id, "PMaxNGreaterNi"] + revCDF
                                        print(paste("PMaxNGreaterNi = ",histogram[histogram$Size == id, "PMaxNGreaterNi"]))
                                        print("Calculated")  
                                    }
                                    
                                    incProgress(
                                        amount = 1 / 2*n.iter+0.01,
                                        message = paste("Нижняя граница. Итерация", i, "из", n.iter)
                                    )
                                }
                                #PDF of cluster size
                                histogram$MeanProbabilities = histogram$SumProbabilities / n.iter
                                histogram$ExpectedNn = histogram$ExpectedNn / n.iter
                                histogram$PMaxNGreaterNi = histogram$PMaxNGreaterNi / n.iter
                                histogram = histogram[order(histogram$Size), ]
                                result$sim.down = histogram[-1,]
                            }
                            
                            
                            if(p.down == 1 - p.up){
                                result$sim.up = histogram[-1,]
                            }
                            else if(1 - p.up != 0){
                                histogram = data.frame(Size = 0,SumProbabilities = 0, MeanProbabilities = 0,ExpectedNn = 0,PMaxNGreaterNi = 0)
                                incProgress(0.1)
                                print("Up simulation")
                                p.mark = 1 - p.up
                                for (i in 1:n.iter) {
                                    print(paste("------------------","\n","Iteration #",i))
                                    #mark random regions with probability marks.probs
                                    itendif = sample(0:1,nrow(adjacencyMatrix),replace = T,prob = c(1 - p.mark, p.mark))
                                    select = which(itendif != 0, arr.ind = T)
                                    print(paste("Selection", "Length =",length(select)))
                                    #find connected(neibourhood AND marked) regions - clusters and this sizes
                                    graph = graph.adjacency(adjacencyMatrix[select, select])
                                    comps <- components(graph)
                                    clusters = igraph::groups(comps)
                                    sizes = sapply(clusters, length)
                                    print(paste("Num clusters =",length(sizes)))
                                    
                                    if (length(sizes) == 0) {
                                        max.clust.size[nrow(max.clust.size) + 1, 1] = 0
                                        size.probs = 0
                                        names(size.probs) = c("0")
                                    } 
                                    else{
                                        max.clust.size[nrow(max.clust.size)+1,1] = max(sizes)
                                        num.sizes = table(sizes)
                                        size.probs = num.sizes / length(sizes)
                                    }
                                    
                                    size.probs = size.probs[order(as.numeric(names(size.probs)))]
                                    print(paste("Sizes = ",size.probs))
                                    k = 1
                                    for (j in names(size.probs)) {
                                        id = as.numeric(j)
                                        
                                        print(paste("Size =",id))
                                        
                                        if(!(id %in% histogram$Size)){
                                            histogram[nrow(histogram)+1,] = c(id,0,0,num.sizes[j],0)
                                        }
                                        
                                        histogram[histogram$Size == id, "SumProbabilities"] = histogram[histogram$Size == id, "SumProbabilities"] + size.probs[j]
                                        print(paste("SumProbabilities = ",histogram[histogram$Size == id, "SumProbabilities"]))
                                        
                                        histogram[histogram$Size == id, "ExpectedNn"] = histogram[histogram$Size == id, "ExpectedNn"] + num.sizes[j]
                                        print(paste("ExpectedNn = ",histogram[histogram$Size == id, "ExpectedNn"]))
                                        
                                        revCDF = ifelse(length(size.probs) == 1, size.probs, sum(size.probs[k:nrow(size.probs)]))
                                        k = k + 1
                                        print(paste("revCDF = ",revCDF))
                                        
                                        histogram[histogram$Size == id, "PMaxNGreaterNi"] = histogram[histogram$Size == id, "PMaxNGreaterNi"] + revCDF
                                        print(paste("PMaxNGreaterNi = ",histogram[histogram$Size == id, "PMaxNGreaterNi"]))
                                        print("Calculated")  
                                    }
                                    
                                    incProgress(
                                        amount = 1 / 2*n.iter+0.01,
                                        message = paste("Верхняя граница. Итерация", i, "из", n.iter)
                                    )
                                }
                                #PDF of cluster size
                                histogram$MeanProbabilities = histogram$SumProbabilities / n.iter
                                histogram$ExpectedNn = histogram$ExpectedNn / n.iter
                                histogram$PMaxNGreaterNi = histogram$PMaxNGreaterNi / n.iter
                                histogram = histogram[order(histogram$Size), ]
                                result$sim.up = histogram[-1,]
                            }
                            
                            
                        })
                        enable("start.sim")
                        enable("save_sim")
                        enable("ogr_dir")
                    }
                }
                else{
                    shinyalert("Warning",
                               "Не могу считать данные. NULL.",
                               type = 'warning')
                    print(sim.data())
                }
            })
            
            output$save_sim = downloadHandler(
                filename = function(){
                    A  =  paste0("simulation_", 
                                 gsub("-","_",Sys.Date(),fixed = T), 
                                 "_down=",
                                 gsub(".",",",input$mark.limits[1],fixed = T)
                                 ,"_up=",
                                 gsub(".",",",input$mark.limits[2],fixed = T),
                                 ".xlsx", sep="")
                    print(A)
                    A
                },
                content = function(file){
                    tryCatch({
                        print(as.character(file))
                        print(result$params)
                        #write.csv(result$params, file)
                        # write.xlsx(result$params, file, asTable = T, sheetName = "Params",rowNames = F)
                        # write.xlsx(as.data.frame(result$adj.mat),file, asTable = T,sheetName = "Adj_Mat",overwrite = F,rowNames = T,colNames = T)
                        # write.xlsx(result$sim.down,file, asTable = T,sheetName = "Down",overwrite = F,rowNames = F)
                        # write.xlsx(result$sim.up,file, asTable = T,sheetName = "Up",overwrite = F,rowNames = F)
                        wb <- createWorkbook()
                        
                        # add worksheets ----------------------------------------------------------
                        
                        addWorksheet(wb, "Params")
                        addWorksheet(wb, "Adj_Mat")
                        addWorksheet(wb, "Down")
                        addWorksheet(wb, "Up")
                        
                        writeData(wb, sheet = "Params", x = result$params)
                        writeData(wb, sheet = "Adj_Mat", x = as.data.frame(result$adj.mat),rowNames = T)
                        writeData(wb, sheet = "Down", x = result$sim.down)
                        writeData(wb, sheet = "Up", x = result$sim.up)
                        
                        saveWorkbook(wb, file, overwrite = T)
                    },error = function(e){
                        shinyalert("Error",
                                   paste("Не могу сохранить результат, потому что ",e),
                                   type = "error"
                        )
                    })
                })
            
            output$down_PDF = renderPlotly({
                validate(need(!is.null(sim.data()),message = "Нет данных."))
                validate(need(result$sim.down,"Запустите симуляцию."))
                # ggplot(result$sim.down) + geom_col(aes(x = Size, y = MeanProbabilities), alpha = 0.85) +
                #     labs(x = "Size of cluster(number of regions)", y = "Probability", title = "Clusters size distribution")
                fig = plot_ly(result$sim.down, x = ~Size, y = ~MeanProbabilities,type = 'bar',color = I("blue"), alpha = 0.85) %>%
                    layout(xaxis = list(title="Размер"),yaxis = list(title="Вероятность"),title="Распределение размеров кластеров")
                fig
            })
            
            output$down_alpha = renderPlotly({
                 validate(need(!is.null(sim.data()),message = "Нет данных."))
                 validate(need(result$sim.down,"Запустите симуляцию."))
                 # ggplot(result$sim.down) + geom_col(aes(x = Size, y = PMaxNGreaterNi), alpha = 0.85) +
                 #     labs(x = "Maximal size of cluster(number of regions)", y = "Probability", title = "Maximal clusters size distribution")
                 fig = plot_ly(result$sim.down, x = ~Size, y = ~PMaxNGreaterNi,type = 'bar',color = I("blue"), alpha = 0.85) %>%
                     layout(xaxis = list(title="Размер"),yaxis = list(title="Вероятность"),title="Распределение максимального размера кластеров")
                 fig
            })
                        
            output$down_table = renderTable({
                validate(need(!is.null(sim.data()),message = "Нет данных."))
                validate(need(result$sim.down,"Запустите симуляцию."))
                result$sim.down
            },digits = 4)
            
            output$up.PDF = renderPlotly({
                validate(need(!is.null(sim.data()),message = "Нет данных."))
                validate(need(result$sim.up,"Запустите симуляцию."))
                #     ggplot(result$sim.up) + geom_col(aes(x = Size, y = MeanProbabilities), alpha = 0.85) +
                #         labs(x = "Size of cluster(number of regions)", y = "Probability", title = "Clusters size distribution")
                fig = plot_ly(result$sim.up, x = ~Size, y = ~MeanProbabilities,type = 'bar',color = I("blue"), alpha = 0.85) %>%
                    layout(xaxis = list(title="Размер"),yaxis = list(title="Вероятность"),title="Распределение размеров разряжений")
                fig
            })
            
            output$up.alpha = renderPlotly({
                validate(need(!is.null(sim.data()),message = "Нет данных."))
                validate(need(result$sim.up,message = "Запустите симуляцию."))
                # ggplot(result$sim.up) + geom_col(aes(x = Size, y = PMaxNGreaterNi), alpha = 0.85) +
                #     labs(x = "Maximal size of cluster(number of regions)", y = "Probability", title = "Maximal clusters size distribution")
                fig = plot_ly(result$sim.up, x = ~Size, y = ~PMaxNGreaterNi,type = 'bar',color = I("blue"), alpha = 0.85) %>%
                    layout(xaxis = list(title="Размер"),yaxis = list(title="Вероятность"),title="Распределение максимального размера разряжений")
                fig
            })
            
            output$up.table = renderTable({
                validate(need(!is.null(sim.data()),message = "Нет данных."))
                validate(need(result$sim.up,"Запустите симуляцию."))
                result$sim.up
            },digits = 4)
            
           
        }
    )
}