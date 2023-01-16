library(leaflet)
library(openxlsx)
library(ggplot2)
library(purrr)
library(plotly)
library(igraph)
library(surveillance)
library(sf)
Main.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            #Initialisation----------------------------------------------------
            

            row = reactiveVal(0)#It is marker of selected region
            render.col = reactiveVal(0)#It is marker of mode view (which column should use)
            
            #Previous step result (monte carlo calculations)    
            monte.carlo = reactiveValues(
                params = NULL,
                adj.mat = NULL,
                sim.down = NULL,
                sim.up = NULL
            )
            
            #Final results
            result = reactiveValues(
                discharges = NULL,
                clusters = NULL
            )
            
            #Only for statistically significant results
            signif.result = reactiveValues(
                discharges = NULL,
                clusters = NULL
            )

            #Observed columns indexes
            obs.columns = reactiveValues(
                first = 0, 
                second = 0
            )
            
            proxy = leafletProxy("map")
            click.list <- reactiveVal(NULL)#Store map click
            
            map.data = reactiveVal(NULL)#initial map data
            copy.map = reactiveVal(NULL)#cached map,using in program
                
            
            #Rendering block-----------------------------------------------------------------

            
            output$map <- renderLeaflet({
                validate(need(map.data(), message = "Загрузите карту"))
                withProgress(message = "Rendering...", {
                    print("Render map! - renderer")
                    leaflet() %>% addProviderTiles("CartoDB.Positron") 
                })
            })
            
            output$down_crit_reg = renderPlotly({
                validate(need(!is.na(monte.carlo$sim.down$S.crit),"Отстутствует критическая область."))
                monte.carlo$sim.down = monte.carlo$sim.down[order(monte.carlo$sim.down$Size),]
                    
                fig = plot_ly(monte.carlo$sim.down, x = ~Size, y = ~S.crit,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
                    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Кластеры")
                if (!is.null(result$clusters)){
                    fig = fig %>%
                        add_trace(data = result$clusters,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
                        add_trace(data = signif.result$clusters,x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label)
                }
                fig
            })
            
            output$down_stat = renderTable({
                validate(need(!is.null(result$clusters),"Отстутствует результат."))
                result$clusters
            },digits = 4)

            output$up.crit.reg = renderPlotly({
                validate(need(!is.na(monte.carlo$sim.up$S.crit),"Отстутствует критическая область."))
                monte.carlo$sim.up = monte.carlo$sim.up[order(monte.carlo$sim.up$Size),]
                
                fig = plot_ly(monte.carlo$sim.up, x = ~Size, y = ~S.crit,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
                    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Разряжения")
                if (!is.null(result$discharges)){
                    fig = fig %>%
                        add_trace(data = result$discharges,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
                        add_trace(data = signif.result$discharges,x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label)
                }
                fig
            })
            
            output$up.stat = renderTable({
                validate(need(!is.null(result$discharges),"Отстутствует результат."))
                result$discharges
            },digits = 4)
            
            
            output$dif.print = renderText({
                req(row() > 0)
                paste(
                    "P для",
                    copy.map()@data$NAME_0[row()],
                    copy.map()@data$NAME_1[row()],
                    copy.map()@data$GID_1[row()],
                    "(Разница)",
                    "=",
                    copy.map()@data$P[row()]
                )
            })
            
            output$first.pvalue = renderText({
                req(row() > 0)
                paste(
                    "P(1 момент)",
                    "=",
                    copy.map()@data$P1[row()]
                )    
                
            })
            
            output$second.pvalue = renderText({
                req(row() > 0)
                paste(
                    "P(2 момент)",
                    "=",
                    copy.map()@data$P2[row()]
                )    
            })
            
            output$save_res = downloadHandler(
                filename = function(){
                    paste("result-", Sys.Date(), "-alpha=",input$alpha,".xlsx", sep="")
                },
                content = function(file){
                    tryCatch({
                        #path = dirname(file)
                        #rgdal::writeOGR(copy.map(),path, "map", driver="ESRI Shapefile",encoding = "UTF-8")
                        wb <- createWorkbook()
                        
                        # add worksheets ----------------------------------------------------------
                        
                        addWorksheet(wb, "Discharges")
                        addWorksheet(wb, "Clusters")
                        addWorksheet(wb, "Crit_Down")
                        addWorksheet(wb, "Crit_Up")
                        addWorksheet(wb, "Adj_Mat")
                        addWorksheet(wb, "Params")
                        
                        writeData(wb, sheet = "Params", x = result$params)
                        writeData(wb, sheet = "Adj_Mat", x = as.data.frame(result$adj.mat),rowNames = T)
                        writeData(wb, sheet = "Down", x = result$sim.down)
                        writeData(wb, sheet = "Up", x = result$sim.up)
                        
                        if(!is.null(result$discharges)){
                            writeData(wb, sheet = "Discharges", x = result$discharges)
                            write.xlsx(result$discharges,file = file,sheetName = "Discharges")
                        }
                        if(!is.null(result$clusters)){
                            writeData(wb, sheet = "Clusters", x = result$clusters)
                        }
                        if(!is.null(monte.carlo$sim.down)){
                            writeData(wb, sheet = "Crit_Down", x = monte.carlo$sim.down)
                        }
                        if(!is.null(monte.carlo$sim.up)){
                            writeData(wb, sheet = "Crit_Up", x = monte.carlo$sim.up)
                        }
                        if(!is.null(monte.carlo$adj.mat)){
                            writeData(wb, sheet = "Adj_Mat", x = monte.carlo$adj.mat)
                        }
                        if(!is.null(monte.carlo$params)){
                            writeData(wb, sheet = "Params", x = monte.carlo$params)
                        }
                        saveWorkbook(wb, file, overwrite = T)
                    },error = function(e){
                        shinyalert("Error",
                                   paste("Не могу сохранить результат, потому что ",e),
                                   type = "error"
                        )
                    })
                })
            
            
            #Block of actions-------------------------------------------------------------
            
            observeEvent({render.col()},ignoreInit = T,ignoreNULL = T,{
                req(render.col() > 0)
                coords = sp::coordinates(copy.map())
                print(paste(min(coords[,1]),min(coords[,2]),max(coords[,1]),max(coords[,2])))
                withProgress(message = "Rendering...",{
                
                proxy %>% clearShapes() %>% addPolygons(
                    data = copy.map(),
                    weight = 1,
                    fillOpacity = 0.25,
                    color = p(copy.map()@data[,render.col()]),
                    layerId = copy.map()@data$GID_1,
                    highlight = highlightOptions(
                        weight = 5,
                        color = p(copy.map()@data[,render.col()]),
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    )
                ) %>% 
                    addLegend(pal = p,
                              title = "P",
                              values = copy.map()@data[,render.col()],
                              layerId = "Legend") %>% 
                        fitBounds(min(coords[,1]),min(coords[,2]),max(coords[,1]),max(coords[,2]))
                    
                    if (row() > 0){
                        region = copy.map()[row(),]
                       proxy %>% addPolylines(
                            data = region,
                            layerId = region@data$GID_1,
                            color = p(region@data[,render.col()]),
                            weight = 5,
                            opacity = 1
                        )
                    }
                    
                    })
            })
            
            
            observe({
                toggleState("clearHighlight", row() != 0 || (!is.null(result$discharges) || !is.null(result$clusters)))
                toggleState("save_res",!is.null(result$discharges) || !is.null(result$clusters))
                toggleState("do.clust", !is.null(map.data()) && !is.null(monte.carlo$params))
                toggle("first.cases", condition = !is.null(click.list()))
                toggle("second.cases", condition = !is.null(click.list()))
            })
            
            observeEvent(input$ogr_dir,{
                if (is.integer(input$ogr_dir)) {
                    cat("No map\n")
                }
                else {
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
                        
                        # Now we read the shapefile with readOGR() of rgdal package
                        # passing the name of the file with .shp extension.
                        
                        # We use the function grep() to search the pattern "*.shp$"
                        # within each element of the character vector shpdf$name.
                        # grep(pattern="*.shp$", shpdf$name)
                        # ($ at the end denote files that finish with .shp,
                        # not only that contain .shp)
                        map.df <- readOGR(paste(tempdirname,
                                             shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                             sep = "/"
                        ))
                        
                        # path = parseDirPath(volumes, input$ogr_dir)
                        # print(paste("Path to map","=",path))
                        # layers = strsplit(path, "/")[[1]]
                        # layer = layers[length(layers)]
                        # print(paste("Layer","=",layer))
                        # 
                        # map.df = rgdal::readOGR(dsn = path, layer = as.character(layer))
                        map.df1 = sf::st_shift_longitude(sf::st_as_sf(map.df))
                        map.df = as(map.df1, "Spatial")
                        map.data(map.df)
                        copy.map(map.df)
                        
                        first.finded = F
                        for (col in 1:ncol(copy.map()@data)) {
                            if(is.numeric(copy.map()@data[,col])){
                                if(first.finded){
                                    obs.columns$second = col
                                    break
                                }
                                else{
                                    obs.columns$first = col
                                    first.finded = T
                                }
                            } 
                        }

                        print(c(obs.columns$first,obs.columns$second))
                        updateSelectInput(session = session,inputId = "first",label = "Выберите первый момент", choices = colnames(copy.map()@data),selected = colnames(copy.map()@data)[obs.columns$first])
                        updateSelectInput(session = session,inputId = "second",label = "Выберите второй момент", choices = colnames(copy.map()@data),selected = colnames(copy.map()@data)[obs.columns$second])
                        
                    },
                    error = function(e){
                        shinyalert("Error",
                                   paste("Не могу считать данные, потому что ",e),
                                   type = "error"
                        )
                    })

                }
            })
            
            
            observeEvent(input$mc_res,{
                  tryCatch({
                      monte.carlo$params = read.xlsx(input$mc_res$datapath,sheet = "Params")
                      monte.carlo$adj.mat = read.xlsx(input$mc_res$datapath,sheet = "Adj_Mat",rowNames = T)
                      monte.carlo$sim.down = read.xlsx(input$mc_res$datapath,sheet = "Down")
                      monte.carlo$sim.up = read.xlsx(input$mc_res$datapath,sheet = "Up")

                  },
                  error = function(e){
                      shinyalert("Error",
                                 paste("Не могу считать данные, потому что ",e),
                                 type = "error"
                      )
                      monte.carlo$params = NULL
                      monte.carlo$adj.mat = NULL
                      monte.carlo$sim.down = NULL
                      monte.carlo$sim.up = NULL
                  })
              })
            
            
            observeEvent(input$representation,{
                req(copy.map())
                if(input$representation == "Разница"){
                    render.col(match("P", colnames(copy.map()@data))[1])
                }
                else if(input$representation == "Первый момент"){
                    render.col(match("P1", colnames(copy.map()@data))[1])
                }
                else if(input$representation == "Второй момент"){
                    render.col(match("P2", colnames(copy.map()@data))[1])
                }
            })
            
            
            calc.first.moment = eventReactive(copy.map()@data[,obs.columns$first],
                {
                data = isolate(copy.map())
                if(!is.numeric(data@data[,obs.columns$first])){
                    shinyalert("Error",
                               paste("Выбран нечисловой параметр для показа"),
                               type = "error"
                    )
                    req(is.numeric(data@data[,obs.columns$first]))#silent exit
                }
                else{
                    first.time = as.numeric(data@data[,obs.columns$first])
                    m1 = mean(first.time,na.rm = T)
                    data@data$X1 = (first.time - m1) / sqrt(m1)
                    data@data$P1 = pnorm(data@data$X1,lower.tail = F)

                    if(input$representation == "Первый момент")
                        render.col(match("P1", colnames(data@data))[1])
                    copy.map(data)
                }
            })
            
            
            calc.second.moment = eventReactive(copy.map()@data[,obs.columns$second],
                {
                data = isolate(copy.map())
                if(!is.numeric(data@data[,obs.columns$second])){
                    shinyalert("Error",
                               paste("Выбран нечисловой параметр для показа"),
                               type = "error"
                    )
                    req(is.numeric(data@data[,obs.columns$second]))#silent exit
                }
                else{
                    second.time = as.numeric(data@data[,obs.columns$second])
                    m2 = mean(second.time,na.rm = T)
                    data@data$X2 = (second.time - m2) / sqrt(m2)
                    data@data$P2 = pnorm(data@data$X2,lower.tail = F)

                    if(input$representation == "Второй момент")
                        render.col(match("P2", colnames(data@data))[1])
                    copy.map(data)
                }
            })

            calc.dif = eventReactive(
                {copy.map()@data[,obs.columns$first]; copy.map()@data[,obs.columns$second]},
                {
                    data = isolate(copy.map())
                    if(!is.numeric(data@data[,obs.columns$first]) ||
                        !is.numeric(data@data[,obs.columns$second])){
                        shinyalert("Error",
                                   paste("Выбран нечисловой параметр для показа"),
                                   type = "error"
                        )
                        req(!is.numeric(data@data[,obs.columns$first]) ||
                            !is.numeric(data@data[,obs.columns$second]))#silent exit
                    }
                    else{
                      first.time = as.numeric(data@data[,obs.columns$first])
                      second.time = as.numeric(data@data[,obs.columns$second])
                                         
                      data@data$X = (first.time - second.time) / (sqrt(first.time + second.time))
                      data@data$P = pnorm(data@data$X,lower.tail = F)
                      print(input$representation)
                      if(input$representation == "Разница")
                          render.col(match("P", colnames(data@data))[1])

                      copy.map(data)
                    }
                })

            observeEvent({input$first
                  input$second },
                  ignoreNULL = T,
                  ignoreInit = T,{
                      
                      req(copy.map(),input$first,input$second)

                      obs.columns$first = input$first
                      obs.columns$second = input$second
                      
                      print(paste("First",input$first,"Second",input$second))
                      
                      calc.dif()
                      calc.first.moment() 
                      calc.second.moment()
                      tmp = render.col()
                      render.col(-1)
                      render.col(tmp)
                  })


            observeEvent(input$map_shape_click,
                             ignoreNULL = T,
                             ignoreInit = T,
            {
                isolate.copy = isolate(copy.map())
                first.time = as.numeric(isolate.copy@data[,obs.columns$first])
                second.time = as.numeric(isolate.copy@data[,obs.columns$second])

                i = which(isolate.copy@data$GID_1 %in% input$map_shape_click)

                if(is.na(first.time[i]) || is.na(second.time[i]) ||
                   is.nan(first.time[i]) || is.nan(second.time[i]) ||
                   is.infinite(first.time[i]) || is.infinite(second.time[i])){
                    req(input$changeableNA)
                }

                req(copy.map()@data[,render.col()])

                ev <- input$map_shape_click

                row(which(isolate.copy@data$GID_1 %in% ev$id))

                print("-----------------------------------------------------")
                print("Map click")
                print(paste("Row =",row(),"Region =",isolate.copy@data$NAME_1[row()]))
                print(paste("First =",isolate.copy@data[row(),obs.columns$first],"Second =",isolate.copy@data[row(),obs.columns$second]))
                print("-----------------------------------------------------")
               
                region = isolate.copy[which(isolate.copy@data$GID_1 %in% click.list()), ]
                proxy %>% removeShape(layerId = region@data$GID_1) %>%
                    addPolygons(
                        data = region,
                        weight = 1,
                        fillOpacity = 0.25,
                        color = p(region@data[,render.col()]),
                        layerId = region@data$GID_1,
                        highlight = highlightOptions(
                            weight = 5,
                            color = p(region@data[,render.col()]),
                            fillOpacity = 0.7,
                            bringToFront = TRUE
                        )
                    )

                click.list(ev$id)  # we only store the last click now!

                updateNumericInput(session = session,inputId = "first.cases",
                                   label = paste("Значение(1 момент)"),
                                   min = -.Machine$integer.max,
                                   max = .Machine$integer.max,
                                   value = first.time[row()],step = 0.1
                )

                updateNumericInput(session = session,inputId = "second.cases",
                                   label = paste("Значение(2 момент)"),
                                   min = -.Machine$integer.max,
                                   max = .Machine$integer.max,
                                   value = second.time[row()],step = 0.1
                )
                region = isolate.copy[row(), ]
                proxy %>% addPolylines(
                    data = region,
                    layerId = region@data$GID_1,
                    color = p(region@data[,render.col()]),
                    weight = 5,
                    opacity = 1
                )
            })

#---------------------------------------------------------------------------------
            
            observeEvent({input$alpha;monte.carlo$params},{
                req(!is.null(monte.carlo$params))
                p.down = isolate(monte.carlo$params)[1,"Probabilities"]
                p.up = isolate(monte.carlo$params)[2,"Probabilities"]
                if(p.down > 0 && p.down < 1)
                {
                    sim.down = as.data.frame(isolate(monte.carlo$sim.down))

                    a1 = input$alpha / 2
                    a2 = input$alpha / 2
                    
                    sim.down$distToCrit = abs(sim.down$PMaxNGreaterNi - a2)
                    ind = which.min(sim.down$distToCrit)
                    a2 = sim.down$PMaxNGreaterNi[ind]
                    a1 = input$alpha - a2
                    n.crit = sim.down$Size[ind]
                    print(paste("Alpha = ",a1,"+",a2,";","N.crit =",n.crit))
                    a.uni = a1 / (n.crit - 1)
                    f.cr = function(s.cr,size,e.n){return(pchisq(s.cr,df = 2 * size) - (1 - a.uni / e.n))}
                    tol = 0.0001
                    
                    for (j in 1:nrow(sim.down)) {
                        if(sim.down[j,"Size"] <= n.crit){
                            sim.down[j,"S.crit"] = rootSolve::uniroot.all(f.cr,
                                                                          c(qchisq(tol,2*sim.down[j,"Size"]), qchisq(1-tol,2*sim.down[j,"Size"])),
                                                                          tol = tol,
                                                                          size = sim.down[j,"Size"],
                                                                          e.n = sim.down[j,"ExpectedNn"])[1]
                        }
                        else{
                            sim.down[j,"S.crit"] = 0
                        }
                    }
                    monte.carlo$sim.down = rbind.data.frame(sim.down,data.frame(Size = n.crit,
                                                                    SumProbabilities = 0,
                                                                    MeanProbabilities = 0,
                                                                    ExpectedNn = 0,
                                                                    PMaxNGreaterNi = 0,
                                                                    distToCrit = 0,
                                                                    S.crit = 0))
                    
                    
                }
                
                if(p.up > 0 && p.up < 1){
                    sim.up = as.data.frame(isolate(monte.carlo$sim.up))

                    a1 = input$alpha/ 2
                    a2 = input$alpha / 2
                    sim.up$distToCrit = abs(sim.up$PMaxNGreaterNi - a2)
                    ind = which.min(sim.up$distToCrit)
                    a2 = sim.up$PMaxNGreaterNi[ind]
                    a1 = input$alpha - a2
                    n.crit = sim.up$Size[ind]
                    print(paste("Alpha = ",a1,"+",a2,";","N.crit =",n.crit))
                    a.uni = a1 / (n.crit - 1)
                    f.cr = function(s.cr,size,e.n){return(pchisq(s.cr,df = 2 * size) - (1 - a.uni / e.n))}
                    tol = 0.0001
                    
                    for (j in 1:nrow(sim.up)) {
                        if(sim.up[j,"Size"] <= n.crit){
                            sim.up[j,"S.crit"] = rootSolve::uniroot.all(f.cr,
                                                                        c(qchisq(tol,2*sim.up[j,"Size"]), qchisq(1-tol,2*sim.up[j,"Size"])),
                                                                        tol = tol,
                                                                        size = sim.up[j,"Size"],
                                                                        e.n = sim.up[j,"ExpectedNn"])[1]
                        }
                        else{
                            sim.up[j,"S.crit"] = 0
                        }
                    }
                    monte.carlo$sim.up = rbind.data.frame(sim.up,data.frame(Size = n.crit,
                                                                SumProbabilities = 0,
                                                                MeanProbabilities = 0,
                                                                ExpectedNn = 0,
                                                                PMaxNGreaterNi = 0,
                                                                distToCrit = 0,
                                                                S.crit = 0))
                }
            })
            
            
            observeEvent(input$do.clust,
            {
                print("-----------------------------------------------------")
                print("Clustering checks")
                print("Identical names?")
                print(identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(monte.carlo$adj.mat)))))
                print("-----------------------------------------------------")
                if(!identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(monte.carlo$adj.mat))))){
                    shinyalert("Error","Результат симуляции НЕ для этой карты", type = "error")
                    return()
                }
                #0 - no discharges/clusters
                #1 - has discharges/clusters but they are not significant
                #2 - has significant discharges/clusters
                has.discharges = 0
                has.clusters = 0
                p.down = isolate(monte.carlo$params)[1,"Probabilities"]
                p.up = isolate(monte.carlo$params)[2,"Probabilities"]
                sim.up = as.data.frame(isolate(monte.carlo$sim.up))
                sim.down = as.data.frame(isolate(monte.carlo$sim.down))
                adj.mat = as.matrix(isolate(monte.carlo$adj.mat))
                print("-----------------------------------------------------")
                print(paste("Probability limits = [",p.down,";",p.up,"]"))
                if(p.down > 0 && p.down < 1)
                {
                    print("Search clusters")

                    s.n.down = function(x, p, data){
                        return(-2 * sum(log(data@data[which(data@data$GID_1 %in% x),render.col()] / p)))
                    }

                    buf = isolate(copy.map())
                    buf@data$CRIT = ifelse(buf@data[,render.col()]<=p.down,1,0)
                    select = which(buf@data$CRIT == 1,arr.ind = T)
                    if(length(select) != 0){
                        graph <- graph.adjacency(adj.mat[select,select])
                        comps <- components(graph)
                        clusters = igraph::groups(comps)
                        print(clusters)
                        if(length(clusters) > 0){
                            has.clusters = 1
                            s = sapply(clusters,s.n.down,p = p.down, data = buf)
                            sizes = sapply(clusters, length)
                            cls = as.data.frame(table(sizes))
                            data.clusters = data.frame(ID = names(s),N = sizes,S = s)
                            #names(clusters) = data.clusters$ID

                            for (j in 1:nrow(data.clusters)) {
                                nearest = which.min(abs(sim.down$Size - data.clusters$N[j]))
                                data.clusters$S.crit[j] = sim.down[nearest,"S.crit"]
                                data.clusters$Label[j] = paste(clusters[[as.character(data.clusters$ID[j])]],collapse = "; ")
                                data.clusters$Names[j] = paste(buf@data$NAME_1[which(buf@data$GID_1 %in% clusters[[as.character(data.clusters$ID[j])]])],collapse = "; ")
                            }
                            result$clusters = data.clusters
                            print("Data clusters")
                            print(data.clusters)
                            n.crit = sim.down$Size[which.min(sim.down$distToCrit)]
                            signif.clust = data.clusters[data.clusters$N >= n.crit | data.clusters$S >= data.clusters$S.crit,]
                            signif.result$clusters = signif.clust
                            print("Significant clusters")
                            print(signif.clust)
                            if(nrow(signif.clust) > 0){
                                has.clusters = 2
                                #colours = cluster.color(nrow(signif.clust))
                                for (i in signif.clust$ID) {
                                    region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                    #print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                    union.clust = surveillance::unionSpatialPolygons(region)
                                    proxy %>%
                                        addPolylines(
                                                data = union.clust,
                                                layerId = paste0("clusters",i),
                                                color = "blue",
                                                weight = 5,
                                                opacity = 1
                                            )
                                    }
                                }
                            }
                    }
                    print("-----------------------------------------------------")
                    }

                if(p.up > 0 && p.up < 1)
                {
                        print("Search discharges")
                        

                        s.n.up = function(x, p, data){
                            return(-2 * sum((log((1 - data@data[which(data@data$GID_1 %in% x),render.col()]) / (1 - p)))))
                        }

                        buf = isolate(copy.map())
                        buf@data$CRIT = ifelse(buf@data[,render.col()]>=p.up,1,0)
                        select = which(buf@data$CRIT == 1,arr.ind = T)
                        if(length(select) != 0){
                            graph <- graph.adjacency(adj.mat[select,select])
                            comps <- components(graph)
                            clusters = igraph::groups(comps)
                            print(clusters)
                            if(length(clusters) > 0){
                                has.discharges = 1
                                s = sapply(clusters,s.n.up,p = p.up, data = buf)
                                sizes = sapply(clusters, length)
                                cls = as.data.frame(table(sizes))
                                data.discharges = data.frame(ID = names(s),N = sizes,S = s)
                                for (j in 1:nrow(data.discharges)) {
                                    nearest = which.min(abs(sim.up$Size - data.discharges$N[j]))
                                    data.discharges$S.crit[j] = sim.up[nearest,"S.crit"]
                                    data.discharges$Label[j] = paste(clusters[[as.character(data.discharges$ID[j])]],collapse = "; ")
                                    data.discharges$Names[j] = paste(buf@data$NAME_1[which(buf@data$GID_1 %in% clusters[[as.character(data.discharges$ID[j])]])],collapse = "; ")

                                }
                                result$discharges = data.discharges
                                print("Data discharges")
                                print(data.discharges)
                                n.crit = sim.up$Size[which.min(sim.up$distToCrit)]
                                signif.dis = data.discharges[data.discharges$N >= n.crit | data.discharges$S >= data.discharges$S.crit,]
                                signif.result$discharges = signif.dis
                                print("Significant discharges")
                                print(signif.dis)
                                if(nrow(signif.dis) > 0){
                                    has.discharges = 2
                                    #colours = cluster.color(nrow(signif.dis))
                                    for (i in signif.dis$ID) {
                                        region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                        #print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                        union = surveillance::unionSpatialPolygons(region)
                                        proxy %>%
                                            addPolylines(
                                                data = union,
                                                layerId = paste0("clusters",i),
                                                color = "black",
                                                weight = 5,
                                                opacity = 1
                                            )
                                    }
                                }

                                print("-----------------------------------------------------")
                            }
                        }
                    }
                    
                message = ""
                if(has.discharges == 0 && has.clusters == 0){
                    message = "Разряжения и кластеры не найдены."
                }
                else if(has.discharges == 1 || has.clusters == 1){
                    insertion = ifelse(has.discharges == 1 && has.clusters == 1, "Разряжения и кластеры",
                                       ifelse(has.discharges == 1, "Разряжения", "Кластеры"))
                    message = paste(insertion,"найдены, но статистически не значимы")
                }
                if(has.discharges != 2 && has.clusters != 2){
                    shinyalert("Result",
                                   message,
                                   type = "info"
                    )
                }
                enable("save_res")
            })

#---------------------------------------------------------------------------------            
            observeEvent({input$first.cases
                        input$second.cases},ignoreInit = T,ignoreNULL = T,
                         {

                             req(copy.map(),input$first,input$second)
                             buf = isolate(copy.map())
                             req(is.numeric(buf@data[,obs.columns$first]))
                             req(is.numeric(buf@data[,obs.columns$second]))

                             buf@data[row(),obs.columns$first] = input$first.cases
                             buf@data[row(),obs.columns$second] = input$second.cases
                             copy.map(buf)
                             calc.first.moment()
                             calc.second.moment()
                             calc.dif()

                             print("-----------------------------------------------------")
                             print("Change cases")
                             print(paste("Row =",row(),"Region =",buf@data$NAME_1[row()]))
                             print(paste("First =",buf@data[row(),obs.columns$first]))
                             print(paste("Second =",buf@data[row(),obs.columns$second]))
                             print("-----------------------------------------------------")

                            
                             region = copy.map()[row(),]
                             proxy %>% removeShape(layerId = region@data$GID_1) %>%
                                 addPolygons(
                                     data = region,
                                     weight = 1,
                                     fillOpacity = 0.25,
                                     color = p(region@data[,render.col()]),
                                     layerId = region@data$GID_1,
                                     highlight = highlightOptions(
                                         weight = 5,
                                         color = p(region@data[,render.col()]),
                                         fillOpacity = 0.7,
                                         bringToFront = TRUE
                                     )
                                 )%>% addPolylines(
                                     data = region,
                                     layerId = region@data$GID_1,
                                     color = p(region@data[,render.col()]),
                                     weight = 5,
                                     opacity = 1
                                 )

           })

            # 
            # #TODO
            observeEvent(input$clearHighlight, ignoreInit = T,
            {
                row(0)
                click.list(NULL)
                copy.map(map.data())
                #Final results
                result$discharges = NULL
                result$clusters = NULL
                
                #Only for statistically significant results
                signif.result$discharges = NULL
                signif.result$clusters = NULL
                
                
                calc.dif()
                calc.first.moment() 
                calc.second.moment()
                tmp = render.col()
                render.col(-1)
                render.col(tmp)

            })

            # observeEvent(input$save_res,ignoreInit = T,{
            #     if (is.integer(input$save_res)) {
            #         cat("No file has been selected (shinyResSave)\n")
            #     }
            #     else {
            #         tryCatch({
            #                     path = parseDirPath(volumes, input$save_res)
            #                     rgdal::writeOGR(copy.map(),path, "map", driver="ESRI Shapefile",encoding = "UTF-8")
            #                     tbl.path = file.path(path, paste0("result_","alpha=",input$alpha,".xlsx"))
            #                     if(!is.null(result$discharges)){
            #                         write.xlsx(result$discharges,file = tbl.path,sheetName = "Discharges")
            #                     }
            #                     if(!is.null(result$clusters)){
            #                         write.xlsx(result$clusters,file = tbl.path,sheetName = "Clusters",append = T)
            #                     }
            #                     if(!is.null(monte.carlo$sim.down)){
            #                         write.xlsx(monte.carlo$sim.down,file = tbl.path,sheetName = "Crit_Down",append = T)
            #                     }
            #                     if(!is.null(monte.carlo$sim.up)){
            #                         write.xlsx(monte.carlo$sim.up,file = tbl.path,sheetName = "Crit_Up",append = T)
            #                     }
            #                     if(!is.null(monte.carlo$adj.mat)){
            #                         write.xlsx(monte.carlo$adj.mat,file = tbl.path,sheetName = "Adj_Mat",append = T)
            #                     }
            #                     if(!is.null(monte.carlo$params)){
            #                         write.xlsx(monte.carlo$params,file = tbl.path,sheetName = "Params",append = T)
            #                     }
            #         },error = function(e){
            #             shinyalert("Error",
            #                        paste("Не могу сохранить результат, потому что ",e),
            #                        type = "error"
            #             )
            #         })
            # 
            #     }
            # })
        }
    )
}