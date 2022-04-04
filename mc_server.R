library(shiny)
library(rgdal)
library(shinyjs)
library(ggplot2)
library(shinyFiles)
library(igraph)
library(shinyalert)
library(surveillance)
library(xlsx)

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
            
            volumes = c(Home = "~")
            
            sim.data = reactiveValues(
                path = NA,
                data = NULL
            )
            
            shinyDirChoose(
                input,
                'ogr_dir',
                roots = volumes,
                allowDirCreate = FALSE,
                session = session
            )
            shinyFileSave(
                input,
                'save_sim',
                roots = volumes,
                allowDirCreate = T,
                session = session
            )

            observe({
                if (is.integer(input$ogr_dir)) {
                  cat("No map directory has been selected\n")
                } 
                else {
                  sim.data$path = parseDirPath(volumes, input$ogr_dir)
                  enable("start.sim")
                  cat(paste0(sim.data$path,"\n"))
                  }
             })

            observeEvent(input$start.sim, {

                layers = strsplit(sim.data$path, "/")[[1]]
                layer = layers[length(layers)]
                success.parse = F
                withProgress(message = "Read the data", {
                    incProgress(0.5)
                    sim.data$data = tryCatch(expr = {
                        polygons = readOGR(dsn = sim.data$path, layer = as.character(layer))   
                        success.parse = T
                        polygons
                    },
                    error = function(e){
                        shinyalert("Error", 
                               paste("Cannot parse the data, because",e),
                               type = "error"
                        )
                        NULL
                    })
                    incProgress(0.5)
                })
                if(success.parse){
                    n.iter = isolate(input$number.iteration)
                    p.down = isolate(input$mark.limits[1])
                    p.up = isolate(input$mark.limits[2])
                    queen = isolate(input$queen)
                    
                   
                    if ((p.up == 0 || p.up == 1) && (p.down == 1 || p.down == 0))
                    {
                        shinyalert("Warning", 
                                   "Choose correct thresholds!",
                                   type = "warning"
                        )
                    }
                    else{
                        result$params[1,"Probabilities"] = p.down
                        result$params[2,"Probabilities"] = p.up
                        result$params$Queen = queen
                        result$params$numIter = n.iter
                    
                          
                        disable("start.sim")
                        disable("ogr_dir")
                        cat("Start simulation\n")
                        
                        withProgress(message = "Simulation", {
                            histogram = data.frame(Size = 0,SumProbabilities = 0, MeanProbabilities = 0,ExpectedNn = 0,PMaxNGreaterNi = 0)
                            max.clust.size = data.frame()
                            incProgress(0.2,message = "Build adjacency matrix")
                            print(sim.data$data)
                            adjacencyMatrix = poly2adjmat(sim.data$data, queen = queen, zero.policy = T,row.names = sim.data$data@data$GID_1)
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
                                    message = paste("Down simulation. Iteration", i, "from", n.iter)
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
                                        message = paste("Up simulation. Iteration", i, "from", n.iter)
                                    )
                                }
                                #PDF of cluster size
                                histogram$MeanProbabilities = histogram$SumProbabilities / n.iter
                                histogram$ExpectedNn = histogram$ExpectedNn / n.iter
                                histogram$PMaxNGreaterNi = histogram$PMaxNGreaterNi / n.iter
                                histogram = histogram[order(histogram$Size), ]
                                result$sim.up = histogram[-1,]
                            }
                            
                            enable("start.sim")
                            enable("save_sim")
                            enable("ogr_dir")
                        })
                }
            }
            else{
                    shinyalert("Warning",
                               "Cannot parse the data. It is NULL.",
                               type = 'warning')
                print(sim.data$data)
            }
        })

                observe({
                    if (is.integer(input$save_sim)) {
                      cat("No file has been selected (shinySaveChoose)\n")
                    } 
                    else {
                        info = parseSavePath(volumes, input$save_sim)
                        write.xlsx(result$params,file = as.character(info$datapath),sheetName = "Params",row.names = F)
                        write.xlsx(result$adj.mat,as.character(info$datapath),sheetName = "Adj.Mat",append = T,row.names = T,col.names = T)
                        write.xlsx(result$sim.down,as.character(info$datapath),sheetName = "Down",append = T,row.names = F)
                        write.xlsx(result$sim.up,as.character(info$datapath),sheetName = "Up",append = T,row.names = F)
                    }
              })
              
              output$down.PDF = renderPlot({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.down,"Uncorrect threshold for discharging simulation"))
                  ggplot(result$sim.down) + geom_col(aes(x = Size, y = MeanProbabilities), alpha = 0.85) +
                      labs(x = "Size of cluster(number of regions)", y = "Probability", title = "Clusters size distribution")
              })
              
              output$up.PDF = renderPlot({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.up,"Uncorrect threshold for clustering simulation"))
                  ggplot(result$sim.up) + geom_col(aes(x = Size, y = MeanProbabilities), alpha = 0.85) +
                      labs(x = "Size of cluster(number of regions)", y = "Probability", title = "Clusters size distribution")
              })


              output$down.table = renderTable({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.down,"Uncorrect threshold for discharging simulation"))
                  result$sim.up
              },digits = 4)
              
              output$up.table = renderTable({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.up,"Uncorrect threshold for clustering simulation"))
                  result$sim.up
              },digits = 4)
              
              output$down.alpha2 = renderPlot({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.down,"Uncorrect threshold for discharging simulation"))
                  ggplot(result$sim.down) + geom_col(aes(x = Size, y = PMaxNGreaterNi), alpha = 0.85) +
                      labs(x = "Maximal size of cluster(number of regions)", y = "Probability", title = "Maximal clusters size distribution")
              })
              
              output$up.alpha2 = renderPlot({
                  validate(need(!is.null(sim.data$data),label = "Data"))
                  validate(need(result$sim.up,message = "Uncorrect threshold for clustering simulation"))
                  ggplot(result$sim.up) + geom_col(aes(x = Size, y = PMaxNGreaterNi), alpha = 0.85) +
                      labs(x = "Maximal size of cluster(number of regions)", y = "Probability", title = "Maximal clusters size distribution")
              })
        }
    )
}