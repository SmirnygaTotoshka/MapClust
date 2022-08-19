library(leaflet)
library(xlsx)
library(ggplot2)
library(purrr)
library(plotly)
library(igraph)
library(surveillance)
Main.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            row = reactiveVal(0)
            render.col = reactiveVal(0)
            
            mc.params = reactiveVal(NULL)
            mc.adj.mat = reactiveVal(NULL)
            mc.sim.down = reactiveVal(NULL)
            mc.sim.up = reactiveVal(NULL)
            
            res.dis = reactiveVal(NULL)
            res.clust = reactiveVal(NULL)
            
            cols = reactiveValues(
                first = 1, 
                second = 1
            )
           
            click.list <- reactiveVal(NULL)
            proxy = leafletProxy("map")

            map.data = reactiveVal(NULL)
            copy.map = reactiveVal(NULL)
                # 
                # output$down_crit_reg = renderPlot(
                # {
                #    validate(FALSE,"Запустите критерий")
                # })
                # 
                # output$up_crit_reg = renderPlot(
                # {
                #    validate(FALSE,"Запустите критерий")
                # })
                # 
                # output$down_stat = renderTable({
                #     validate(FALSE,"Запустите критерий")
                # })
                # 
                # output$up_stat = renderTable({
                #     validate(FALSE,"Запустите критерий")
                # })
            
                volumes = c(Home = "~")
                
                shinyDirChoose(
                    input,
                    'ogr_dir',
                    roots = volumes,
                    allowDirCreate = FALSE,
                    session = session
                )
                shinyFileSave(
                    input,
                    'save_res',
                    roots = volumes,
                    allowDirCreate = T,
                    session = session
                )
            
            search.numeric.columns = reactive({
                cols$first = 1
                cols$second = 1
                first.finded = F
                for (col in 1:ncol(copy.map()@data)) {
                    if(is.numeric(copy.map()@data[,col])){
                        if(first.finded){
                            cols$second = col
                            break
                        }
                        else{
                            cols$first = col
                            first.finded = T
                        }
                    } 
                }
            })
            
            observeEvent(input$ogr_dir,{
                if (is.integer(input$ogr_dir)) {
                    cat("No map\n")
                }
                else {
                    tryCatch({
                        path = parseDirPath(volumes, input$ogr_dir)
                        print(paste("Path to map","=",path))
                        layers = strsplit(path, "/")[[1]]
                        layer = layers[length(layers)]
                        print(paste("Layer","=",layer))
                        map.df = rgdal::readOGR(dsn = path, layer = as.character(layer))
                        map.df1 = sf::st_shift_longitude(sf::st_as_sf(map.df))
                        map.df = as(map.df1, "Spatial")
                        map.data(map.df)
                        copy.map(map.df)

                        search.numeric.columns()
                        print(c(cols$first,cols$second))
                        updateSelectInput(session = session,inputId = "first",label = "Выберите первый момент", choices = colnames(copy.map()@data),selected = colnames(copy.map()@data)[cols$first])
                        updateSelectInput(session = session,inputId = "second",label = "Выберите второй момент", choices = colnames(copy.map()@data),selected = colnames(copy.map()@data)[cols$second])
                        
                    },
                    error = function(e){
                        shinyalert("Error",
                                   paste("Не могу считать данные, потому что ",e),
                                   type = "error"
                        )
                    })

                }
            })
            
            observe({
                toggleState("clearHighlight", row() != 0 || (!is.null(res.dis()) || !is.null(res.clust())))
                toggleState("save_res",!is.null(res.dis()) || !is.null(res.clust()))
                toggleState("do.clust", !is.null(map.data()) && !is.null(mc.params()))
            })
            
            
            observeEvent({input$first
                  input$second },
                  ignoreNULL = T,
                  ignoreInit = T,{
                      print("-----------------------------------------------------")
                      print("Select columns")
                      req(copy.map(),input$first,input$second)
                      data = copy.map()
                      cols$first = input$first
                      cols$second = input$second
                      print(paste("First",input$first,"Second",input$second))
                      
                      req(is.numeric(copy.map()@data[,cols$first]))
                      req(is.numeric(copy.map()@data[,cols$second]))
                      first.time = as.numeric(copy.map()@data[,cols$first])
                      second.time = as.numeric(copy.map()@data[,cols$second])
                      
                      print("First time")
                      print(first.time)
                      print("Second time")
                      print(second.time)
                      
                      data@data$X = (first.time - second.time) / (sqrt(first.time + second.time))
                      data@data$P.VALUE = pnorm(data@data$X,lower.tail = F)
                      
                      print("X")
                      print(data@data$X)
                      print("P-value")
                      print(data@data$P.VALUE)
                      
                      m1 = mean(first.time,na.rm = T)
                      data@data$X1 = (first.time - m1) / sqrt(m1)
                      data@data$P.VALUE1 = pnorm(data@data$X1,lower.tail = F)
                      print("Mean 1")
                      print(m1)
                      print("X 1")
                      print(data@data$X1)
                      print("P-value 1")
                      print(data@data$P.VALUE1)
                      
                      m2 = mean(second.time,na.rm = T)
                      data@data$X2 = (second.time - m2) / sqrt(m2)
                      data@data$P.VALUE2 = pnorm(data@data$X2,lower.tail = F)
                      print("Mean 2")
                      print(m2)
                      print("X 2")
                      print(data@data$X2)
                      print("P-value 2")
                      print(data@data$P.VALUE2)
                      print("-----------------------------------------------------")
                      copy.map(data)
                      render.col(match("P.VALUE", colnames(copy.map()@data))[1])
                      
                      updateNumericInput(session = session,inputId = "first.cases",
                                         label = paste("Значение(1 момент)"),
                                         min = min(first.time,na.rm = T) - 10 * (max(first.time,na.rm = T) - min(first.time,na.rm = T)),
                                         max = max(first.time,na.rm = T) + 10 * (max(first.time,na.rm = T) - min(first.time,na.rm = T)),
                                         value = first.time[row()],step = 0.1
                      )

                      updateNumericInput(session = session,inputId = "second.cases",
                                         label = paste("Значение(2 момент)"),
                                         min = min(second.time,na.rm = T) - 10 * (max(second.time,na.rm = T) - min(second.time,na.rm = T)),
                                         max = max(second.time,na.rm = T) + 10 * (max(second.time,na.rm = T) - min(second.time,na.rm = T)),
                                         value = second.time[row()],step = 0.1
                      )
                      # withProgress(message = "Rendering...", {
                      #     proxy %>% clearShapes()  %>%
                      #         clearControls() %>% addProviderTiles("CartoDB.Positron") %>%
                      #         addPolygons(
                      #             data = copy.map(),
                      #             weight = 1,
                      #             fillOpacity = 0.25,
                      #             color = p(copy.map()@data[,render.col()]),
                      #             layerId = copy.map()@data$GID_1,
                      #             highlight = highlightOptions(
                      #                 weight = 5,
                      #                 color = p(copy.map()@data[,render.col()]),
                      #                 fillOpacity = 0.7,
                      #                 bringToFront = TRUE
                      #             )
                      #         ) %>%
                      #         addLegend(pal = p,
                      #                   title = "P-Value",
                      #                   values = copy.map()@data[,render.col()])
                      #     print("Render map! - select columns")
                      # 
                      # })
                  })
            
            observeEvent(input$representation,{
                req(copy.map())
                if(input$representation == "Difference"){
                    render.col(match("P.VALUE", colnames(copy.map()@data))[1])
                }
                else if(input$representation == "First"){
                    render.col(match("P.VALUE1", colnames(copy.map()@data))[1])
                }
                else{
                    render.col(match("P.VALUE2", colnames(copy.map()@data))[1])
                }
            })
            
            output$map <- renderLeaflet({
                print(paste("Mode",render.col(),class(render.col())))
                validate(need(copy.map(),message = "Загрузите карту"))
                  validate(need(copy.map()@data$P.VALUE,message = "Выберите моменты времени")) 
                withProgress(message = "Rendering...", {
                    print("Render map! - renderer")
                  leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
                    addPolygons(
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
                              title = "P-Value",
                              values = copy.map()@data[,render.col()])
                })
              })
              
            observeEvent(input$mc_res,{
                  tryCatch({
                      mc.params(read.xlsx(input$mc_res$datapath,sheetName = "Params"))
                      mc.adj.mat(read.xlsx(input$mc_res$datapath,sheetName = "Adj.Mat",row.names = T,col.names = T))
                      mc.sim.down(read.xlsx(input$mc_res$datapath,sheetName = "Down"))
                      mc.sim.up(read.xlsx(input$mc_res$datapath,sheetName = "Up"))
                      
                      # mc.params(read.xlsx("~/Romania.xlsx",sheetName = "Params"))
                      # mc.adj.mat(read.xlsx("~/Romania.xlsx",sheetName = "Adj.Mat"))
                      # mc.sim.down(read.xlsx("~/Romania.xlsx",sheetName = "Down"))
                      # mc.sim.up(read.xlsx("~/Romania.xlsx",sheetName = "Up"))
                  },
                  error = function(e){
                      shinyalert("Error", 
                                 paste("Не могу считать данные, потому что ",e),
                                 type = "error"
                      )    
                      mc.params = reactiveVal(NULL)
                      mc.adj.mat = reactiveVal(NULL)
                      mc.sim.down = reactiveVal(NULL)
                      mc.sim.up = reactiveVal(NULL)
                  })
              })
              

              
        
            observeEvent(input$map_shape_click,
                             ignoreNULL = T,
                             ignoreInit = T,
            {
                isolate.copy = isolate(copy.map())
                first.time = as.numeric(isolate.copy@data[,cols$first])
                second.time = as.numeric(isolate.copy@data[,cols$second])
                
                i = which(isolate.copy@data$GID_1 %in% input$map_shape_click)
                if(is.na(first.time[i]) || is.na(second.time[i]) || 
                   is.nan(first.time[i]) || is.nan(second.time[i]) ||
                   is.infinite(first.time[i]) || is.infinite(second.time[i])){
                    req(input$changeableNA)
                }
                req(copy.map()@data[,render.col()])
                
                ev <- input$map_shape_click
                
                row(which(isolate.copy@data$GID_1 %in% ev$id))
                
                # m1 = round(mean(first.time,na.rm = T),2)
                # sd1 = round(sd(first.time,na.rm = T),2)
                # m2 = round(mean(second.time,na.rm = T),2)
                # sd2 = round(sd(second.time,na.rm = T),2)
                
                print("-----------------------------------------------------")
                print("Map click")
                print(paste("Row =",row(),"Region =",isolate.copy@data$NAME_1[row()]))
                print(paste("First =",isolate.copy@data[row(),cols$first],"Second =",isolate.copy@data[row(),cols$second]))
                # print(paste("m1 =",m1,"sd1 =",sd1))
                # print(paste("m2 =",m2,"sd2 =",sd2))
                print("-----------------------------------------------------")
                # If already selected, first remove previous selection
                if (!is.null(click.list())) {
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
                     print("Render map!-map click1")
               }
               else{
                   show("first.cases")
                   show("second.cases")
                }
                click.list(ev$id)  # we only store the last click now!
               # updateSelectInput(session = session,inputId = "map", selected = ev$id)
                updateNumericInput(session = session,inputId = "first.cases",
                                  label = paste("Значение(1 момент)"),
                                  min = min(first.time,na.rm = T) - 10 * (max(first.time,na.rm = T) - min(first.time,na.rm = T)),
                                  max = max(first.time,na.rm = T) + 10 * (max(first.time,na.rm = T) - min(first.time,na.rm = T)),
                                  value = first.time[row()],step = 0.1
                )
                
                updateNumericInput(session = session,inputId = "second.cases",
                                  label = paste("Значение(2 момент)"),
                                  min = min(second.time,na.rm = T) - 10 * (max(second.time,na.rm = T) - min(second.time,na.rm = T)),
                                  max = max(second.time,na.rm = T) + 10 * (max(second.time,na.rm = T) - min(second.time,na.rm = T)),
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
               print("Render map! - map.click2")

            })
            
            output$dif.print = renderText({
                req(row() > 0)
                paste(
                    "P-value для",
                    copy.map()@data$NAME_0[row()],
                    copy.map()@data$NAME_1[row()],
                    copy.map()@data$GID_1[row()],
                    "=",
                    copy.map()@data$P.VALUE[row()]
                )
            })
            output$first.pvalue = renderText({
                req(row() > 0)
                paste(
                    "P-value(1 момент)",
                    "=",
                    copy.map()@data$P.VALUE1[row()]
                )    
                
            })
            output$second.pvalue = renderText({
                req(row() > 0)
                paste(
                    "P-value(2 момент)",
                    "=",
                    copy.map()@data$P.VALUE2[row()]
                )    
            })
              
            observeEvent(input$do.clust,
            {
                print("-----------------------------------------------------")
                print("Clustering checks")
                print("Identical names?")
                print(identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(mc.adj.mat())))))
                print("GID_1")
                print(sort(as.vector(copy.map()@data$GID_1)))
                print("Adj.Mat")
                print(sort(as.vector(row.names(mc.adj.mat()))))
                print("-----------------------------------------------------")
                if(!identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(mc.adj.mat()))))){
                    shinyalert("Error","Результат симуляции НЕ для этой карты", type = "error")
                    return()
                }
                #0 - no discharges/clusters
                #1 - has discharges/clusters but they are not significant
                #2 - has significant discharges/clusters
                has.discharges = 0
                has.clusters = 0
                p.down = isolate(mc.params())[1,"Probabilities"]
                p.up = isolate(mc.params())[2,"Probabilities"]
                print("-----------------------------------------------------")
                print(paste("Probability limits = [",p.down,";",p.up,"]"))
                if(p.down > 0 && p.down < 1)
                {
                    print("Search clusters")
                    sim.down = as.data.frame(isolate(mc.sim.down()))
                    adj.mat = as.matrix(isolate(mc.adj.mat()))
                    a = input$alpha
                   # a=0.05
                    a1 = a / 2
                    a2 = a / 2
                    sim.down$distToCrit = abs(sim.down$PMaxNGreaterNi - a2)
                    ind = which.min(sim.down$distToCrit)
                    a2 = sim.down$PMaxNGreaterNi[ind]
                    a1 = a - a2
                    n.crit = sim.down$Size[ind]
                    print(paste("Alpha = ",a1,"+",a2,";","N.crit =",n.crit))
                    a.uni = a1 / (n.crit - 1)
                    f.cr = function(s.cr,size,e.n){return(pchisq(s.cr,df = 2 * size) - (1 - a.uni / e.n))}
                    tol = 0.0001
    
                    for (j in 1:nrow(sim.down)) {
                         if(sim.down[j,"Size"] <= n.crit){
                             sim.down[j,"s.cr"] = rootSolve::uniroot.all(f.cr,
                                                                    c(qchisq(tol,2*sim.down[j,"Size"]), qchisq(1-tol,2*sim.down[j,"Size"])),
                                                                    tol = tol,
                                                                    size = sim.down[j,"Size"],
                                                                    e.n = sim.down[j,"ExpectedNn"])[1]
                         }
                         else{
                             sim.down[j,"s.cr"] = 0
                         }
                    }
                    sim.down = rbind.data.frame(sim.down,data.frame(Size = n.crit,
                                                                    SumProbabilities = 0,
                                                                    MeanProbabilities = 0,
                                                                    ExpectedNn = 0,
                                                                    PMaxNGreaterNi = 0,
                                                                    distToCrit = 0,
                                                                    s.cr = 0))
    
                    s.n.down = function(x, p, data){
                        print(data@data[which(data@data$GID_1 %in% x),render.col()])
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
                                data.clusters$S.CR[j] = sim.down[nearest,"s.cr"]
                                data.clusters$Label[j] = paste(clusters[[as.character(data.clusters$ID[j])]],collapse = "; ")
                                data.clusters$Names[j] = paste(buf@data$NAME_1[which(buf@data$GID_1 %in% clusters[[as.character(data.clusters$ID[j])]])],collapse = "; ")
                            }
                            print("Data clusters")
                            print(data.clusters)
                            signif.clust = data.clusters[data.clusters$N >= n.crit | data.clusters$S >= data.clusters$S.CR,]
                            print("Significant clusters")
                            print(signif.clust)
                            if(nrow(signif.clust) > 0){
                                has.clusters = 2
                                #colours = cluster.color(nrow(signif.clust))
                                for (i in signif.clust$ID) {
                                    region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                    print("Region data")
                                    #print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                    print(region@data)
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
                            res.clust(data.clusters)
                            mc.sim.down(sim.down)
                            output$down_crit_reg = renderPlotly(
                            {
                                validate(need(sim.down$s.cr,"Постройте критическую область"))
                                sim.down = sim.down[order(sim.down$Size),]
                                fig = plot_ly(sim.down, x = ~Size, y = ~s.cr,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
                                        add_trace(data = data.clusters,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
                                    add_trace(data = signif.clust,x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label)%>%
                                    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Кластеры")
                                fig
                                    # ggplot(sim.down) + geom_line(aes(x = Size, y = s.cr)) +
                                    #     geom_point(data = data.discharges,mapping = aes(x = N, y = S),color = "green",size = 3) +
                                    #     geom_point(data = signif.dis,mapping = aes(x = N, y = S),color = "blue",size = 3) + theme_bw()
    
                            })
                            output$down_stat = renderTable({
                                data.clusters
                                },digits = 4)
                            }
                    }
                    print("-----------------------------------------------------")
                    }

                    if(p.up > 0 && p.up < 1)
                    {
                        print("Search discharges")
                        sim.up = as.data.frame(isolate(mc.sim.up()))
                        adj.mat = as.matrix(isolate(mc.adj.mat()))
                        a = input$alpha
                        # a=0.05
                        a1 = a / 2
                        a2 = a / 2
                        sim.up$distToCrit = abs(sim.up$PMaxNGreaterNi - a2)
                        ind = which.min(sim.up$distToCrit)
                        a2 = sim.up$PMaxNGreaterNi[ind]
                        a1 = a - a2
                        n.crit = sim.up$Size[ind]
                        print(paste("Alpha = ",a1,"+",a2,";","N.crit =",n.crit))
                        a.uni = a1 / (n.crit - 1)
                        f.cr = function(s.cr,size,e.n){return(pchisq(s.cr,df = 2 * size) - (1 - a.uni / e.n))} 
                        tol = 0.0001
                        
                        for (j in 1:nrow(sim.up)) {
                            if(sim.up[j,"Size"] <= n.crit){
                                sim.up[j,"s.cr"] = rootSolve::uniroot.all(f.cr, 
                                                                            c(qchisq(tol,2*sim.up[j,"Size"]), qchisq(1-tol,2*sim.up[j,"Size"])), 
                                                                            tol = tol, 
                                                                            size = sim.up[j,"Size"],
                                                                            e.n = sim.up[j,"ExpectedNn"])[1]
                            }
                            else{
                                sim.up[j,"s.cr"] = 0
                            }
                        }
                        sim.up = rbind.data.frame(sim.up,data.frame(Size = n.crit,
                                                                        SumProbabilities = 0,
                                                                        MeanProbabilities = 0,
                                                                        ExpectedNn = 0,
                                                                        PMaxNGreaterNi = 0,
                                                                        distToCrit = 0,
                                                                        s.cr = 0))
                        output$up_crit_reg = renderPlot(
                            {
                                validate(need(sim.up$s.cr,"Постройте критическую область"))
                            })
                        
                        s.n.up = function(x, p, data){
                            print(data@data[which(data@data$GID_1 %in% x),render.col()])
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
                                    data.discharges$S.CR[j] = sim.up[nearest,"s.cr"]
                                    data.discharges$Label[j] = paste(clusters[[as.character(data.discharges$ID[j])]],collapse = "; ")
                                    data.discharges$Names[j] = paste(buf@data$NAME_1[which(buf@data$GID_1 %in% clusters[[as.character(data.clusters$ID[j])]])],collapse = "; ")
                                    
                                }
                                print("Data discharges")
                                print(data.discharges)
                                signif.dis = data.discharges[data.discharges$N >= n.crit | data.discharges$S >= data.discharges$S.CR,]
                                print("Significant discharges")
                                print(signif.dis)
                                if(nrow(signif.dis) > 0){
                                    has.discharges = 2
                                    #colours = cluster.color(nrow(signif.dis))
                                    for (i in signif.dis$ID) {
                                        region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                        print("Region data")
                                        #print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                        print(region@data)
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
                                
                                res.dis(data.discharges)
                                mc.sim.up(sim.up)
                                output$up_crit_reg = renderPlotly(
                                    {
                                        validate(need(sim.up$s.cr,"Постройте критическую область"))
                                        sim.up = sim.up[order(sim.up$Size),]
                                        fig = plot_ly(sim.up, x = ~Size, y = ~s.cr,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
                                            add_trace(data = data.discharges,x = ~N,y = ~S, name = 'All', mode = 'markers',color = I("red"), size = 3, text = ~Label)%>%
                                            add_trace(data = signif.dis,x = ~N,y = ~S, name = 'Significant', mode = 'markers',color = I("black"), size = 3, text = ~Label)%>%
                                            layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Разряжения")
                                        fig
                                        # ggplot(sim.up) + geom_line(aes(x = Size, y = s.cr)) +
                                        #     geom_point(data = data.clust,mapping = aes(x = N, y = S),color = "yellow",size = 3) + 
                                        #     geom_point(data = signif.clust,mapping = aes(x = N, y = S),color = "black",size = 3) + theme_bw()
                                        
                                    })
                                output$up_stat = renderTable({
                                    data.discharges
                                },digits = 4)
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
            
            observeEvent({input$first.cases
                        input$second.cases},ignoreInit = T,ignoreNULL = T,
                         {
                             
                             req(copy.map(),input$first,input$second)
                             buf = copy.map()
                             req(is.numeric(buf@data[,cols$first]))
                             req(is.numeric(buf@data[,cols$second]))
                             
                             print(paste("First =",buf@data[row(),cols$first]))
                             print(paste("Second =",buf@data[row(),cols$second]))
                             buf@data[row(),cols$first] = input$first.cases
                             buf@data[row(),cols$second] = input$second.cases
                             print(paste("First =",buf@data[row(),cols$first]))
                             print(paste("Second =",buf@data[row(),cols$second]))
                             
                             first.time = as.numeric(buf@data[,cols$first])
                             second.time = as.numeric(buf@data[,cols$second])
                             buf@data$X = (first.time - second.time) / (sqrt(first.time + second.time))
                             buf@data$P.VALUE = pnorm(buf@data$X,lower.tail = F)
                             
                             m1 = mean(buf@data[,cols$first],na.rm = T)
                             buf@data$X1 = (buf@data[,cols$first] - m1) / sqrt(m1)
                             buf@data$P.VALUE1 = pnorm(buf@data$X1,lower.tail = F)
                             
                             m2 = mean(buf@data[,cols$second],na.rm = T)
                             buf@data$X2 = (buf@data[,cols$second] - m2) / sqrt(m2)
                             buf@data$P.VALUE2 = pnorm(buf@data$X2,lower.tail = F)
                             
                             print("-----------------------------------------------------")
                             print("Change cases")
                             print(paste("Row =",row(),"Region =",buf@data$NAME_1[row()]))
                             print(paste("First =",buf@data[row(),cols$first]))
                             print(paste("Second =",buf@data[row(),cols$second]))
                             print("X")
                             print(buf@data$X)
                             print("P-value")
                             print(buf@data$P.VALUE)
                             print("Mean 1")
                             print(m1)
                             print("X 1")
                             print(buf@data$X1)
                             print("P-value 1")
                             print(buf@data$P.VALUE1)
                             print("Mean 2")
                             print(m2)
                             print("X 2")
                             print(buf@data$X2)
                             print("P-value 2")
                             print(buf@data$P.VALUE2)
                             print("-----------------------------------------------------")
                             
                             copy.map(buf)
                             region = buf[row(), ]
                             
                             # proxy %>%
                             #     addPolygons(
                             #         data = region,
                             #         weight = 1,
                             #         fillOpacity = 0.25,
                             #         color = p(region@data[,render.col()]),
                             #         layerId = region@data$GID_1,
                             #         highlight = highlightOptions(
                             #             weight = 5,
                             #             color = p(region@data[,render.col()]),
                             #             fillOpacity = 0.7,
                             #             bringToFront = TRUE
                             #         )
                             #     ) %>%
                             #     addPolylines(
                             #         data = region,
                             #         layerId = region@data$GID_1,
                             #         color = p(region@data[,render.col()]),
                             #         weight = 5,
                             #         opacity = 1
                             #     )
                             # print("Render map! - change cases")
                         })
            
            
            #TODO
            observeEvent(input$clearHighlight, ignoreInit = T,
            {
                if(row() != 0){
                    hide("first.cases")
                    hide("second.cases")
                }
                
                output$down_crit_reg = renderPlot(
                    {
                        validate(FALSE,"Запустите критерий")
                    })
                
                output$up_crit_reg = renderPlot(
                    {
                        validate(FALSE,"Запустите критерий")
                    })
                
                output$down_stat = renderTable({
                    validate(FALSE,"Запустите критерий")
                })
                
                output$up_stat = renderTable({
                    validate(FALSE,"Запустите критерий")
                })
                row(0)
                click.list(NULL)
                copy.map(map.data())
                print("clear and calculate")
                req(copy.map(),input$first,input$second)
                data = copy.map()
                print(c(input$first,input$second))
                req(is.numeric(copy.map()@data[,cols$first]))
                req(is.numeric(copy.map()@data[,cols$second]))
                first.time = as.numeric(copy.map()@data[,cols$first])
                second.time = as.numeric(copy.map()@data[,cols$second])
                print(c(first.time,second.time))
                data@data$X = (first.time - second.time) / (sqrt(first.time + second.time))
                data@data$P.VALUE = pnorm(data@data$X,lower.tail = F)
                copy.map(data)
                print(copy.map()@data[,render.col()])
                withProgress(message = "Rendering...", {
                      proxy %>% clearShapes()  %>%
                                clearControls() %>% 
                                addProviderTiles("CartoDB.Positron") %>%
                        addPolygons(
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
                                  title = "P-Value",
                                  values = copy.map()@data[,render.col()])
                    print("Render map! - clear")
                })
                res.dis(NULL)
                res.clust(NULL)
            })
            
            observeEvent(input$save_res,{
                if (is.integer(input$save_res)) {
                    cat("No file has been selected (shinyResSave)\n")
                } 
                else {
                    info = parseSavePath(volumes, input$save_res)
                    if(!is.null(res.dis())){
                        write.xlsx(res.dis(),file = as.character(info$datapath),sheetName = "Discharges")
                    }
                    if(!is.null(res.clust())){
                        write.xlsx(res.clust(),file = as.character(info$datapath),sheetName = "Clusters",append = T)
                    }
                    if(!is.null(mc.sim.down())){
                        write.xlsx(mc.sim.down(),file = as.character(info$datapath),sheetName = "Crit_Down",append = T)
                    }
                    if(!is.null(mc.sim.up())){
                        write.xlsx(mc.sim.up(),file = as.character(info$datapath),sheetName = "Crit_Up",append = T)
                    }
                }
            })
        }
    )
}