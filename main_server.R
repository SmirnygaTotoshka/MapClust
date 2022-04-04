library(leaflet)
library(xlsx)
library(ggplot2)
library(plotly)
library(igraph)
library(surveillance)
Main.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            disable("do.clust")
            disable("clearHighlight")
            disable("save_res")
            ns = session$ns
            row = reactiveVal(0)
            
            mc.params = reactiveVal(NULL)
            mc.adj.mat = reactiveVal(NULL)
            mc.sim.down = reactiveVal(NULL)
            mc.sim.up = reactiveVal(NULL)
            
            res.dis = reactiveVal(NULL)
            res.clust = reactiveVal(NULL)
           
            click.list <- reactiveVal(NULL)
            proxy = leafletProxy("map")

            map.data = reactiveVal()
            copy.map = reactiveVal()
            
                output$down_crit_reg = renderPlot(
                {
                   validate(FALSE,"Launch criteria")
                })

                output$up_crit_reg = renderPlot(
                {
                   validate(FALSE,"Launch criteria")
                })
                
                output$down_stat = renderTable({
                    validate(FALSE,"Launch criteria")
                })
                
                output$up_stat = renderTable({
                    validate(FALSE,"Launch criteria")
                })
            
            
            # observe({
            #     map.df = rgdal::readOGR(dsn = "~/RWorkspace/ClickMap/Romania", layer = "Romania")
            #     
            #     map.df@data$CASES = rpois(length(map.df@data$GID_1),input$ChooseLmbd) #,20)
            #     m = mean(map.df@data$CASES)
            #     map.df@data$X = (map.df@data$CASES - m) / sqrt(m)
            #     map.df@data$P.VALUE = pnorm(map.df@data$X,lower.tail = F)
            #     #map.df@data$P.VALUE = sapply(map.df@data$P.VALUE, function(x){if(x == 0){x = 0.000001}})
            #     map.data(map.df)
            #     copy.map(map.df)
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
            
            observe({
                if (is.integer(input$ogr_dir)) {
                    cat("No map directory has been selected\n")
                }
                else {
                    tryCatch({
                        path = parseDirPath(volumes, input$ogr_dir)
                        layers = strsplit(path, "/")[[1]]
                        layer = layers[length(layers)]
                        map.df = rgdal::readOGR(dsn = path, layer = as.character(layer))
                        map.df@data$CASES = rpois(length(map.df@data$GID_1), input$ChooseLmbd)
                        m = mean(map.df@data$CASES)
                        map.df@data$X = (map.df@data$CASES - m) / sqrt(m)
                        map.df@data$P.VALUE = pnorm(map.df@data$X,lower.tail = F)
                        map.data(map.df)
                        copy.map(map.df)
                    },
                    error = function(e){
                        shinyalert("Error",
                                   paste("Cannot parse the data, because",e),
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
        
              output$map <- renderLeaflet({
                validate(need(copy.map(),label = "OGR map data"))
                 
                withProgress(message = "Rendering...", {
                  leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
                    addPolygons(
                      data = copy.map(),
                      weight = 1,
                      fillOpacity = 0.25,
                      color = p(copy.map()@data$P.VALUE),
                      layerId = copy.map()@data$GID_1,
                      highlight = highlightOptions(
                        weight = 5,
                        color = p(copy.map()@data$P.VALUE),
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                      )
                    ) %>%
                    addLegend(pal = p,
                              title = "P-Value",
                              values = copy.map()@data$P.VALUE)
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
                                 paste("Cannot parse the data, because",e),
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
                ev <- input$map_shape_click
                row(which(isolate.copy@data$GID_1 %in% ev$id))
                # If already selected, first remove previous selection
                if (!is.null(click.list())) {
                     region = isolate.copy[which(isolate.copy@data$GID_1 %in% click.list()), ]
                     proxy %>% removeShape(layerId = region@data$GID_1) %>%
                               addPolygons(
                                     data = region,
                                     weight = 1,
                                     fillOpacity = 0.25,
                                     color = p(region@data$P.VALUE),
                                     layerId = region@data$GID_1,
                                     highlight = highlightOptions(
                                       weight = 5,
                                       color = p(region@data$P.VALUE),
                                       fillOpacity = 0.7,
                                       bringToFront = TRUE
                                     )
                               )
               }
               else{
                    insertUI(selector = paste0("#",ns("ChooseLmbd")),
                             where = "afterEnd",
                             ui = sliderInput(ns("changer"),
                                              paste("Change value for",isolate.copy@data$NAME_0[row()],isolate.copy@data$NAME_1[row()]),
                                              min = 0,
                                              max = 3 * input$ChooseLmbd,
                                              value = isolate.copy@data$CASES[row()]
                                            )
                             )
                     insertUI(selector = paste0("#",ns("changer")),
                              where = "afterEnd",
                              ui = textOutput(ns("pvalue")))
                }

                click.list(ev$id)  # we only store the last click now!
                updateSelectInput(session = session,inputId = "map", selected = ev$id)
                updateSliderInput(session = session,inputId = "changer",
                                  label = paste("Change value for", isolate.copy@data$NAME_0[row()],isolate.copy@data$NAME_1[row()]),
                                  min = 0,
                                  max = 3 * input$ChooseLmbd,
                                  value = isolate.copy@data$CASES[row()]
                               )#TODO doesnt change when region change
                 output$pvalue = renderText({
                                 paste(
                                   "P-value for",
                                   copy.map()@data$NAME_0[row()],
                                   copy.map()@data$NAME_1[row()],
                                   copy.map()@data$GID_1[row()],
                                   "=",
                                   copy.map()@data$P.VALUE[row()]
                                 )
               })
               region = isolate.copy[row(), ]
               proxy %>% addPolylines(
                                 data = region,
                                 layerId = region@data$GID_1,
                                 color = p(region@data$P.VALUE),
                                 weight = 5,
                                 opacity = 1
                               )

            })
            
              observeEvent(input$changer,ignoreInit = T,ignoreNULL = T,
              {
                 req(input$changer)
                 
                 buf = copy.map()
                 buf@data$CASES[row()] = input$changer
                 m = mean(buf@data$CASES)
                 buf@data$X = (buf@data$CASES - m) / sqrt(m)
                 buf@data$P.VALUE = pnorm(buf@data$X,lower.tail = F)
                 copy.map(buf)
                 
                 region = buf[row(), ]
                 proxy %>%
                            addPolygons(
                                 data = region,
                                 weight = 1,
                                 fillOpacity = 0.25,
                                 color = p(region@data$P.VALUE),
                                 layerId = region@data$GID_1,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = p(region@data$P.VALUE),
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE
                                 )
                               ) %>%
                            addPolylines(
                                 data = region,
                                 layerId = region@data$GID_1,
                                 color = p(region@data$P.VALUE),
                                 weight = 5,
                                 opacity = 1
                               )
            })
              
            observeEvent(input$do.clust,
            {
                print(identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(mc.adj.mat())))))
                print(sort(as.vector(copy.map()@data$GID_1)))
                print(sort(as.vector(row.names(mc.adj.mat()))))
                if(!identical(sort(as.vector(copy.map()@data$GID_1)),sort(as.vector(row.names(mc.adj.mat()))))){
                    shinyalert("Error","Loaded parameters are not for this map.", type = "error")
                    return()
                }
                lmbd = input$ChooseLmbd
                #0 - no discharges/clusters
                #1 - has discharges/clusters but they are not significant
                #2 - has significant discharges/clusters
                has.discharges = 0
                has.clusters = 0
                p.down = isolate(mc.params())[1,"Probabilities"]
                p.up = isolate(mc.params())[2,"Probabilities"]
                if(p.down > 0 && p.down < 1)
                {
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
                        print(data@data$P.VALUE[which(data@data$GID_1 %in% x)])
                        return(-2 * sum(log(data@data$P.VALUE[which(data@data$GID_1 %in% x)] / p)))
                    }
    
                    buf = isolate(copy.map())
                    buf@data$CRIT = ifelse(buf@data$P.VALUE<=p.down,1,0)
                    print("meow")
                    select = which(buf@data$CRIT == 1,arr.ind = T)
                    if(length(select) != 0){
                        graph <- graph.adjacency(adj.mat[select,select])
                        comps <- components(graph)
                        print("meow")
                        clusters = igraph::groups(comps)
                        print(clusters)
                        if(length(clusters) > 0){
                            has.discharges = 1
                            print("meow")
                            s = sapply(clusters,s.n.down,p = p.down, data = buf)
                            sizes = sapply(clusters, length)
                            print("meow")
                            cls = as.data.frame(table(sizes))
                            data.discharges = data.frame(ID = names(s),N = sizes,S = s)
                            #names(clusters) = data.discharges$ID
                            print("Data clust")
                            print(data.discharges)
                            for (j in 1:nrow(data.discharges)) {
                                data.discharges$S.CR[j] = sim.down[sim.down$Size == data.discharges$N[j],"s.cr"]
                                data.discharges$Label[j] = paste(clusters[[as.character(data.discharges$ID[j])]],collapse = "; ")
                            }
                            signif.dis = data.discharges[data.discharges$N >= n.crit | data.discharges$S >= data.discharges$S.CR,]
                            print(signif.dis)
                            if(nrow(signif.dis) > 0){
                                has.discharges = 2
                                #colours = cluster.color(nrow(signif.dis))
                                for (i in signif.dis$ID) {
                                    region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                    print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                    print(region@data)
                                    union.dis = surveillance::unionSpatialPolygons(region)
                                    proxy %>%
                                        addPolylines(
                                                data = union.dis,
                                                layerId = paste0("discharges",i),
                                                color = "blue",
                                                weight = 5,
                                                opacity = 1
                                            )
                                    }
                            }
                            res.dis(data.discharges)
                            mc.sim.down(sim.down)
                            output$down_crit_reg = renderPlotly(
                            {
                                validate(need(sim.down$s.cr,"Build critical region"))
                                sim.down = sim.down[order(sim.down$Size),]
                                fig = plot_ly(sim.down, x = ~Size, y = ~s.cr,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Critical region") %>%
                                        add_trace(data = data.discharges,x = ~N,y = ~S, name = 'All', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
                                    add_trace(data = signif.dis,x = ~N,y = ~S, name = 'Significant', mode = 'markers',color = I("blue"), size = 3, text = ~Label)
                                fig
                                    # ggplot(sim.down) + geom_line(aes(x = Size, y = s.cr)) +
                                    #     geom_point(data = data.discharges,mapping = aes(x = N, y = S),color = "green",size = 3) +
                                    #     geom_point(data = signif.dis,mapping = aes(x = N, y = S),color = "blue",size = 3) + theme_bw()
    
                            })
                            output$down_stat = renderTable({
                                    data.discharges
                                },digits = 4)
                            }
                        }
                    }
                    #TODO
                    #Plots make plotly

                    if(p.up > 0 && p.up < 1)
                    {
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
                                validate(need(sim.up$s.cr,"Build critical region"))
                            })
                        
                        s.n.up = function(x, p, data){
                            print(data@data$P.VALUE[which(data@data$GID_1 %in% x)])
                            return(-2 * sum((log((1 - data@data$P.VALUE[which(data@data$GID_1 %in% x)]) / (1 - p)))))
                        }
                        
                        buf = isolate(copy.map())
                        buf@data$CRIT = ifelse(buf@data$P.VALUE>=p.up,1,0)
                        print("meow")
                        select = which(buf@data$CRIT == 1,arr.ind = T)
                        if(length(select) != 0){
                            
                            graph <- graph.adjacency(adj.mat[select,select])
                            comps <- components(graph)
                            clusters = igraph::groups(comps)
                            print(clusters)
                            if(length(clusters) > 0){
                                print("meow")
                                has.clusters = 1
                                s = sapply(clusters,s.n.up,p = p.up, data = buf)
                                sizes = sapply(clusters, length)
                                print("meow")
                                cls = as.data.frame(table(sizes))
                                data.clust = data.frame(ID = names(s),N = sizes,S = s)
                                #names(clusters) = data.clust$ID
                                print("Data clust")
                                print(data.clust)
                                for (j in 1:nrow(data.clust)) {
                                    data.clust$S.CR[j] = sim.up[sim.up$Size == data.clust$N[j],"s.cr"]
                                    data.clust$Label[j] = paste(clusters[[as.character(data.clust$ID[j])]],collapse = "; ")
                                }
                                signif.clust = data.clust[data.clust$N >= n.crit | data.clust$S >= data.clust$S.CR,]
                                print(signif.clust)
                                if(nrow(signif.clust) > 0){
                                    has.clusters = 2
                                    #colours = cluster.color(nrow(signif.clust))
                                    for (i in signif.clust$ID) {
                                        region = buf[which(copy.map()@data$GID_1 %in% clusters[[i]]),]
                                        print(which(copy.map()@data$GID_1 %in% clusters[[i]]))
                                        #sf.cl = sf::st_as_sf()
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
                                
                                res.clust(data.clust)
                                mc.sim.up(sim.up)
                                output$up_crit_reg = renderPlotly(
                                    {
                                        validate(need(sim.up$s.cr,"Build critical region"))
                                        sim.up = sim.up[order(sim.up$Size),]
                                        fig = plot_ly(sim.up, x = ~Size, y = ~s.cr,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Critical region") %>%
                                            add_trace(data = data.clust,x = ~N,y = ~S, name = 'All', mode = 'markers',color = I("red"), size = 3, text = ~Label)%>%
                                            add_trace(data = signif.clust,x = ~N,y = ~S, name = 'Significant', mode = 'markers',color = I("black"), size = 3, text = ~Label)
                                        fig
                                        # ggplot(sim.up) + geom_line(aes(x = Size, y = s.cr)) +
                                        #     geom_point(data = data.clust,mapping = aes(x = N, y = S),color = "yellow",size = 3) + 
                                        #     geom_point(data = signif.clust,mapping = aes(x = N, y = S),color = "black",size = 3) + theme_bw()
                                        
                                    })
                                output$up_stat = renderTable({
                                    data.clust
                                },digits = 4)
                            }
                        }
                    }
                    message = ""
                    if(has.discharges == 0 && has.clusters == 0){
                        message = "Discharges and clusters isn`t found."
                    }
                    else if(has.discharges == 1 || has.clusters == 1){
                        insertion = ifelse(has.discharges == 1 && has.clusters == 1, " Discharges and clusters",
                                           ifelse(has.discharges == 1, "Discharges", "Clusters"))
                        message = paste(insertion,"has found, but they are not significant.")
                    }
                    if(has.discharges != 2 && has.clusters != 2){
                        shinyalert("Result", 
                                   message,
                                   type = "info"
                        ) 
                    }
                    enable("save_res")
            })
            
            #TODO
            observeEvent({input$clearHighlight
                            input$ChooseLmbd
                        }, ignoreInit = T,
            {
                if(row() != 0){
                      removeUI(selector = paste0("div:has(> #",ns("changer"),")"))
                      removeUI(selector = paste0("div:has(> #",ns("pvalue"),")"))
                }
                
                output$down_crit_reg = renderPlot(
                    {
                        validate(FALSE,"Launch criteria")
                    })
                
                output$up_crit_reg = renderPlot(
                    {
                        validate(FALSE,"Launch criteria")
                    })
                
                output$down_stat = renderTable({
                    validate(FALSE,"Launch criteria")
                })
                
                output$up_stat = renderTable({
                    validate(FALSE,"Launch criteria")
                })
                row(0)
                click.list(NULL)
                copy.map(map.data())
                withProgress(message = "Rendering...", {
                      proxy %>% clearShapes()  %>%
                                clearControls() %>% 
                                addProviderTiles("CartoDB.Positron") %>%
                        addPolygons(
                            data = copy.map(),
                            weight = 1,
                            fillOpacity = 0.25,
                            color = p(copy.map()@data$P.VALUE),
                            layerId = copy.map()@data$GID_1,
                            highlight = highlightOptions(
                                weight = 5,
                                color = p(copy.map()@data$P.VALUE),
                                fillOpacity = 0.7,
                                bringToFront = TRUE
                            )
                        ) %>%
                        addLegend(pal = p,
                                  title = "P-Value",
                                  values = copy.map()@data$P.VALUE)
                })
                res.dis(NULL)
                res.clust(NULL)
            })
            observe({
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