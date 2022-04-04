library(shinyFiles)
library(leaflet)
Main.UI = function(id, label = "Criteria"){
    crit = NS(id)
    tabPanel("Criteria",
             sidebarLayout(
                 sidebarPanel(
                     verticalLayout(
                         shinyDirButton(crit("ogr_dir"), "Choose map directory", "Upload"),
                         fileInput(crit("mc_res"), "Choose simulation result", buttonLabel = "Browse xlsx"),
                         sliderInput(crit("alpha"), "Type I error", min = 0.001,max = 0.1, step = 0.001, value = 0.05),
                         sliderInput(crit("ChooseLmbd"), "Poisson \u03BB", min = 1, max = 50, value = init.lmbd, step = 1),
                         actionButton(crit("do.clust"),"Start procedure"),
                         hr(),
                         splitLayout(
                             actionButton(crit("clearHighlight"),"Clear All"),
                             shinySaveButton(crit("save_res"), "Save result", "Save result",filetype=c(".xlsx"))
                         )
                     )
                 ),
                 #+ Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Map", leafletOutput(crit("map"),height=600)),
                         tabPanel("Discharges statistics",plotlyOutput(crit("down_crit_reg")),tableOutput(crit("down_stat"))),
                         tabPanel("Clusters statistics",plotlyOutput(crit("up_crit_reg")),tableOutput(crit("up_stat")))
                     )
                 )
             )
    )
}