library(shinyFiles)
MonteCarlo.UI = function(id, label = "Monte Carlo"){
    mc = NS(id)
    tabPanel("Monte-Carlo simulation",
             sidebarLayout(
                 sidebarPanel(
                     verticalLayout(
                         shinyDirButton(mc("ogr_dir"), "Choose map directory", "Upload"),
                         sliderInput(mc("number.iteration"), "Choose number of iteration",  min = 100, max = 10000,value = 1000,step = 100),
                         sliderInput(mc("mark.limits"), "Limits", min = 0,max = 1, step = 0.01, value = c(0.2,0.8)),
                         checkboxInput(mc("queen"), "8-binded?", value = F),
                         hr(),
                         splitLayout(
                             actionButton(mc("start.sim"),"Start"),
                             shinySaveButton(mc("save_sim"), "Save simulation result", "Save result",filetype=c(".xlsx"))
                         )
                     )
                 ),
                 #+ Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Down limit simulation", plotOutput(mc("down.PDF")),plotOutput(mc("down.alpha2")),tableOutput(mc("down.table"))),
                         tabPanel("Up limit simulation", plotOutput(mc("up.PDF")),plotOutput(mc("up.alpha2")),tableOutput(mc("up.table")))
                     )
                 )
             )
    )
}