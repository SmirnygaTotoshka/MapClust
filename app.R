#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("mc_ui.R")
source("mc_server.R")
source("global.R")
source("main_ui.R")
source("main_server.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    tabsetPanel(
        MonteCarlo.UI(monte.carlo.id),
        Main.UI(main.id)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    MonteCarlo.Server(monte.carlo.id)
    Main.Server(main.id)
}

# Run the application 
shinyApp(ui = ui, server = server)
