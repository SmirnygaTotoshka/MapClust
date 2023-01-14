#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
options(shiny.maxRequestSize=100*1024^2)
source("mc_ui.R", encoding = "UTF-8")
source("mc_server.R", encoding = "UTF-8")
source("global.R", encoding = "UTF-8")
source("main_ui.R", encoding = "UTF-8")
source("main_server.R", encoding = "UTF-8")



# Define UI for application that draws a histogram
ui <- tagList(
    useShinyjs(),
    navbarPage("MapClust",
        #rmarkdown::render("intro_rus.Rmd", rmarkdown::html_document(toc = TRUE,toc_float = T))
        tabPanel("Введение", fluidPage(htmlOutput("intro"))),
        MonteCarlo.UI(monte.carlo.id),
        Main.UI(main.id)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$intro<-renderUI({includeHTML("intro_rus.html")})
    MonteCarlo.Server(monte.carlo.id)
    Main.Server(main.id)
}

# Run the application 
shinyApp(ui = ui, server = server)
