Example.UI = function(id, label = "Примеры данных"){
    ex = NS(id)
    tabPanel("Примеры данных", fluidPage(htmlOutput(ex("example_data"))))
}

Example.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            output$example_data = renderUI({includeHTML("example_data_rus.html")})
        }
    )
}