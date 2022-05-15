library(shinyFiles)
library(leaflet)
Main.UI = function(id, label = "Критерий"){
    crit = NS(id)
    tabPanel("Критерий",
             sidebarLayout(
                 sidebarPanel(
                     verticalLayout(
                           wellPanel(shinyDirButton(crit("ogr_dir"), title = "Выберите карту", label = "Карта"),
                                     fileInput(crit("mc_res"), "Загрузить результат симуляции", buttonLabel = "Файл .xlsx")),
                           wellPanel(selectInput(crit("representation"),"Режим отображения",choices = c("Первый момент","Второй момент","Разница"),selected = "Разница"),
                                     actionButton(crit("check.alpha"),"Проверить ошибку первого рода")),
                         sliderInput(crit("alpha"), "Ошибка первого рода", min = 0.001,max = 0.1, step = 0.001, value = 0.05),
                         hr(),
                         textOutput(crit("dif.print")),
                         hr(),
                         checkboxInput(crit("changeableNA"),"Можно ли изменять NA значения?",FALSE),
                         fluidRow(
                             column(6, wellPanel(selectInput(crit("first"),"Выберите первый момент", choices = NULL),
                                                 numericInput(crit("first.cases"),
                                                             label = paste("Value(1 time)"),
                                                             min = 0,
                                                             max = 1,
                                                             value = 0.5,step = 0.1),
                                                 textOutput(crit("first.pvalue")))),                           
                             column(6, wellPanel(selectInput(crit("second"),"Выберите второй момент", choices = NULL),
                                                 numericInput(crit("second.cases"),
                                                             label = paste("Value(2 time)"),
                                                             min = 0,
                                                             max = 1,
                                                             value = 0.5,step = 0.1),
                                                 textOutput(crit("second.pvalue"))))
                         ),
                         hr(),
                         wellPanel(fluidRow(
                             column(4, actionButton(crit("do.clust"),"Запуск")),
                             column(4, shinySaveButton(crit("save_res"), "Сохранить результат", "Сохранить результат",filetype=c(".xlsx"))),
                             column(4,  actionButton(crit("clearHighlight"),"Очистить"))
                         ))
                     )
                 ),
                 #+ Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Карта", leafletOutput(crit("map"),height=600)),
                         tabPanel("Кластеры",plotlyOutput(crit("down_crit_reg")),tableOutput(crit("down_stat"))),
                         tabPanel("Разряжения",plotlyOutput(crit("up_crit_reg")),tableOutput(crit("up_stat")))
                     )
                 )
             )
    )
}