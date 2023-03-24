library(leaflet)

Main.UI = function(id, label = "Критерий"){
    crit = NS(id)
    tabPanel("Критерий",
             sidebarLayout(
                 sidebarPanel(
                     verticalLayout(
                           wellPanel(fileInput(crit("ogr_dir"), label = "Карта", multiple = T),
                                     fileInput(crit("mc_res"), "Загрузить результат симуляции", buttonLabel = "Файл .xlsx", accept = ".xlsx")),
                           wellPanel(selectInput(crit("representation"),"Режим отображения данных",choices = c("Первый момент","Второй момент","Разница"),selected = "Разница"),
                                     selectInput(crit("representation_map"),"Режим отображения результатов",choices = c("Разряжения","Кластеры"),selected = "Кластеры")
                                     #actionButton(crit("check.alpha"),"Проверить ошибку первого рода")
                                     ),
                         sliderInput(crit("alpha"), "Ошибка первого рода", min = 0.001,max = 0.1, step = 0.001, value = 0.05),
                         hr(),
                         textOutput(crit("dif.print")),
                         hr(),
                         checkboxInput(crit("changeableNA"),"Можно ли изменять NA значения?",FALSE),
                         fluidRow(
                             column(6, wellPanel(selectInput(crit("first"),"Выберите первый момент", choices = NULL),
                                                 hidden(numericInput(crit("first.cases"),
                                                             label = paste("Value(1 time)"),
                                                             min = 0,
                                                             max = 1,
                                                             value = 0.5,step = 0.1)),
                                                 textOutput(crit("first.pvalue")))),                           
                             column(6, wellPanel(selectInput(crit("second"),"Выберите второй момент", choices = NULL),
                                                 hidden(numericInput(crit("second.cases"),
                                                             label = paste("Value(2 time)"),
                                                             min = 0,
                                                             max = 1,
                                                             value = 0.5,step = 0.1)),
                                                 textOutput(crit("second.pvalue"))))
                         ),
                         hr(),
                         wellPanel(fluidRow(
                             column(4, align = "center", disabled(actionButton(crit("do.clust"),"Запуск"))),
                             column(4, align = "center", disabled(downloadButton(crit("save_res"), "Сохранить результат"))),
                             column(4, align = "center", disabled(actionButton(crit("clearHighlight"),"Очистить")))
                         ))
                     )
                 ),
                 #+ Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Карта", leafletOutput(crit("map"),height=600), plotlyOutput(crit("val_distr")), tableOutput(crit("val"))),
                         tabPanel("Разряжения",plotlyOutput(crit("down_crit_reg")),tableOutput(crit("down_stat"))),
                         tabPanel("Кластеры",plotlyOutput(crit("up.crit.reg")),tableOutput(crit("up.stat")))
                     )
                 )
             )
    )
}