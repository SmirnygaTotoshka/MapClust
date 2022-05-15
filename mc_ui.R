library(shinyFiles)
MonteCarlo.UI = function(id, label = "Monte Carlo"){
    mc = NS(id)
    tabPanel("Симуляция Монте-Карло",
             sidebarLayout(
                 sidebarPanel(
                     verticalLayout(
                         shinyDirButton(mc("ogr_dir"), "Выберите карту", "Загрузить"),
                         sliderInput(mc("number.iteration"), "Выберите число итераций",  min = 100, max = 10000,value = 1000,step = 100),
                         sliderInput(mc("mark.limits"), "Пределы", min = 0,max = 1, step = 0.01, value = c(0.2,0.8)),
                         checkboxInput(mc("queen"), "8-связный поиск?", value = F),
                         hr(),
                         splitLayout(
                             actionButton(mc("start.sim"),"Запуск"),
                             shinySaveButton(mc("save_sim"), "Сохранить результат", "Save result",filetype=c(".xlsx"))
                         )
                     )
                 ),
                 #+ Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Нижняя граница", plotlyOutput(mc("down.PDF")),plotlyOutput(mc("down.alpha2")),tableOutput(mc("down.table"))),
                         tabPanel("Верхняя граница", plotlyOutput(mc("up.PDF")),plotlyOutput(mc("up.alpha2")),tableOutput(mc("up.table")))
                     )
                 )
             )
    )
}