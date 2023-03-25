Example.UI = function(id, label = "Примеры данных"){
    ex = NS(id)
    tabPanel("Примеры данных", fluidPage(
      verticalLayout(
        HTML(
          "<h1> Примеры датасетов </h1>

                <p>На этой странице представлены готовые версии наборов данных, которые можно сразу использовать.</p>

                <h2> 1. Офицальная статистика по заболеваемости ВИЧ в РФ.</h2>

                 <p>Данные взяты с портала ЕМИСС с 2005 по 2016 год.
                 Заболеваемость с впервые в жизни установленным диагнозом ВИЧ-инфекции на 100 тыс. человек населения.
                 До 2014 Тюменская и Архангельская области &mdash; с автономными округами. </p>
               "
        ),
        downloadButton(ex("hiv.download")),
        HTML("
                <h2> 2. Эпидемия кори в Румынии</h2>
            <p>Данные по эпидемии кори в Румынии на конец 2018 года.
            Абсолютное количество случаев (Cases) и заболеваемость на 100000 населения (Incidence).
            Данные взяты из статьи <i>Dascalu S (2019) Measles Epidemics in Romania: Lessons for Public Health and Future Policy. Front. Public Health 7:98. doi: 10.3389/fpubh.2019.00098</i></p>

               "
        ),
        downloadButton(ex("measles.download")),
        HTML('
            <h2> 3. Содержание частиц диаметром 10 микрометров и меньше в воздухе. Румыния.</h2>
            <p>Данные взяты с сайта <a href="https://discomap.eea.europa.eu/App/AirQualityStatistics/index.html">Европейского агенства по окружающей среде</a>. Данные за 2018 и 2021 год.</p>'
        ),
        downloadButton(ex("pm10.rou")),
      )))
}

Example.Server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
    
            output$hiv.download = downloadHandler(
                filename <- function() {paste("HIV-RF", "zip", sep=".")},
                content <- function(file) {file.copy("datasets/HIV-RF.zip", file)},
                contentType = "application/zip"
            )
            
            output$measles.download = downloadHandler(
              filename <- function() {paste("Measles-Rom", "zip", sep=".")},
              content <- function(file) {file.copy("datasets/Measles-Rom.zip", file)},
              contentType = "application/zip"
            )
            
            output$pm10.rou = downloadHandler(
                filename <- function() {paste("pm10.rou", "zip", sep=".")},
                content <- function(file) {file.copy("datasets/PM10_ROU.zip", file)},
                contentType = "application/zip"
            )
        }
    )
}