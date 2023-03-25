read_excel_all = function(path){
    require(readxl)
    sheetnames <- excel_sheets(path)
    mylist <- lapply(excel_sheets(path), read_excel, path = path)
    
    # name the dataframes
    names(mylist) <- sheetnames
    mylist
}
library(dplyr)
library(plotly)
hiv.res.2005 = read_excel_all("~/Documents/Pyat/test_art5/HIV_RUS/result-2005-alpha=0.05.xlsx")

hiv.res.2005$Crit_Down = hiv.res.2005$Crit_Down[order(hiv.res.2005$Crit_Down$Size),]
hiv.res.2005$Crit_Up = hiv.res.2005$Crit_Up[order(hiv.res.2005$Crit_Up$Size),]

fig1 = plot_ly(hiv.res.2005$Crit_Down, x = ~Size, y = ~S.crit,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Кластеры")
if (!is.null(hiv.res.2005$Clusters)){
    fig1 = fig1 %>%
        add_trace(data = hiv.res.2005$Clusters,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
        add_trace(data = hiv.res.2005$Clusters %>% filter(S > S.crit),x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label)
}


fig2 = plot_ly(hiv.res.2005$Crit_Up, x = ~Size, y = ~S.crit,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область", showlegend = F) %>%
    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Кластеры")
if (!is.null(hiv.res.2005$Clusters)){
    fig2 = fig2 %>%
        add_trace(data = hiv.res.2005$Discharges,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label, showlegend = F)%>%
        add_trace(data = hiv.res.2005$Discharges %>% filter(S > S.crit),x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label, showlegend = F)
}
annotations = list( 
    list( 
        x = 0.2,  
        y = 1.0,  
        text = "Кластеры",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        yshift = -10,
        showarrow = FALSE 
    ),  
    list( 
        x = 0.8,  
        y = 1,  
        text = "Разряжения",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom", 
        yshift = -10,
        showarrow = FALSE 
    ))
fig = subplot(fig1,fig2,shareX = T, shareY = T) %>%  layout(annotations = annotations, title = 'ВИЧ РФ 2005 год')
fig

Rom = geodata::gadm(country="ROU", level = 1, path = "~/Documents/Pyat/test_art5/PM10_Rom")
Rom = as(Rom,"Spatial")
length(Rom@data$NAME_1)

data = readxl::read_excel("~/Documents/Pyat/test_art5/PM10_Rom/Romania_PM10_processed (1).xlsx") %>% 
    filter(!is.na(PM10)) %>% 
    mutate(Judet = case_when(
        Judet == "Arges" ~ "Argeș",
        Judet == "Bacau" ~ "Bacău",
        Judet == "Botosani" ~ "Botoșani",
        Judet == "Braila" ~ "Brăila",
        Judet == "Brasov" ~ "Brașov",
        Judet == "Bucuresti" ~ "Bucharest",
        Judet == "Buzau" ~ "Buzău",
        Judet == "Calarasi" ~ "Călărași",
        Judet == "Constanta" ~ "Constanța",
        Judet == "Galati" ~ "Galați",
        Judet == "Iasi" ~ "Iași",
        Judet == "Judeţul Neamţ" ~ "Neamț",
        Judet == "Mures" ~ "Mureș",
        Judet == "Valcea" ~ "Vâlcea",
        Judet == "Bistriţa-Năsăud" ~ "Bistrița-Năsăud",
        Judet == "Maramureş" ~ "Maramureș",
        Judet == "Timiş" ~ "Timiș",
        TRUE ~ Judet
    ))
data = data %>%  group_by(City, Year, Judet) %>%
    summarize(PM10 = mean(PM10, na.rm = TRUE)) %>%
    ungroup()
data %>% group_by(Year) %>% summarise(num_reg = n())
y.2018 = data %>% filter(Year == 2018) %>% select(-c("City", "Year")) 
y.2021 = data %>% filter(Year == 2021) %>% select(-c("City", "Year"))
notin.2018 = data.frame(Judet = Rom@data$NAME_1[which(!Rom@data$NAME_1 %in% y.2018$Judet)],
                        PM10 = NA)
y.2018 = rbind(y.2018, notin.2018)
colnames(y.2018) = c("Judet","PM10.2018")
notin.2021 = data.frame(Judet = Rom@data$NAME_1[which(!Rom@data$NAME_1 %in% y.2021$Judet)],
                        PM10 = NA)
y.2021 = rbind(y.2021, notin.2021)
colnames(y.2021) = c("Judet","PM10.2021")

Rom@data = inner_join(Rom@data,y.2018, by = c("NAME_1" = "Judet"))
Rom@data = inner_join(Rom@data,y.2021, by = c("NAME_1" = "Judet"))

intersect(unique(data$Judet), Rom@data$NAME_1)
cbind(Rom@data$NAME_1,c(sort(unique(data$Judet)),rep(NA, length(Rom@data$NAME_1)-length(unique(data$Judet)))))
length(unique(data$Judet))


rgdal::writeOGR(Rom, "~/Documents/Pyat/test_art5/PM10_Rom/", layer = "Romania", driver = "ESRI Shapefile", layer_options = "ENCODING=UTF-8")
