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

fig = plot_ly(hiv.res.2005$Crit_Down, x = ~Size, y = ~S.crit,type = 'scatter', mode = 'lines+markers',color = I("black"),name = "Критическая область") %>%
    layout(xaxis = list(title="Размер"),yaxis = list(title="Статистика"),title="Кластеры")
if (!is.null(hiv.res.2005$Clusters)){
    fig = fig %>%
        add_trace(data = hiv.res.2005$Clusters,x = ~N,y = ~S, name = 'Все', mode = 'markers',color = I("green"), size = 3, text = ~Label)%>%
        add_trace(data = hiv.res.2005$Clusters %>% filter(S > S.crit),x = ~N,y = ~S, name = 'Значимые', mode = 'markers',color = I("blue"), size = 3, text = ~Label)
}
fig
