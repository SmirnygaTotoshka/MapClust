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

