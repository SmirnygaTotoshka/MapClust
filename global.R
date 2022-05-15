library(leaflet)

monte.carlo.id = "mc"
main.id = "crit"
init.lmbd = 20
p.seq = seq(from = 0,to = 1, by = 0.001)
p <- colorNumeric(colorRampPalette(c("red", "green"))(length(p.seq)),domain = p.seq)
cluster.color = function(num){colorNumeric(colorRampPalette(c("blue", "black"))(length(num)),domain = 1:num)}
