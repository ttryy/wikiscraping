library("WikipediR")
library("pageviews")
library("dplyr")
library("visNetwork")
library("htmlwidgets")
library("RColorBrewer")
source("nodes.R")

search <- "Wikipedia"
linkLimit <- 50

articles <- getRecursiveLoop(search, linkLimit, 2)

nodes <- data.frame(id=articles$id, label=articles$label)
data <- data.frame(from=articles$from, to=articles$to)





# Sammel die Views zu allen Artikeln (nodes)
views <- data.frame(name=c(), weekly=c())

for(article in nodes$id){
  tryCatch({view <- article_pageviews(project="de.wikipedia", article=article, platform="all", reformat=TRUE, 
                             start=pageview_timestamps(Sys.Date()-7), end=pageview_timestamps(Sys.Date()))},
           error=function(cond){ view <- data.frame(views=c(0))})
  views <- rbind(views, data.frame(name=c(article),weekly=c(sum(view$views))))
}

views$weekly <- views$weekly/max(views$weekly)







#Färbe die Nodes nach anzahl der Views ein
gradient <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdGy")))
myColors <- gradient(50)

nodes$color <- myColors[ceiling(views$weekly*50)]

view <- article_pageviews(project="de.wikipedia", article=article, platform="all", reformat=TRUE, 
                          start=pageview_timestamps(Sys.Date()-7), end=pageview_timestamps(Sys.Date()))





#Zähle wie oft auf ein Artikel verwiesen wird und setze den Wert als Value
values <- c()

for(id in nodes$id){
  values <- c(values, count(filter(data, data$from == id)))
}

nodes$value <- values







#Bauen des Netzwerkes
widget <- visNetwork(nodes=nodes, edges=data, width=1920, height=1080)%>%
  visPhysics(solver = "repulsion", repulsion=list(nodeDistance="200"))%>%
  visEdges(selfReferenceSize = 0)%>%
  visNodes(font=list(size=18))%>%
  visLayout(randomSeed = 7)
  
saveWidget(widget, file="wikipedia_50_2.html")

print("DONE")
