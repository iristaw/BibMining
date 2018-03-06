# create multi_graph
wos.multi_co_network.countries <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  colrs <- c("tomato4", "darkgoldenrod1","blue","orangered","green3","azure2")
  V(net)$color <- colrs[V(net)$continent_type]
  V(net)$size <- degree(net)
  V(net)$label.color <- "black"
  V(net)$label <- NA
  E(net)$width <- (E(net)$weight)/3
  E(net)$edge.color <- "gray80"
  #layout <- layout.sphere
  #graph <- plot(net,layout=layout)
  #legend(x=-1.5, y=-1.1, c("Africa","Asia", "Europe","North America","Oceania","South America"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
  layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
  layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
  par(mfrow=c(2,3), mar=c(1,1,1,1))
  
  for (layout in layouts) {
    
  #print(layout)
    
  l <- do.call(layout, list(net)) 
    
  graph <- plot(net, edge.arrow.mode=0, layout=l, main=layout)
  }
  #legend(x=-1.5, y=-1.1, c("Africa","Asia", "Europe","North America","Oceania","South America"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8,bty="n", ncol=3,xpd=T)#horiz=T
  return(graph)
  dev.off()
}


# create graph
wos.co_network.countries <- function(items,start= NULL,end= NULL,layoutstyle=NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  colrs <- c("tomato4", "darkgoldenrod1","blue","orangered","green3","azure2")
  V(net)$color <- colrs[V(net)$continent_type]
  V(net)$size <- degree(net)
  V(net)$label <- V(net)$country
  V(net)$label.color <- "black"
  E(net)$width <- (E(net)$weight)/3
  E(net)$edge.color <- "gray80"
  par(mfrow=c(1,1))
  #if (!is.null(layout))
  #((layout=layout_as_star)&(layout=layout_components)&(layout=layout_in_circle)
   #   &(layout=layout_nicely)&(layout=layout_on_grid)&(layout=layout.sphere)
    #  &(layout=layout_randomly)&(layout=layout_with_dh)&(layout=layout_with_drl)
     # &(layout=layout_with_fr)&(layout=layout_with_gem)&(layout=layout.graphopt)
      #&(layout=layout_with_kk)&(layout=layout_with_lgl)&(layout=layout_with_mds)) 
   # {
    #layout<-layout}
  graph <- plot(net,vertex.label.cex=0.6)
  if (!is.null(layoutstyle))
  {graph <- plot(net,layout=layoutstyle,vertex.label.cex=0.7)}
  legend(x=-1.5, y=-1.1, c("Africa","Asia", "Europe","North America","Oceania","South America"), pch=21,col="#777777", pt.bg=colrs, pt.cex=2, cex=.8,bty="n", ncol=3,xpd=T)#horiz=T
  return(graph)
  
}


# create heatmap
wos.heatmap.countries <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  netm <- get.adjacency(net, attr="weight", sparse=F)
  colnames(netm) <- V(net)$country
  rownames(netm) <- V(net)$country
  palf <- colorRampPalette(c("gold", "dark orange","orangered"))
  heatmap <- heatmap(netm[,length:1], Rowv = NA, Colv = NA, col = palf(100),scale="none", margins=c(10,10) )
  return(heatmap)
}

#create degree_distribution
wos.degree_distribution.countries <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  deg <- degree(net, mode="all")
  deg.dist <- degree_distribution(net, cumulative=T, mode="all")
  par(mfrow=c(1,1), mar=c(5,5,5,5))
  
  degree.hist <-hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
  degree.dist <- plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency")
  
  return(degree.hist)
  return(degree.dist)
}

#create communities
wos.communities.countries <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  system.time(fc <- fastgreedy.community(net))
  print(membership(fc))
  #plot(fc,net,class="continent_label")
  plot(fc, net)
  #cfg <- cluster_fast_greedy(as.undirected(net))
  #plot <- plot(cfg, as.undirected(net))
  #return(plot)
}

#create net
#create communities
wos.net.countries <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  papersize <- wos.country.freq(countries$country)
  country.continent_type <- wos.country.continent_type(papersize$country)
  country.continent_label <- wos.country.continent_label(country.continent_type$continent_type)
  country.continent_label$continent_type <- as.numeric(country.continent_label$continent_type)
  country.continent_type$continent_type <- as.numeric(country.continent_type$continent_type)
  country1 <- merge(country.continent_type,country.continent_label,by="continent_type")
  info.countries <- merge(country1,papersize,by="country")
  #here obtaining id and sifting nodes
  togethers <- wos.items.togethers(countries)
  agg1 <- wos.items.final(togethers)
  agg <- wos.items.3cols_2(agg1)
  agg_country <- as.data.frame(agg[,1],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- as.data.frame(agg_country[!duplicated(agg_country),],stringsAsFactors = FALSE)
  colnames(agg_country) <- c("country")
  agg_country <- agg_country[order(agg_country$country),]
  agg_country <- as.data.frame(agg_country,stringsAsFactors = FALSE)
  length <- lengths(agg_country)
  id<- wos.countries.ids(c(1:length),flag = "S")
  agg_country_id <- cbind(id,agg_country)
  
  agg_country_id1 <- merge(agg_country_id,agg1,by.x="agg_country",by.y="country1")
  colnames(agg_country_id1) <- c("country1","id1","country2","Times")
  agg_country_id1_id2 <- merge(agg_country_id,agg_country_id1,by.x="agg_country",by.y="country2")
  colnames(agg_country_id1_id2) <- c("country1","id1","country2","id2","Times")
  edges <- agg_country_id1_id2[,c(2,4,5)]
  colnames(edges) <- c("id1","id2","weight")
  #create edges
  colnames(agg_country_id) <- c("id","country")
  info.countries <- cbind(id,info.countries[,1:ncol(info.countries)])
  nodes <- merge(agg_country_id,info.countries,by="country")
  nodes <- nodes[,c(2,1,4,5,6)]
  nodes <- nodes[!duplicated(nodes),]
  colnames(nodes) <- c("id","country","continent_type","continent_label","papersize")
  #create nodes
  library(igraph)
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
  return(net)
}


