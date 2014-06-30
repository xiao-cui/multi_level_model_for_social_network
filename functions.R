#variable x is an igraph object where nodes represent users edges represent connections among users
#users' profiles attached to nodes in the form of attributes.
#attributes include:
#"uid","province","gender",
#"followers_count","friends_count","bilaterals_count", 
#"statuses_count","comments_count","reposts_count",
#"likes_count","verified_type","weibo_age"
assign.weight<-function(x,w1,w2,w3,w4,w5,w6){
  
    #
    writeTo<-tclvalue(tkgetOpenFile())
  
    #w1, w2, w3, w4, w5, w6 are coefficients of 
    #"followers_count","friends_count","bilaterals_count",
    #"statuses_count","comments_count","reposts_count" respectively
    num_of_edges<-ecount(x)   
    
    for (i in 1:num_of_edges) {
    
    dist_followers_count=abs(V(x)[get.edge(x, i)[1]]$followers_count - V(x)[get.edge(x, i)[2]]$followers_count)
    dist_friends_count=abs(V(x)[get.edge(x, i)[1]]$friends_count - V(x)[get.edge(x, i)[2]]$friends_count)
    dist_bilaterals_count=abs(V(x)[get.edge(x, i)[1]]$bilaterals_count - V(x)[get.edge(x, i)[2]]$bilaterals_count)
    dist_statuses_count=abs(V(x)[get.edge(x, i)[1]]$statuses_count - V(x)[get.edge(x, i)[2]]$statuses_count)
    dist_comments_count=abs(V(x)[get.edge(x, i)[1]]$comments_count - V(x)[get.edge(x, i)[2]]$comments_count)
    dist_reposts_count=abs(V(x)[get.edge(x, i)[1]]$reposts_count - V(x)[get.edge(x, i)[2]]$reposts_count)
    
    #assign weight
    similarity=w1*dist_followers_count+w2*dist_friends_count+w3*dist_bilaterals_count+w4*dist_statuses_count+w5*dist_comments_count+w6*dist_reposts_count
    
    #write edges with their weight line by line
    write(c(V(x)[get.edge(x, i)[1]]$name,V(x)[get.edge(x, i)[2]]$name,as.character(similarity)), file=writeTo, ncolumns=3, append=TRUE)
    
    }
    
    #end for-loop
    #a new file created, instead of original edgelist file, new file contains weighting information as well
}

#min-max normalization
#x is an igraph object as described above
rescaling<-function(x){
    lower_bound.followers_count<-min(V(x)$followers_count)
    upper_bound.followers_count<-max(V(x)$followers_count)
    lower_bound.friends_count<-min(V(x)$friends_count)
    upper_bound.friends_count<-max(V(x)$friends_count)
    lower_bound.bilaterals_count<-min(V(x)$bilaterals_count)
    upper_bound.bilaterals_count<-max(V(x)$bilaterals_count)
    lower_bound.statuses_count<-min(V(x)$statuses_count)
    upper_bound.statuses_count<-max(V(x)$statuses_count)
    lower_bound.comments_count<-min(V(x)$comments_count)
    upper_bound.comments_count<-max(V(x)$comments_count)
    lower_bound.reposts_count<-min(V(x)$reposts_count)
    upper_bound.reposts_count<-max(V(x)$reposts_count)
    num_of_vertex<-length(V(x))
    for (i in 1:num_of_vertex){
        V(x)[i]$followers_count<-(V(x)[i]$followers_count-lower_bound.followers_count)/(upper_bound.followers_count-lower_bound.followers_count)
        V(x)[i]$friends_count<-(V(x)[i]$friends_count-lower_bound.friends_count)/(upper_bound.friends_count-lower_bound.friends_count)
        V(x)[i]$bilaterals_count<-(V(x)[i]$bilaterals_count-lower_bound.bilaterals_count)/(upper_bound.bilaterals_count-lower_bound.bilaterals_count)
        V(x)[i]$statuses_count<-(V(x)[i]$statuses_count-lower_bound.statuses_count)/(upper_bound.statuses_count-lower_bound.statuses_count)
        V(x)[i]$comments_count<-(V(x)[i]$comments_count-lower_bound.comments_count)/(upper_bound.comments_count-lower_bound.comments_count)
        V(x)[i]$reposts_count<-(V(x)[i]$reposts_count-lower_bound.reposts_count)/(upper_bound.reposts_count-lower_bound.reposts_count)
    }
    return (x)
}

#graph pruning (biggest undirected component)
#x is an igraph object as described above
biggest.undirected.comp<-function(x){
  #conver to undirected graph
  ug<-as.undirected(x, mode="mutual")
  #retrieve the biggest connected component
  clts<-clusters(ug)
  comps<-clts$membership
  big_comp_id<-which.max(clts$csize)
  bad.vs<-V(ug)[comps!=big_comp_id]
  good.vs<-delete.vertices(ug, bad.vs)
  ug<-good.vs
  #return a weighted undirected component that is the biggest component of the original graph
  return (ug)
}

#plot communities based on multi-level community detection
#x is an igraph object, must be an undirected graph
plot.multilevel.communities<-function(x){
  mc<-multilevel.community(ug)
  ug$layout<-layout.fruchterman.reingold
  V(ug)$size<-degree(ug)
  par(mar=c(0,0,0,0))
  V(ug)$color=V(ug)$verified_type
  V(ug)$color=gsub("Unverified Account", "blue", V(ug)$color)
  V(ug)$color=gsub("DaRen", "red", V(ug)$color)
  V(ug)$color=gsub("Application Software|Campus|Corporate Account|Government|Media|Organization|Personal Account|Website|Weibo Lady|8", "goldenrod1", V(ug)$color)
  plot(ug, vertex.label=NA, edge.arrow.mode=0)
}

#plot communities based on walktrap algorithm
#x is an igraph object, must be a weighted directed graph
plot.colored.communities<-function(x){
  wc<-walktrap.community(x, weights=E(x)$weight)
  colbar<-rainbow(length(wc))
  x$layout<-layout.lgl(x, area=10*vcount(x)^2)
  V(x)$size<-2
  plot(wc, x, col=colbar[membership(wc)], mark.groups=communities(wc), edge.color=c("black", "red")[crossing(wc,x)+1], vertex.label=NA, edge.arrow.mode=0)
}

#plot communities based on multi-level algorithm
#x is an igraph object, must be a weighted but undirected graph
plot.root.level<-function(x){
  mc<-multilevel.community(x, weights=E(x)$weight)
  colbar<-rainbow(length(mc))
  x$layout<-layout.lgl(x, area=vcount(x)^10)
  V(x)$size<-2
  plot(mc, x, col=colbar[membership(mc)], mark.groups=communities(mc), edge.color=c("black", "red")[crossing(mc,x)+1], vertex.label=NA, edge.arrow.mode=0)
}