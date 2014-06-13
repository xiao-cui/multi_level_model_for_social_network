#variable g is an igraph object where nodes represent users edges represent connections among users
#users' profiles attached to nodes in the form of attributes.
#attributes include:
#"uid","province","gender",
#"followers_count","friends_count","bilaterals_count", 
#"statuses_count","comments_count","reposts_count",
#"likes_count","verified_type","weibo_age"
as.weighted<-function(x,w1,w2,w3,w4,w5,w6){
    num_of_edges<-ecount(x)
    for (i in 1:num_of_edges) {
    dist_followers_count=abs(V(x)[get.edge(x, i)[1]]$followers_count - V(x)[get.edge(x, i)[2]]$followers_count)
    dist_friends_count=abs(V(x)[get.edge(x, i)[1]]$friends_count - V(x)[get.edge(x, i)[2]]$friends_count)
    dist_bilaterals_count=abs(V(x)[get.edge(x, i)[1]]$bilaterals_count - V(x)[get.edge(x, i)[2]]$bilaterals_count)
    dist_statuses_count=abs(V(x)[get.edge(x, i)[1]]$statuses_count - V(x)[get.edge(x, i)[2]]$statuses_count)
    dist_comments_count=abs(V(x)[get.edge(x, i)[1]]$comments_count - V(x)[get.edge(x, i)[2]]$comments_count)
    dist_reposts_count=abs(V(x)[get.edge(x, i)[1]]$reposts_count - V(x)[get.edge(x, i)[2]]$reposts_count)
    similarity=w1*dist_followers_count+w2*dist_friends_count+w3*dist_bilaterals_count+w4*dist_statuses_count+w5*dist_comments_count+w6*dist_reposts_count
    
    write(c(V(x)[get.edge(x, i)[1]]$name,V(x)[get.edge(x, i)[2]]$name,as.character(similarity)), file="X:\\experiment\\rand_sample_of_1w\\rand_sample_weighted_graph_01", ncolumns=3, append=TRUE)
    }
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
big.undirected.comp<-function(x){
  
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
plot.communities<-function(x){
  wc<-walktrap.community(x, weights=E(x)$weight)
  com<-community.to.membership(x, wc$merges, steps=which.max(wc$modularity)-1)
  V(x)$color<-com$membership+1
  x$layout<-layout.fruchterman.reingold
  V(x)$size<-degree(x)
  par(mar=c(0,0,0,0))
  plot(x, vertex.label=NA, edge.arrow.mode=0)
}