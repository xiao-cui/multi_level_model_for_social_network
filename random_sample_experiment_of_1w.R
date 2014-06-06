library(igraph)
#attach users' information to vertices
sample.profile<-read.csv("/Users/quyao/experiment/rand_sample_of_1w/rand_sample_01", head=F, sep=",")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#construct a network based on users' connections
sample.graph<-read.csv("/Users/quyao/experiment/rand_sample_of_1w/rand_sample_graph_01", head=F, sep=" ")
colnames(sample.graph)<-c("from","to")
#construct a social graph where nodes represent users
g<-graph.data.frame(sample.graph, vertices=sample.profile)

#graph pre-processing

#pre-processing 1
#delete isolated nodes
bad.vs<-V(g)[degree(g)<1]
good.vs<-delete.vertices(g, bad.vs)
g<-good.vs

#pre-processing 2
#retrieve the biggest connected component
clts<-clusters(g, mode="strong")
comps<-clts$membership
big_comp_id<-which.max(clts$csize)
bad.vs<-V(g)[comps!=big_comp_id]
good.vs<-delete.vertices(g, bad.vs)
g<-good.vs

#graph analysis

#community detection
wc<-walktrap.community(g)
com<-community.to.membership(g, wc$merges, steps=which.max(wc$modularity)-1)
V(g)$color<-com$membership+1
g$layout<-layout.fruchterman.reingold
V(g)$size<-degree(g)
par(mar=c(0.1,0.1,0.1,0.1))
plot(g, vertex.label=NA, edge.arrow.mode=0)






