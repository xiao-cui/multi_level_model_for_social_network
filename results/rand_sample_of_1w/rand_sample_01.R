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
#conver to undirected graph
ug<-as.undirected(g, mode="mutual")

#pre-processing 2
#retrieve the biggest connected component
clts<-clusters(ug)
comps<-clts$membership
big_comp_id<-which.max(clts$csize)
bad.vs<-V(ug)[comps!=big_comp_id]
good.vs<-delete.vertices(ug, bad.vs)
ug<-good.vs

#graph analysis
#multi-level community detection
mc<-multilevel.community(ug)
ug$layout<-layout.fruchterman.reingold
V(ug)$size<-degree(ug)
par(mar=c(0,0,0,0))
V(ug)$color=V(ug)$verified_type
V(ug)$color=gsub("Unverified Account", "blue", V(ug)$color)
V(ug)$color=gsub("DaRen", "red", V(ug)$color)
V(ug)$color=gsub("Application Software|Campus|Corporate Account|Government|Media|Organization|Personal Account|Website|Weibo Lady|8", "goldenrod1", V(ug)$color)

plot(ug, vertex.label=NA, edge.arrow.mode=0)

#retrieve articuation points
cutpoint_ids=articulation.points(ug)
V(ug)[cutpoint_ids]$color="deeppink1"

#construct 1 abstraction
V(undirected.g)$size=1
E(undirected.g)$count=1
comm.graph<-contract.vertices(undirected.g, mc$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph<-simplify(comm.graph, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph)$size<-V(comm.graph)$size/10
plot(comm.graph, vertex.label=NA, edge.arrow.mode=0)




