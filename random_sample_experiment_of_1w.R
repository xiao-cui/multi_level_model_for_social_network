library(igraph)
#attach users' information to vertices
sample.profile<-read.csv("X:\\experiment\\rand_sample_of_1w\\rand_sample_01", header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#construct a network based on users' connections
sample.graph<-read.csv("X:\\experiment\\rand_sample_of_1w\\rand_sample_graph_01", header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to")
#construct a social graph where nodes represent users
g<-graph.data.frame(sample.graph, vertices=sample.profile)

###################################################################################################################
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#rescaling attribute values to (0, 1]
rg<-rescaling(g)
#construct a social graph where edges are weighted
as.weighted(rg,w1=1,w2=1,w3=1,w4=1,w5=1,w6=10)

#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
#                                                                                                                 #
###################################################################################################################

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
plot(ug, vertex.label=NA, edge.arrow.mode=0)

#retrieve articuation points whose degree greater than 10
big.point.ids<-V(ug)[cutpoint_ids]$size>10
#generate frequency table based on the types of verification
t<-cbind(V(ug)[big.point.ids]$verified_type)
freq.t<-table(t)
par(mar=c(10,10,8,8))
plot(freq.t, cex.axis=0.5, las=2, xlab="verified types", ylab="number of cut points")

#construct 1 abstraction
V(ug)$size=1
E(ug)$count=1
comm.graph<-contract.vertices(ug, mc$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph<-simplify(comm.graph, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph)$size<-V(comm.graph)$size/10
par(mar=c(0,0,0,0))
plot(comm.graph, vertex.label=NA, edge.arrow.mode=0)

#retrieve articuation points
cutpoint_ids=articulation.points(comm.graph)
V(comm.graph)[cutpoint_ids]$color="deeppink1"
plot(comm.graph, vertex.label=NA, edge.arrow.mode=0)

#retrieve articuation points
V(comm.graph)[cutpoint_ids]
#cut points locate within #5 and #35 communities
V(ug)[mc$membership==5]
V(ug)[mc$membership==35]
#community 5
#generate frequency table based on the types of verification
t<-cbind(V(ug)[mc$membership==5]$verified_type)
freq.t<-table(t)
par(mar=c(10,10,8,8))
plot(freq.t, cex.axis=0.5, las=2, xlab="verified types", ylab="number of cut points")
#community 35
#generate frequency table based on the types of verification
t<-cbind(V(ug)[mc$membership==35]$verified_type)
freq.t<-table(t)
par(mar=c(10,10,8,8))
plot(freq.t, cex.axis=0.5, las=2, xlab="verified types", ylab="number of cut points")

#construct 2 abstraction
mcmc<-multilevel.community(comm.graph)
V(comm.graph)$size=1
E(comm.graph)$count=1
comm.comm.graph<-contract.vertices(comm.graph, mcmc$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.comm.graph<-simplify(comm.comm.graph, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.comm.graph)$size<-V(comm.comm.graph)$size
par(mar=c(0,0,0,0))
plot(comm.comm.graph, vertex.label=NA, edge.arrow.mode=0)

#retrieve articuation points
cutpoint_ids=articulation.points(comm.comm.graph)
V(comm.comm.graph)[cutpoint_ids]$color="deeppink1"
plot(comm.comm.graph, vertex.label=NA, edge.arrow.mode=0)






