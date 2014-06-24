library(igraph)
library(tcltk)
source("functions.R")
#attach users' information to vertices
sample.profile<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#construct a network based on users' connections
sample.graph<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to")
#construct a social graph where nodes represent users
g<-graph.data.frame(sample.graph, vertices=sample.profile)

#decompose graph (min.vertices: supply 2 here to ignore isolate vertices.)
connected.comp.list<-decompose.graph(g, mode="weak", max.comps=NA, min.vertices=2)
i<-which.max(sapply(connected.comp.list, vcount))
#comp is the biggest connected component in the original graph 'g'
comp<-connected.comp.list[[i]]

#rescaling attribute values to (0, 1]
rescaled.comp<-rescaling(comp)
#construct a social graph where edges are weighted
assign.weight(rescaled.comp,w1=10,w2=1,w3=1,w4=1,w5=1,w6=1)
assign.weight(rescaled.comp,w1=1,w2=10,w3=1,w4=1,w5=1,w6=1)
assign.weight(rescaled.comp,w1=1,w2=1,w3=10,w4=1,w5=1,w6=1)
assign.weight(rescaled.comp,w1=1,w2=1,w3=1,w4=10,w5=1,w6=1)
assign.weight(rescaled.comp,w1=1,w2=1,w3=1,w4=1,w5=10,w6=1)
assign.weight(rescaled.comp,w1=1,w2=1,w3=1,w4=1,w5=1,w6=10)

#construct biggest connected component
#vertex+edges
sample.profile<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#using weighted graph from 'processed_data'
sample.graph<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to","weight")
wg<-graph.data.frame(sample.graph, vertices=sample.profile)
#decompose graph (min.vertices: supply 2 here to ignore isolate vertices.)
connected.comp.list<-decompose.graph(wg, mode="weak", max.comps=NA, min.vertices=2)
i<-which.max(sapply(connected.comp.list, vcount))
#comp is the biggest connected component in the original graph 'g'
comp<-connected.comp.list[[i]]
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_03\\communities_by_comments.png", type="cairo", units="in", width=10, height=10, res=300)
plot.colored.communities(comp)
dev.off()

#construct undirected graph
ug<-biggest.undirected.comp(comp)

#construct 1 abstraction
mc<-multilevel.community(ug)
V(ug)$size=1
E(ug)$count=1
comm.graph<-contract.vertices(ug, mc$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph<-simplify(comm.graph, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph)$size<-V(comm.graph)$size/10
comm.graph$layout<-layout.fruchterman.reingold(comm.graph, area=vcount(comm.graph)^2)
par(mar=c(0,0,0,0))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_03\\communities_by_comments_abs_1st.png", type="cairo", units="in", width=10, height=10, res=300)
plot(comm.graph, vertex.label=NA, edge.arrow.mode=0)
dev.off()

#construct 2 abstraction
mc2<-multilevel.community(comm.graph)
V(comm.graph)$size=1
E(comm.graph)$count=1
comm.graph2<-contract.vertices(comm.graph, mc2$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph2<-simplify(comm.graph2, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph2)$size<-V(comm.graph2)$size
comm.graph2$layout<-layout.fruchterman.reingold(comm.graph2, area=vcount(comm.graph)^2)
par(mar=c(0,0,0,0))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_03\\communities_by_comments_abs_2nd.png", type="cairo", units="in", width=10, height=10, res=300)
plot(comm.graph2, vertex.label=NA, edge.arrow.mode=0)
dev.off()

#construct 3 abstraction
mc3<-multilevel.community(com.graph2)
V(comm.graph2)$size=1
E(comm.graph2)$count=1
comm.graph3<-contract.vertices(comm.graph2, mc3$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph3<-simplify(comm.graph3, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph3)$size<-V(comm.graph3)$size
comm.graph3$layout<-layout.fruchterman.reingold(comm.graph3, area=vcount(comm.graph)^2)
par(mar=c(0,0,0,0))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_02\\communities_by_followers_abs_3rd.png", type="cairo", units="in", width=10, height=10, res=300)
plot(comm.graph3, vertex.label=NA, edge.arrow.mode=0)
dev.off()

#从sample.graph中抽取某一特定类型的用户，instead of the biggest connected component

#attach users' information to vertices
sample.profile<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#construct a network based on users' connections
sample.graph<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to")
#construct a social graph where nodes represent users
g<-graph.data.frame(sample.graph, vertices=sample.profile)

#抽取北京的达人用户
bad.vs<-V(g)[V(g)$province!="北京"]
good.vs<-delete.vertices(g, bad.vs)
g<-good.vs
bad.vs<-V(g)[V(g)$verified_type!="DaRen"]
good.vs<-delete.vertices(g, bad.vs)
g<-good.vs

#retrieve the biggest connected component
#decompose graph (min.vertices: supply 2 here to ignore isolate vertices.)
connected.comp.list<-decompose.graph(g, mode="weak", max.comps=NA, min.vertices=2)
i<-which.max(sapply(connected.comp.list, vcount))
#comp is the biggest connected component in the original graph 'g'
comp<-connected.comp.list[[i]]
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\按属性抽取用户\\communities_上海达人用户.png", type="cairo", units="in", width=10, height=10, res=300)
plot.colored.communities(comp)
dev.off()



