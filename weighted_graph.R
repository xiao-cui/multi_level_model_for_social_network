library(igraph)
library(tcltk)
#attach users' information to vertices
sample.profile<-read.csv("X:\\experiment\\rand_sample_of_1w\\rand_sample_01", header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count","bilaterals_count", "statuses_count","comments_count","reposts_count","likes_count","verified_type","weibo_age")
#construct a network based on users' connections
sample.graph<-read.csv("X:\\experiment\\rand_sample_of_1w\\rand_sample_graph_01", header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to")
#construct a social graph where nodes represent users
g<-graph.data.frame(sample.graph, vertices=sample.profile)

#rescaling attribute values to (0, 1]
rescaled.g<-rescaling(g)
#construct a social graph where edges are weighted
assign.weight(rescaled.g,w1=1,w2=1,w3=1,w4=1,w5=1,w6=10)
#read external file, construct a network based on it
sample.graph<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to","weight")
#re-construct a social graph where nodes represent users and edges are weighted
weighted.g<-graph.data.frame(sample.graph, vertices=sample.profile)
#plot communities based on walktrap algorithm, results are affected through tuning weightings
#sub-step1: decompose graph (min.vertices: supply 2 here to ignore isolate vertices.)
connected.comp.list<-decompose.graph(weighted.g, mode="weak", max.comps=NA, min.vertices=2)
i<-which.max(sapply(connected.comp.list, vcount))
#connected.comp.list is a list of lists, thus, [[]] used as index
comp<-connected.comp.list[[i]]
plot.colored.communities(comp)








#construct undirected graph
undirected.g<-biggest.undirected.comp(weighted.g)