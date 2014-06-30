#实验阶段1
#数据：processed_data下的biggest connected component but weighted by attributes
#实验设计：original multi-level communities -> abstraction 1 -> abstraction 2 （三层结构图）
#其他：一些针对特定用户属性的分析，比如，北京的达人用户的3层结构


###############################
#     导入所需的外部文件      #
#                             #
library(igraph)
library(tcltk)
source("functions.R")
#                             #
#                             #
###############################


###############################################################
#                    创建igraph object                        #
#            sample.profile从rand_sample_of_1w中取            #
#              sample.graph从processed_data中取               #
sample.profile<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=",", fileEncoding="UTF-8")
colnames(sample.profile)<-c("uid","province","gender","followers_count","friends_count",
                            "bilaterals_count", "statuses_count","comments_count","reposts_count",
                            "likes_count","verified_type","weibo_age")
sample.graph<-read.csv(tclvalue(tkgetOpenFile()), header=F, sep=" ", fileEncoding="UTF-8")
colnames(sample.graph)<-c("from","to","weight")
wg<-graph.data.frame(sample.graph, vertices=sample.profile)
#                                                             #
#                                                             #
###############################################################




###############################################################
#                获取biggest connected component              #
#                 并转化为undirected graph                    #
connected.comp.list<-decompose.graph(wg, mode="weak", max.comps=NA, min.vertices=2)
i<-which.max(sapply(connected.comp.list, vcount))
comp<-connected.comp.list[[i]]
ug<-biggest.undirected.comp(comp)
#                                                             #
#                                                             #
###############################################################



##########################
#                        #
#      ROOT LEVEL        #
#                        #
##########################
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_new.png", type="cairo", units="in", width=10, height=10, res=300)
mc<-multilevel.community(ug, weights=E(ug)$weight)
colbar<-rainbow(length(mc))
lay<-layout.fruchterman.reingold(ug)
V(ug)$size<-2
par(mar=c(0,0,0,0))
plot(mc, ug, layout=lay, col=colbar[membership(mc)], mark.groups=communities(mc), edge.color=c("black", "red")[crossing(mc,ug)+1], vertex.label=NA, edge.arrow.mode=0)
dev.off()


##########################
#                        #
#        ABS 1           #
#                        #
##########################
V(ug)$size=1
E(ug)$count=1
comm.graph<-contract.vertices(ug, mc$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph<-simplify(comm.graph, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph)$size<-V(comm.graph)$size/10
comm.graph$layout<-layout.kamada.kawai(comm.graph, niter=1000)
par(mar=c(0,0,0,0))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_abs_1st.png", type="cairo", units="in", width=10, height=10, res=300)
plot(comm.graph, vertex.label=NA, edge.arrow.mode=0)
dev.off()


##########################
#                        #
#        ABS 2           #
#                        #
##########################
mc2<-multilevel.community(comm.graph)
V(comm.graph)$size=1
E(comm.graph)$count=1
comm.graph2<-contract.vertices(comm.graph, mc2$membership, vertex.attr.comb=list(size="sum","ignore"))
comm.graph2<-simplify(comm.graph2, remove.loops=TRUE, edge.attr.comb=list(count="sum","ignore"))
V(comm.graph2)$size<-V(comm.graph2)$size
comm.graph2$layout<-layout.kamada.kawai(comm.graph2, niter=1000)
par(mar=c(0,0,0,0))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_abs_2nd.png", type="cairo", units="in", width=10, height=10, res=300)
plot(comm.graph2, vertex.label=NA, edge.arrow.mode=0)
dev.off()

