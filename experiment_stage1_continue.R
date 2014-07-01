#实验阶段1-补充(Pie Chart)
#数据：processed_data下的biggest connected component but weighted by attributes
#实验设计：original multi-level communities -> abstraction 1 -> abstraction 2 （三层结构图）
#补充: Pie Chart for A Single Node


##
## PIE CHART 用户认证类型
##
num_of_comms=42
values<-lapply(1:num_of_comms, function(x) t<-cbind(V(ug)[mc$membership==x]$verified_type))
slices<-lapply(1:num_of_comms, function(x) freq.t<-table(values[[x]]))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_abs_1st_pie_chart_by_verified_types.png", type="cairo", units="in", width=10, height=10, res=300)
par(mar=c(0,0,0,0))
comm.graph$layout<-layout.kamada.kawai(comm.graph, niter=1000)
plot(comm.graph, vertex.shape="pie", vertex.pie=slices, vertex.pie.color=list(rainbow(11, start=0, end=max(1, 10)/11)), vertex.size=15)
dev.off()
#vt<-cbind("DaRen", "Unverified Account", "Application Software", "Campus", "Corporate Account", "Government", "Media", "Organization", "Personal Account", "Website", "Weibo Lady")

##
## PIE CHART 地区
##
num_of_comms=42
values<-lapply(1:num_of_comms, function(x) t<-cbind(V(ug)[mc$membership==x]$province))
slices<-lapply(1:num_of_comms, function(x) freq.t<-table(values[[x]]))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_abs_1st_pie_chart_by_locations.png", type="cairo", units="in", width=10, height=10, res=300)
par(mar=c(0,0,0,0))
comm.graph$layout<-layout.kamada.kawai(comm.graph, niter=1000)
plot(comm.graph, vertex.shape="pie", vertex.pie=slices, vertex.pie.color=list(rainbow(32)), vertex.size=15)
dev.off()

##
## PIE CHART 微博年龄
##
num_of_comms=42
values<-lapply(1:num_of_comms, function(x) t<-cbind(V(ug)[mc$membership==x]$weibo_age))
slices<-lapply(1:num_of_comms, function(x) freq.t<-table(values[[x]]))
png("X:\\experiment\\multi_level_model_for_social_network\\result\\rand_sample_01\\communities_by_followers_abs_1st_pie_chart_by_ages.png", type="cairo", units="in", width=10, height=10, res=300)
par(mar=c(0,0,0,0))
comm.graph$layout<-layout.kamada.kawai(comm.graph, niter=1000)
plot(comm.graph, vertex.shape="pie", vertex.pie=slices, vertex.pie.color=list(rainbow(5)), vertex.size=15)
dev.off()