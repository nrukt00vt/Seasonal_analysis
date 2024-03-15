
filenames = list.files("newmodel",full.names = T)
library(data.table)
all_data = data.table()

for (i in 1:length(filenames)){
  print(i)
  all_data = rbind(all_data, fread(filenames[i]))
}


all_data$year_gen = substr(all_data$month,1,4)
all_data$month_gen = substr(all_data$month,6,7)

data_2019 = subset(all_data, year_gen == 2019)

data_2020 = subset(all_data, year_gen == 2020)


library(sf)
shapefile = read_sf(dsn="VirginiaCounty",layer="VirginiaCounty")

month_inf_mean_2019 = aggregate(data_2019$inf, by = list(data_2019$month_gen,data_2019$day,data_2019$initial), FUN = mean)



all_data_2 = subset(all_data, day == 50)
month_inf_mean_all = aggregate(all_data_2$inf, by = list(all_data_2$month,all_data_2$day,all_data_2$initial), FUN = mean)
names(month_inf_mean_all) = c("month","day","initial","inf")
library(readxl)
metropolitan_dataset = read_xls("statistical_area_delineations2020.xls")

#create full FIPS code including county and state
metropolitan_dataset$FIPS = paste(metropolitan_dataset$`FIPS State Code`, metropolitan_dataset$`FIPS County Code`, sep= "" )

#subset out variables we care about
metropolitan_subset = metropolitan_dataset[,c("Metropolitan/Micropolitan Statistical Area","FIPS")]
#subset  only counties in Virginia
names(metropolitan_subset) = c("metro","FIPS")
metropolitan_subset_VA = subset(metropolitan_subset,is.element(FIPS,shapefile$STCOFIPS))
#merge urban/rural definitions with virginia shapefile
VA_shp_with_metropolitan = merge(shapefile, metropolitan_subset_VA, by.x = "STCOFIPS", by.y = "FIPS", all.y=FALSE , all.x= TRUE)
VA_shp_with_metropolitan$metro[which(VA_shp_with_metropolitan$metro == "Micropolitan Statistical Area")] = "Rural"
VA_shp_with_metropolitan$metro[which(is.na(VA_shp_with_metropolitan$metro))] = "Rural"
names(VA_shp_with_metropolitan)[10] = "metro"
VA_shp_with_metropolitan_noshp = VA_shp_with_metropolitan[,c("STCOFIPS","metro")]


month_inf_mean_all_sub = subset(month_inf_mean_all, !is.element(month,c("2019_01","2019_02","2019_03","2019_04","2019_05","2019_06","2019_07")))
month_inf_mean_2020_2 = merge(month_inf_mean_all_sub,VA_shp_with_metropolitan_noshp, by.x = "initial",by.y="STCOFIPS")


month_inf_mean_2020_2_sub = subset(month_inf_mean_2020_2,day == 50)
month_inf_mean_2020_2_sub$month_sub=substr(month_inf_mean_2020_2_sub$month,6,7)
monthstoplot = unique(month_inf_mean_2020_2_sub$month_sub)
for (i in 1:length(monthstoplot)){
  subdat = subset(month_inf_mean_2020_2_sub,month_sub == monthstoplot[i])
  subdat = subset(subdat,select = c(initial,inf,metro,month))
  VA_shp_with_metropolitan_w_subdat = merge(VA_shp_with_metropolitan,subdat, by.y = "initial",by.x = "STCOFIPS")
  savegg= ggplot() + geom_sf(data = VA_shp_with_metropolitan_w_subdat, mapping = aes(fill=inf))+ggtitle(monthstoplot[i]) + scale_fill_distiller(palette="YlOrRd", direction = 1, trans= "log10") +
    facet_grid(~month)
ggsave(savegg,filename=paste(monthstoplot[i],".png"))
print(i)
  }

month_inf_mean_2020_2_sub$metro[which(month_inf_mean_2020_2_sub$metro == "Metropolitan Statistical Area")] = "Urban"
month_inf_mean_2020_2_sub$metro = as.factor(month_inf_mean_2020_2_sub$metro)
month_inf_mean_2020_2_sub$metro = relevel(month_inf_mean_2020_2_sub$metro,ref ="Urban")

ggplot() + geom_boxplot(data=month_inf_mean_2020_2_sub,mapping = aes(y=inf,x=month, fill = metro)) +
  scale_x_discrete(name="")+ scale_y_continuous(name = "Number infected")+ scale_fill_brewer(palette="Set1",name="")+
  theme_bw(base_size = 16) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="top")


month_inf_mean_2020_2_sub$pre_post = ""
pre_months = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
month_inf_mean_2020_2_sub$pre_post[is.element(month_inf_mean_2020_2_sub$month,pre_months)] = "pre-pandemic"
month_inf_mean_2020_2_sub$pre_post[!is.element(month_inf_mean_2020_2_sub$month,pre_months)] = "pandemic"


sub_pre = subset(month_inf_mean_2020_2_sub, pre_post == "pre-pandemic")
sub_pan = subset(month_inf_mean_2020_2_sub, pre_post == "pandemic")

avginf_pre = aggregate(sub_pre$inf,by = list(sub_pre$initial), FUN = median)
names(avginf_pre) = c("initial","inf")
avginf_pan = aggregate(sub_pan$inf,by = list(sub_pan$initial), FUN = median)
names(avginf_pan) = c("initial","inf")


avginf_all = 
VA_shp_with_metropolitan_w_datapre = merge(VA_shp_with_metropolitan,avginf_pre, by.y = "initial",by.x = "STCOFIPS")
VA_shp_with_metropolitan_w_datapan = merge(VA_shp_with_metropolitan,avginf_pan, by.y = "initial",by.x = "STCOFIPS")

ggplot() + geom_sf(VA_shp_with_metropolitan_w_datapre, mapping = aes(fill=inf)) + scale_fill_distiller(palette="YlOrRd", direction = 1, trans= "log10") +ggtitle("Inf, pre-pandemic")
ggplot() + geom_sf(VA_shp_with_metropolitan_w_datapan, mapping = aes(fill=inf)) + scale_fill_distiller(palette="YlOrRd", direction = 1, trans= "log10") +ggtitle("Inf, pandemic")
  




month_spread_mean_all = aggregate(all_data_2$num_spread, by = list(all_data_2$month,all_data_2$day,all_data_2$initial), FUN = mean)
names(month_spread_mean_all) = c("month","day","initial","num_spread")

month_spread_mean_all_sub = subset(month_spread_mean_all, !is.element(month,c("2019_01","2019_02","2019_03","2019_04","2019_05","2019_06","2019_07")))
month_spread_mean_2020_2 = merge(month_spread_mean_all_sub,VA_shp_with_metropolitan_noshp, by.x = "initial",by.y="STCOFIPS")


month_spread_mean_2020_2_sub = subset(month_spread_mean_2020_2,day == 50)




ggplot() + geom_boxplot(data=month_spread_mean_2020_2_sub,mapping = aes(y=num_spread,x=month, colour = metro))


month_spread_mean_2020_2_sub$pre_post = ""
pre_months = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
month_spread_mean_2020_2_sub$pre_post[is.element(month_spread_mean_2020_2_sub$month,pre_months)] = "pre-pandemic"
month_spread_mean_2020_2_sub$pre_post[!is.element(month_spread_mean_2020_2_sub$month,pre_months)] = "pandemic"


sub_pre = subset(month_spread_mean_2020_2_sub, pre_post == "pre-pandemic")
sub_pan = subset(month_spread_mean_2020_2_sub, pre_post == "pandemic")

avgspread_pre = aggregate(sub_pre$num_spread,by = list(sub_pre$initial), FUN = median)
names(avgspread_pre) = c("initial","num_spread")
avgspread_pan = aggregate(sub_pan$num_spread,by = list(sub_pan$initial), FUN = median)
names(avgspread_pan) = c("initial","num_spread")
VA_shp_with_metropolitan_w_datapre = merge(VA_shp_with_metropolitan,avgspread_pre, by.y = "initial",by.x = "STCOFIPS")
VA_shp_with_metropolitan_w_datapan = merge(VA_shp_with_metropolitan,avgspread_pan, by.y = "initial",by.x = "STCOFIPS")

ggplot() + geom_sf(VA_shp_with_metropolitan_w_datapre, mapping = aes(fill=num_spread)) + scale_fill_distiller(palette="YlOrRd", direction = 1, trans= "log10")  +ggtitle("Spread, pre-pandemic")
ggplot() + geom_sf(VA_shp_with_metropolitan_w_datapan, mapping = aes(fill=num_spread)) + scale_fill_distiller(palette="YlOrRd", direction = 1, trans= "log10") +ggtitle("Spread, pandemic")





pre_list = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
during_list = c("2020_08","2020_09","2020_10","2020_11","2020_12","2021_01","2021_02")


for (i in 1:length(pre_list)){
  print(i)
  plot_inf_mean_sub_pre = subset(month_inf_mean_2020_2_sub, month == pre_list[i])
  plot_inf_mean_sub_pandemic = subset(month_inf_mean_2020_2_sub, month == during_list[i])
  
  shapefile_pre = merge(VA_shp_with_metropolitan,plot_inf_mean_sub_pre)
  shapefile_pan = merge(VA_shp_with_metropolitan,plot_inf_mean_sub_pandemic)
  shapefile_pre$time = pre_list[i]
  shapefile_pan$time = during_list[i]
  shapefile_all = rbind(shapefile_pre,shapefile_pan)
  
  saveplot = ggplot() + geom_sf(shapefile_all, mapping = aes(fill=inf),size=.5) + scale_fill_distiller(palette = "YlOrRd",direction = 1, trans="log10") + facet_wrap(~time)
ggsave(saveplot,filename=paste0("prepost",i,".png"),width = 7 , height = 2)
  }

month_inf_mean_all_sub_50=subset(month_inf_mean_all_sub,day == 50)

pre_list = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
during_list = c("2020_08","2020_09","2020_10","2020_11","2020_12")
normal_list = c("2021_01","2021_02")

plot_inf_mean_sub_pre = subset(month_inf_mean_all_sub_50, is.element(month,pre_list))
plot_inf_mean_sub_pan = subset(month_inf_mean_all_sub_50, is.element(month,during_list))
plot_inf_mean_sub_post = subset(month_inf_mean_all_sub_50, is.element(month,normal_list))

plot_inf_mean_sub_pre_2 = aggregate(plot_inf_mean_sub_pre$inf, by=list(plot_inf_mean_sub_pre$initial),FUN = mean)
plot_inf_mean_sub_pan_2 = aggregate(plot_inf_mean_sub_pan$inf, by=list(plot_inf_mean_sub_pan$initial),FUN = mean)
plot_inf_mean_sub_post_2 = aggregate(plot_inf_mean_sub_post$inf, by=list(plot_inf_mean_sub_post$initial),FUN = mean)


names(plot_inf_mean_sub_pan_2) = c("initial","inf")
names(plot_inf_mean_sub_pre_2) = c("initial","inf")
names(plot_inf_mean_sub_post_2) = c("initial","inf")
shapefile_pre = merge(VA_shp_with_metropolitan,plot_inf_mean_sub_pre_2, by.x = "STCOFIPS", by.y="initial")
shapefile_pan = merge(VA_shp_with_metropolitan,plot_inf_mean_sub_pan_2, by.x = "STCOFIPS", by.y="initial")
shapefile_post = merge(VA_shp_with_metropolitan,plot_inf_mean_sub_post_2, by.x = "STCOFIPS", by.y="initial")

shapefile_pre$time = "Aug 2019 - Feb 2020"
shapefile_pan$time = "March 2020 - Dec 2020"
shapefile_post$time = "Jan 2021 - Feb 2021"
shapefile_all = rbind(shapefile_pre,shapefile_pan, shapefile_post)
shapefile_all$time = as.factor(shapefile_all$time)
shapefile_all$time = factor(shapefile_all$time,levels=c("Aug 2019 - Feb 2020","March 2020 - Dec 2020","Jan 2021 - Feb 2021"))
saveplot = ggplot() + geom_sf(shapefile_all, mapping = aes(fill=inf),size=.5) + scale_fill_distiller(palette = "YlOrRd",direction = 1, trans="log10",name="# Infected") +theme_void(base_size=16)+ facet_wrap(~time) +theme(legend.position="bottom")
ggsave(saveplot,filename=paste0("avg.png"),width = 18 , height = 4)




month_spread_mean_all_sub_50=subset(month_spread_mean_all_sub,day == 50)

pre_list = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
during_list = c("2020_08","2020_09","2020_10","2020_11","2020_12")
normal_list = c("2021_01","2021_02")

plot_spread_mean_sub_pre = subset(month_spread_mean_all_sub_50, is.element(month,pre_list))
plot_spread_mean_sub_pan = subset(month_spread_mean_all_sub_50, is.element(month,during_list))
plot_spread_mean_sub_post = subset(month_spread_mean_all_sub_50, is.element(month,normal_list))

plot_spread_mean_sub_pre_2 = aggregate(plot_spread_mean_sub_pre$num_spread, by=list(plot_spread_mean_sub_pre$initial),FUN = mean)
plot_spread_mean_sub_pan_2 = aggregate(plot_spread_mean_sub_pan$num_spread, by=list(plot_spread_mean_sub_pan$initial),FUN = mean)
plot_spread_mean_sub_post_2 = aggregate(plot_spread_mean_sub_post$num_spread, by=list(plot_spread_mean_sub_post$initial),FUN = mean)


names(plot_spread_mean_sub_pan_2) = c("initial","spread")
names(plot_spread_mean_sub_pre_2) = c("initial","spread")
names(plot_spread_mean_sub_post_2) = c("initial","spread")
shapefile_pre = merge(VA_shp_with_metropolitan,plot_spread_mean_sub_pre_2, by.x = "STCOFIPS", by.y="initial")
shapefile_pan = merge(VA_shp_with_metropolitan,plot_spread_mean_sub_pan_2, by.x = "STCOFIPS", by.y="initial")
shapefile_post = merge(VA_shp_with_metropolitan,plot_spread_mean_sub_post_2, by.x = "STCOFIPS", by.y="initial")

shapefile_pre$time = "Aug 2019 - Feb 2020"
shapefile_pan$time = "March 2020 - Dec 2020"
shapefile_post$time = "Jan 2021 - Feb 2021"
shapefile_all = rbind(shapefile_pre,shapefile_pan, shapefile_post)
shapefile_all$time = as.factor(shapefile_all$time)
shapefile_all$time = factor(shapefile_all$time,levels=c("Aug 2019 - Feb 2020","March 2020 - Dec 2020","Jan 2021 - Feb 2021"))
saveplot = ggplot() + geom_sf(shapefile_all, mapping = aes(fill=spread),size=.5) + scale_fill_distiller(palette = "BuPu",direction = 1, trans="log10", name = "# counties with >10 cases") +theme_void(base_size=16)+ facet_wrap(~time) +theme(legend.position="bottom")
ggsave(saveplot,filename=paste0("avgspread.png"),width = 18 , height = 4)


ggplot() + geom_sf(shapefile_pre, mapping = aes(fill=inf)) + scale_fill_distiller(palette = "YlOrRd",direction = 1, trans="log10")
ggplot() + geom_sf(shapefile_pan, mapping = aes(fill=inf)) + scale_fill_distiller(palette = "YlOrRd",direction = 1, trans="log10")

ggplot() + geom_boxplot(data=month_inf_mean_2020_2_sub,mapping = aes(y=inf,x=month, colour = metro))
month_inf_mean_2020_2_sub_agg = aggregate(month_inf_mean_2020_2_sub$inf,by=list(month_inf_mean_2020_2_sub$metro,
                                                                                month_inf_mean_2020_2_sub$month), FUN = median)

names(month_inf_mean_2020_2_sub_agg) = c("metro","month","inf")
month_inf_mean_2020_2_sub_agg$pre_post = ""
month_inf_mean_2020_2_sub_agg$pre_post[is.element(month_inf_mean_2020_2_sub_agg$month,pre_months)] = "pre-pandemic"
month_inf_mean_2020_2_sub_agg$pre_post[!is.element(month_inf_mean_2020_2_sub_agg$month,pre_months)] = "pandemic"
library(dplyr)

prepostcomp = pivot_wider(month_inf_mean_2020_2_sub_agg,names_from = metro, values_from = inf)
prepostcomp$comp = prepostcomp$`Metropolitan Statistical Area`/prepostcomp$Rural
ggplot() + geom_boxplot(data = prepostcomp, mapping = aes(x=pre_post,y=comp))


prepostcomp$month = paste0(prepostcomp$month,"_01")
prepostcomp$month = as.Date(prepostcomp$month,"%Y_%m_%d")

ggplot() + geom_line(data = prepostcomp, mapping = aes(x=month,y=comp))


ggplot() + geom_line(data = month_inf_mean_2020_2_sub_agg, mapping = aes(x=month,y=inf,colour=metro,group=metro))




names(month_inf_mean_2019) = c("month","day","FIPS","inf")


smallsub = subset(month_inf_mean_2019, month == "09"& day == 50)

shapefile_plot = merge(VA_shp_with_metropolitan,smallsub,by.x="STCOFIPS",by.y="FIPS")

library(ggplot2)
ggplot() + geom_sf(data = shapefile_plot,mapping = aes(fill=metro))# + scale_fill_distiller(palette="YlOrRd")

ggplot() + geom_boxplot(data=shapefile_plot,mapping = aes(y=inf,x=metro, colour = 'Metropolitan/Micropolitan Statistical Area'))


monthly_means = aggregate(all_data$inf, by = list(all_data$month_gen,all_data$day), FUN = mean)
monthly_means = aggregate(all_data$inf, by = list(all_data$month_gen,all_data$day), FUN = 'quantile', probs=c(5, 50, 95)/100 )

monthly_means = aggregate(all_data$inf, by = list(all_data$month,all_data$day), FUN = 'quantile', probs=c(5, 50, 95)/100 )
per_inf = aggregate(all_data$inf,by=list(all_data$initial,all_data$day),FUN = mean)
names(per_inf) = c("initial","day","inf")
per_inf = subset(per_inf, day == 50)
which(sub_data$inf == max(sub_data$inf))
sub_data = subset(all_data,day == 50)
max(sub_data$inf)
min(sub_data$inf)
ggplot() +geom_boxplot(data = sub_data,mapping = aes(x=month,y=num_spread))


filenames = list.files("specificcounty",full.names = T)
library(data.table)
all_data = data.table()

for (i in 1:length(filenames)){
  print(i)
  all_data = rbind(all_data, fread(filenames[i]))
}

all_data = subset(all_data, select = -c(V1))

library(sf)
library(lubridate)
library(od)
VA_shp=read_sf(dsn="VirginiaCounty",
               layer="VirginiaCounty",stringsAsFactors = F)

relative_move_data = read.csv("mobility/VAmatrix_2019_01.csv",row.names=1,header=T, check.names = F)

populations = read.csv("FIPS_Population.csv",header=F)
names(populations) = c("Name","FIPS", "pop")

VA_shp = merge(VA_shp,populations,by.x="STCOFIPS",by.y="FIPS")
relative_move_data_df = odmatrix_to_od(as.matrix(relative_move_data))
relative_move_data_df$date = as.Date("2019-01-01")
names(relative_move_data_df) = c("from","to","movers","date")


patNames = as.numeric(as.character(unique(relative_move_data_df$to)[order(unique(relative_move_data_df$to))]  ))
patIDs = 1:length(patNames)
pat_locator = data.frame(patNames,patIDs)
pat_locator$patNames=as.numeric(pat_locator$patNames)

all_data_2 = merge(all_data,pat_locator,by.x="initial",by.y="patNames")
all_data_2 = subset( all_data_2, runday == 50)
all_data_2 = subset(all_data_2, select = -c(initial,runday))

library(tidyr)
all_data_3 = all_data_2 %>% pivot_longer(!c(month,patIDs),names_to = "patch_to",values_to="count")
all_data_3$patch_to = substr(all_data_3$patch_to,2,10)
all_data_3$patch_to = as.numeric(all_data_3$patch_to)

all_data_4 = aggregate(all_data_3$count,by=list(all_data_3$month,all_data_3$patIDs,all_data_3$patch_to),FUN = mean)
names(all_data_4) = c("month","pat_start","pat_to", "inf")

pre_list = c("2019_08","2019_09","2019_10","2019_11","2019_12","2020_01","2020_02")
during_list = c("2020_08","2020_09","2020_10","2020_11","2020_12")
normal_list = c("2021_01","2021_02")


all_data_4_pre = subset(all_data_4, is.element(month,pre_list))
all_data_4_pan = subset(all_data_4, is.element(month,during_list))
all_data_4_post = subset(all_data_4, is.element(month,normal_list))

all_data_pre_agg = aggregate(all_data_4_pre$inf,by=list(all_data_4_pre$pat_start,all_data_4_pre$pat_to),FUN = mean)
all_data_pan_agg = aggregate(all_data_4_pan$inf,by=list(all_data_4_pan$pat_start,all_data_4_pan$pat_to),FUN = mean)
all_data_post_agg = aggregate(all_data_4_post$inf,by=list(all_data_4_post$pat_start,all_data_4_post$pat_to),FUN = mean)
names(all_data_pre_agg) = c("pat_start","pat_to", "inf")
names(all_data_pan_agg) = c("pat_start","pat_to", "inf")
names(all_data_post_agg) = c("pat_start","pat_to", "inf")
all_data_pre_agg$inf[which(all_data_pre_agg$inf < .75)] = 0
all_data_pan_agg$inf[which(all_data_pan_agg$inf < .75)] = 0
all_data_post_agg$inf[which(all_data_post_agg$inf < .75)] = 0

all_data_pre_agg$time = "Aug 2019 - Feb 2020"
all_data_pan_agg$time = "March 2020 - Dec 2020"
all_data_post_agg$time = "Jan 2021 - Feb 2021"

VA_shp2 = merge(VA_shp, pat_locator,by.x = "STCOFIPS",by.y="patNames")

shapefile_pre = merge(VA_shp2,all_data_pre_agg,by.x = "patIDs", by.y="pat_to")
shapefile_pan = merge(VA_shp2,all_data_pan_agg,by.x = "patIDs", by.y="pat_to")
shapefile_post = merge(VA_shp2,all_data_post_agg,by.x = "patIDs", by.y="pat_to")
shapefile_all = rbind(shapefile_pre,shapefile_pan, shapefile_post)
shapefile_all$time = as.factor(shapefile_all$time)
shapefile_all$time = factor(shapefile_all$time,levels=c("Aug 2019 - Feb 2020","March 2020 - Dec 2020","Jan 2021 - Feb 2021"))

shapefile_all$pat_target = as.numeric(shapefile_all$pat_start == shapefile_all$patIDs)
shapefile_all$pat_target[shapefile_all$pat_target==0] = NA
shapefile_all$pat_start_name = VA_shp2$Name[shapefile_all$pat_start]
county_labeller <- function(variable,value){
  return(county_names[value])
}
ggplot() + geom_sf(data = shapefile_all, mapping=aes(fill = inf,colour=as.factor(pat_target)),size = 1) +
  scale_fill_distiller(palette="YlOrRd",direction = 1,trans="log10", name = "# infected")+
  scale_colour_manual(values= c("blue"), na.value=NA,guide="none")+ 
  facet_grid(pat_start_name~time )+theme_void(base_size=16)+theme(legend.position="bottom")
pat_start = unique(all_data_pre_agg$pat_start)
VA_shp2 = merge(VA_shp, pat_locator,by.x = "STCOFIPS",by.y="patNames")

shapefile_2 = merge(VA_shp2,all_data_pre_agg,by.x = "patIDs", by.y="pat_to")
shapefile_2$locate = NA
shapefile_2$locate[shapefile_2$patIDs == pat_start] = 1

ggplot() + geom_sf(data = shapefile_2, mapping=aes(fill = inf,colour=as.factor(locate)),size = 1) +
  scale_fill_distiller(palette="YlOrRd",direction = 1,trans="log")+
  scale_colour_manual(values= c("blue"), na.value=NA)+ ggtitle("Pre-pandemic")+
  facet_wrap(~pat_start)


shapefile_3 = merge(VA_shp2,all_data_pan_agg,by.x = "patIDs", by.y="pat_to")
shapefile_3$locate = NA
shapefile_3$locate[shapefile_3$patIDs == pat_start] = 1

ggplot() + geom_sf(data = shapefile_3, mapping=aes(fill = inf,colour=as.factor(locate)),size = 1) +
  scale_fill_distiller(palette="YlOrRd",direction = 1,trans="log")+
  scale_colour_manual(values= c("blue"), na.value=NA)+ggtitle("During pandemic")+
  facet_wrap(~pat_start)