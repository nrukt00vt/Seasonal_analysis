rm(list=ls())
library(data.table)
library(lubridate)
library(ggplot2)
library(od)

source("bearmod_fx.R")
file_names_orig = list.files(path = "mobility", full.names = T)
all_data = data.frame()
file_names_orig2 = file_names_orig[13:27]
all_relative_move_data=data.frame()
for (i in 1:length(file_names_orig2)){
  
  relative_move_data = read.csv(file_names_orig2[i],row.names=1,header=T, check.names = F)
  
  relative_move_data_df = odmatrix_to_od(as.matrix(relative_move_data))
  relative_move_data_df$date = i
  all_relative_move_data = rbind(all_relative_move_data,relative_move_data_df)
}
file_names_orig = file_names_orig#[13:27]
file_names = file_names_orig[1:2]
#file_names = file_names_orig[3:4]
#file_names = file_names_orig[5:6]
#file_names = file_names_orig[7:8]
#file_names = file_names_orig[9:10]
#file_names = file_names_orig[11:12]
#file_names = file_names_orig[7:9]
#file_names = file_names_orig[10:12]
for (file_name in file_names){

  setwd("H:/My Drive/travisruiqing")
  print(file_name)
  month_name = substr(file_name, 19,25)
source("preprocess_data_VA.R")
#Initial parameters

NPat = length(patNames)
patnInf = rep(0,NPat)
patnExp = c(rep(0,NPat) )
#### Running the model  ####
#Change out the FIPS code used here
#Be sure to run both lines each time
for (name in c(51003, 51197, 51083, 51059)){
  patnInf = rep(0,NPat)
patnInf[which(patNames == name)] = 10
print(name)

#Initializing the population

pat_locator_full = merge(pat_locator,st_drop_geometry(VA_shp)[,c("STCOFIPS","pop")],by.x="patNames",by.y="STCOFIPS")
recover_df = data.frame(date = seq(from=min(movement_data$date),to=max(movement_data$date),by="days"),recrate = recrate)
HPop = InitiatePop(pat_locator_full,patnInf,patnExp)



#Creating a number of days to run over (here, 40 days)
input_dates = rep("2019-01-01",76)

#Run the simulation
all_data=data.table()

for (run in 1:50){
  HPop_update = runSim(HPop,pat_locator_full,relative_move_data,movement_data, input_dates,recover_df, exposerate,exposepd,exposed_pop_inf_prop = 0, TSinday = 1, prob_move_per_TS= 0, relative_trans_df = moversdat)
  
  sim_data_end = data.frame(month = month_name, initial = name, subset(HPop_update$all_spread_today,select=-c(dates)))
  
  all_data = rbind(all_data,sim_data_end)
}

}}
write.csv(all_data,"G:/My Drive/travisruiqing/varymonth_numspread_inf_iter6new2.csv")


all_data$month_gen = substr(all_data$month,6,7)
monthly_means = aggregate(all_data$inf, by = list(all_data$month_gen,all_data$day), FUN = mean)
monthly_means = aggregate(all_data$inf, by = list(all_data$month_gen,all_data$day), FUN = 'quantile', probs=c(5, 50, 95)/100 )

names(monthly_means)= c("Month","day","x")
monthly_means$q5 = monthly_means$x[,1]
monthly_means$q50 = monthly_means$x[,2]
monthly_means$q95 = monthly_means$x[,3]
ggplot( ) +  geom_ribbon(data=monthly_means,mapping = aes(x=day,ymax = q95, ymin = q5,fill = Month), alpha=.25)

stat_summary(geom = "line", fun = mean) +
  stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                alpha = 0.5, linetype="dashed", colour="red")

geom_line(data = all_data, mapping = aes(x = day, y = inf, colour = month))

ggplot() + geom_ribbon(data = all_data, mapping = aes(ymin = inf - std,
                ymax = value + std,
                group=group),
            fill = "steelblue2")
all_data = read.csv("varymonth.csv")
library(ggplot2)
ggplot() + geom_line(data = all_data, mapping = aes(x = day, y = inf, colour = month))


all_data$month_only = substr(all_data$month,6,7)
month_ids = unique(all_data$month_only)
for (i in 1:length(month_ids)){
  all_data_focus = subset(all_data,month_only == month_ids[i])
  all_data_unfocus = subset(all_data,month_only != month_ids[i])
  ggplot() + geom_line(data = all_data_unfocus, mapping = aes(x = day, y = inf,group=month),colour="grey") +
    geom_line(data = all_data_focus, mapping = aes(x = day, y = inf,group=month),colour="red")+ ggtitle(i)
  
}
ggplot() + geom_line(data = all_data, mapping = aes(x = day, y = inf, colour = month_only,group=month))



all_data_end = subset(all_data, day == 40)
all_data_end$date = as.Date(paste(all_data_end$month, "_01", sep=""), format = c("%Y_%m_%d"))
ggplot() + geom_line(data = all_data_end, mapping = aes(x = date, y = inf)) +scale_x_date()
VA_shp_with_run_data = merge(VA_shp, all_data, by.x = "STCOFIPS", by.y = "FIPS_start")

VA_shp_with_run_data$fraction_infected = VA_shp_with_run_data$total_infected_end / VA_shp_with_run_data$pop


ggplot() + 
  geom_sf(data=VA_shp_with_run_data, mapping = aes(), fill = "light grey" )+
  geom_sf(data=VA_shp_with_run_data, mapping = aes(fill = total_infected_end) )+
  
  
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans="log10")
  #Plot the results
  HPop_update$epidemic_curve$day_number = 1:nrow(HPop_update$epidemic_curve)
#plot the epidemic curve
  ggplot() + geom_line(data = HPop_update$epidemic_curve, mapping = aes(x=day_number, y = inf))
  
#merge the total infected & number initially infected into the shapefile
  output_data = data.frame(FIPS = VA_shp$STCOFIPS,initial_inf = 0, total_infected = 0)
    for (i in 1:nrow(output_data)){
    output_data$initial_inf[i] = HPop_update$HPop$nInitialInf[which(HPop_update$HPop$names == output_data$FIPS[i])]
    output_data$total_infected[i]= HPop_update$HPop$nInf[which(HPop_update$HPop$names == output_data$FIPS[i])] +
                                    HPop_update$HPop$nRec[which(HPop_update$HPop$names == output_data$FIPS[i])]
  }
  
  VA_shp_with_run_data = merge(VA_shp, output_data, by.x = "STCOFIPS", by.y = "FIPS")

  VA_shp_with_run_data$fraction_infected = VA_shp_with_run_data$total_infected / VA_shp_with_run_data$pop
  
#Plot the number of people infected in total
  #Highlight the county where the infection started in red

  ggplot() + 
    geom_sf(data=VA_shp_with_run_data, mapping = aes(), fill = "light grey" )+
    geom_sf(data=subset(VA_shp_with_run_data,total_infected > 5), mapping = aes(fill = total_infected) )+
    geom_sf(data=subset(VA_shp_with_run_data,initial_inf > 0), mapping = aes() , fill = NA, colour = "red") +
  
   scale_fill_distiller(palette = "YlOrRd", direction = 1)
  
  