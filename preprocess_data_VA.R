library(lubridate)
library(od)
# ## 2013 - 2014 data
library(sf)
VA_shp=read_sf(dsn="VirginiaCounty",
               layer="VirginiaCounty",stringsAsFactors = F)


#Read the mobility matrix
#Travis: You will modify this
relative_move_data = read.csv(file_name,row.names=1,header=T, check.names = F)

populations = read.csv("FIPS_Population.csv",header=F)
names(populations) = c("Name","FIPS", "pop")

VA_shp = merge(VA_shp,populations,by.x="STCOFIPS",by.y="FIPS")
relative_move_data_df = odmatrix_to_od(as.matrix(relative_move_data))
relative_move_data_df$date = as.Date("2019-01-01")
names(relative_move_data_df) = c("from","to","movers","date")


patNames = unique(relative_move_data_df$to)[order(unique(relative_move_data_df$to))]  
patIDs = 1:length(patNames)
pat_locator = data.frame(patNames,patIDs)

#add patch names to the movement data
relative_move_data_df = merge(relative_move_data_df,pat_locator,by.x = "from",by.y = "patNames")
names(relative_move_data_df)[which(names(relative_move_data_df) == "patIDs")] = "fr_pat"
relative_move_data_df = merge(relative_move_data_df,pat_locator,by.x = "to",by.y = "patNames")
names(relative_move_data_df)[which(names(relative_move_data_df) == "patIDs")] = "to_pat"

relative_move_data_df$date = ymd(relative_move_data_df$date)
relative_move_data_df$fr_users = 0
relative_move_data_df$to_users = 0

for (i in 1:nrow(relative_move_data_df)){
  
  #print(i)
  sub_to = subset(relative_move_data_df,to == relative_move_data_df$to[i])
  sub_from = subset(relative_move_data_df,from == relative_move_data_df$from[i])
  
  relative_move_data_df$fr_users[i] = sum(sub_from$movers)
  relative_move_data_df$to_users[i] = sum(sub_to$movers)
}

movement_data = relative_move_data_df
movement_data_away = subset(movement_data, to != from)
moversdat = aggregate(movement_data_away$movers,by = list(movement_data_away$to,movement_data_away$date), FUN= sum)

to_users = unique(movement_data[,c("to","to_users")])
names(moversdat) = c("to","date","movers")
moversdat = merge(moversdat,to_users)


all_relative_move_data = subset(all_relative_move_data,orig != dest)
avg_move = aggregate(all_relative_move_data$flow,by=list(all_relative_move_data$dest,all_relative_move_data$date),FUN=sum)
names(avg_move) = c("to","month","movers")
avg_move_2 = aggregate(avg_move$movers,by=list(avg_move$to), FUN = mean)
names(avg_move_2) = c("to","avg_move")
moversdat = merge(moversdat,avg_move_2)


moversdat$frac_move = moversdat$movers/moversdat$avg_move
recrate = 1/10 #daily probability of recovery
exposerate = 2.68/10 # R0 of 2.68, 5.8 days till seeking treatment # How many people a single person potentially infects per day -- can be calculated from R0 estimate if you divide R0 by infectious period
exposepd = 5 # incubation period