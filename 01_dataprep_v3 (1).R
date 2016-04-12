# packages
library("data.table")

# memória kiürítése
rm(list=ls())

# hány órára kerekítsen
hour_to_round <- 6

# working directory megadása
working_directory <- 'C:/Bubi_challenge/Tibi'
setwd(working_directory)

###############
#### TRAIN ####
###############
# adatok beolvasás
train <- fread('train.csv', 
               stringsAsFactors = FALSE,
               colClasses       = c("int","character","character","character","character"))

# melyik nap? melyik útvonal
train$day   <- substr(train$start_time,1,10)

train$usage <- 1
train       <- train[,.(usage = sum(usage)),
                     by       = .(day,
                                  start_location, 
                                  end_location)]

train_aggr      <- train[,.(usage_total = sum(usage)), by = .(day)]
train           <- merge(train, train_aggr, by = c("day"), all.x = T)
train$usage_pct <- train$usage / train$usage_total
train           <- subset(train, select = -c(usage_total))
rm(train_aggr)

##################
#### WEATHER #####
##################

# wspdm - 2 db -9999
# ha wdire = varaiable, akkor wdird 0
# vism-ben és a windchillm-ben sok a missinges -> egyelõre kihagyom

weather <- fread('weather_data.csv', stringsAsFactors = FALSE)

# aggregálás 6 óránként
weather$day        <- substr(weather$time,1,10)
weather$hour_range <- floor(as.numeric(substr(weather$time,12,13)) / hour_to_round)*hour_to_round
weather$wspdm      <- ifelse(weather$wspdm != -9999, weather$wspdm, NA)

attach(weather)
weather$tempm_0  <- ifelse(hour_range ==  0, tempm, NA)
weather$tempm_6  <- ifelse(hour_range ==  6, tempm, NA)
weather$tempm_12 <- ifelse(hour_range == 12, tempm, NA)
weather$tempm_18 <- ifelse(hour_range == 18, tempm, NA)

weather$wspdm_0  <- ifelse(hour_range ==  0, wspdm, NA)
weather$wspdm_6  <- ifelse(hour_range ==  6, wspdm, NA)
weather$wspdm_12 <- ifelse(hour_range == 12, wspdm, NA)
weather$wspdm_18 <- ifelse(hour_range == 18, wspdm, NA)

weather$rain_0  <- ifelse(hour_range ==  0, rain, NA)
weather$rain_6  <- ifelse(hour_range ==  6, rain, NA)
weather$rain_12 <- ifelse(hour_range == 12, rain, NA)
weather$rain_18 <- ifelse(hour_range == 18, rain, NA)

weather$fog_0  <- ifelse(hour_range ==  0, fog, NA)
weather$fog_6  <- ifelse(hour_range ==  6, fog, NA)
# weather$fog_12 <- ifelse(hour_range == 12, fog, NA)
# weather$fog_18 <- ifelse(hour_range == 18, fog, NA)

# weather$snow_0  <- ifelse(hour_range ==  0, snow, NA)
# weather$snow_6  <- ifelse(hour_range ==  6, snow, NA)
# weather$snow_12 <- ifelse(hour_range == 12, snow, NA)
# weather$snow_18 <- ifelse(hour_range == 18, snow, NA)

# weather$thunder_0  <- ifelse(hour_range ==  0, thunder, NA)
# weather$thunder_6  <- ifelse(hour_range ==  6, thunder, NA)
# weather$thunder_12 <- ifelse(hour_range == 12, thunder, NA)
# weather$thunder_18 <- ifelse(hour_range == 18, thunder, NA)

detach(weather)

weather_part_1 <- weather[,.(tempm_avg_0   = mean(  tempm_0  , na.rm=T),
                             tempm_avg_6   = mean(  tempm_6  , na.rm=T),
                             tempm_avg_12  = mean(  tempm_12 , na.rm=T),
                             tempm_avg_18  = mean(  tempm_18 , na.rm=T),
                             
                             tempm_med_0   = median( tempm_0 , na.rm=T),
                             tempm_med_6   = median( tempm_6 , na.rm=T),
                             tempm_med_12  = median( tempm_12, na.rm=T),
                             tempm_med_18  = median( tempm_18, na.rm=T),
                             
                             wspdm_avg_0   = mean(   wspdm_0 , na.rm=T),
                             wspdm_avg_6   = mean(   wspdm_6 , na.rm=T),
                             wspdm_avg_12  = mean(   wspdm_12, na.rm=T),
                             wspdm_avg_18  = mean(   wspdm_18, na.rm=T),
                             
                             rain_0        = max(rain_0      , na.rm=T),
                             rain_6        = max(rain_6      , na.rm=T),
                             rain_12       = max(rain_12     , na.rm=T),
                             rain_18       = max(rain_18     , na.rm=T),
                             
                             fog_0         = max(fog_0       , na.rm=T),
                             fog_6         = max(fog_6       , na.rm=T)),
                          #                            fog_12        = max(fog_12      , na.rm=T),
                          #                            fog_18        = max(fog_18      , na.rm=T),
                          #                            
                          #                            thunder_0     = max(thunder_0   , na.rm=T),
                          #                            thunder_6     = max(thunder_6   , na.rm=T),
                          #                            thunder_12    = max(thunder_12  , na.rm=T),
                          #                            thunder_18    = max(thunder_18  , na.rm=T)
                          by         = .(day)]


weather_part_2 <- weather[,.(tempm_avg      = mean(   tempm),
                             tempm_med      = median( tempm),
                             wspdm_avg      = mean(   wspdm, na.rm=T),
                             hum_avg        = mean(   hum  ),
                             wdird_avg      = mean(   wdird),
                             pressurem_avg  = mean(   pressurem),
                             fog            = max(    fog),
                             rain           = max(    rain),
                             thunder        = max(    thunder)),
                          by         = .(day)]

weather <- merge(weather_part_1,
                 weather_part_2,
                 by = c("day"))

rm(weather_part_1, weather_part_2)

# train vagy test nap?
weather$flag_train <- (!(weather$day %in% c("2015-04-02","2015-04-04","2015-04-06","2015-04-08",
                                            "2015-04-10","2015-04-12","2015-04-14","2015-04-16",
                                            "2015-04-18","2015-04-20","2015-04-22","2015-04-24",
                                            "2015-04-26","2015-04-28","2015-04-30","2015-05-02",
                                            "2015-05-04","2015-05-06","2015-05-08","2015-05-10",
                                            "2015-05-12","2015-05-14","2015-05-16","2015-05-18",
                                            "2015-05-20","2015-05-22","2015-05-24","2015-05-26",
                                            "2015-05-28","2015-05-30")))

weather$not_working_day <- (!(weather$day %in% c("2015-01-01","2015-01-02","2015-01-03",
                                                 "2015-01-04","2015-01-11","2015-01-17",
                                                 "2015-01-18","2015-01-24","2015-01-25",
                                                 "2015-01-31","2015-02-01","2015-02-07",
                                                 "2015-02-08","2015-02-14","2015-02-15",
                                                 "2015-02-21","2015-02-22","2015-02-28",
                                                 "2015-03-01","2015-03-07","2015-03-08",
                                                 "2015-03-14","2015-03-15","2015-03-21",
                                                 "2015-03-22","2015-03-28","2015-03-29",
                                                 "2015-04-04","2015-04-05","2015-04-06",
                                                 "2015-04-11","2015-04-12","2015-04-18",
                                                 "2015-04-19","2015-04-25","2015-04-26",
                                                 "2015-05-01","2015-05-02","2015-05-03",
                                                 "2015-05-09","2015-05-10","2015-05-16",
                                                 "2015-05-17","2015-05-23","2015-05-24",
                                                 "2015-05-25","2015-05-30","2015-05-31")))

# day of the week
weather$weekday <- as.factor(weekdays(as.Date(weather$day)))

## mintavétel
to_select_days       <- weather[flag_train == T,.(day)]
to_select_days$month <- as.numeric(substr(to_select_days$day,6,7))

set.seed(42)
days_from_m1 <- to_select_days[month == 1,]
days_from_m2 <- to_select_days[month == 2,]
days_from_m3 <- to_select_days[month == 3,]
days_from_m4 <- to_select_days[month == 4,]
days_from_m5 <- to_select_days[month == 5,]

days_from_m1 <- days_from_m1[sample(nrow(days_from_m1), size = 3, replace = F),]
days_from_m2 <- days_from_m2[sample(nrow(days_from_m2), size = 3, replace = F),]
days_from_m3 <- days_from_m3[sample(nrow(days_from_m3), size = 3, replace = F),]
days_from_m4 <- days_from_m4[sample(nrow(days_from_m4), size = 3, replace = F),]
days_from_m5 <- days_from_m5[sample(nrow(days_from_m5), size = 3, replace = F),]

days <- c(days_from_m1$day,days_from_m2$day,days_from_m3$day,days_from_m4$day,days_from_m5$day)

weather$flag_own_test <- F
weather[day %in% days, flag_own_test := T]

rm(to_select_days, days_from_m1, days_from_m2, days_from_m3, days_from_m4, days_from_m5, days)


#################
#### STATION ####
#################

station    <- fread('station_data.csv', 
                    colClasses       = c("character","character","numeric","numeric","numeric","character","character"),
                    stringsAsFactors = FALSE)
station    <- unique(station[,.(place_id, place_name)])
station$id <- 1
station    <- merge(station,
                    station,
                    by = c("id"),
                    all = T,
                    allow.cartesian = T)
station    <- station[,.(place_id.x,place_id.y,place_name.x,place_name.y)]

setnames(station,"place_id.x"  ,"s_place_id")
setnames(station,"place_id.y"  ,"e_place_id")
setnames(station,"place_name.x","s_place_name")
setnames(station,"place_name.y","e_place_name")


#######################
#### Adatok kötése ####
#######################

weather$id <- 1
station$id <- 1

# weather & station
full_set <- merge(weather,
                  station,
                  by = c("id"),
                  all = T,
                  allow.cartesian = T)

# full_set & train
setnames(train,"start_location","s_place_id")
setnames(train,  "end_location","e_place_id")

full_set <- merge(full_set,
                  train,
                  by = c("day","s_place_id","e_place_id"),
                  all.x = T)

full_set$usage     <- ifelse(is.na(full_set$usage    ) == T & full_set$flag_train == T, 0, full_set$usage    )
full_set$usage_pct <- ifelse(is.na(full_set$usage_pct) == T & full_set$flag_train == T, 0, full_set$usage_pct)



full_set <- full_set[,.(day,
                        s_place_id,
                        e_place_id,
                        s_place_name,
                        e_place_name,
                        usage,
                        usage_pct,
                        flag_train,
                        flag_own_test,
                        weekday,
                        not_working_day,
                        tempm_avg_0,
                        tempm_avg_6,
                        tempm_avg_12,
                        tempm_avg_18,
                        tempm_avg,
                        tempm_med_0,
                        tempm_med_6,
                        tempm_med_12,
                        tempm_med_18,
                        tempm_med,
                        rain_0,
                        rain_6,
                        rain_12,
                        rain_18,
                        rain,
                        fog_0,
                        fog_6,
                        fog,
                        thunder,
                        wspdm_avg_0,
                        wspdm_avg_6,
                        wspdm_avg_12,
                        wspdm_avg_18,
                        wspdm_avg,
                        hum_avg,
                        wdird_avg,
                        pressurem_avg)]

setkey(full_set, day, s_place_id, e_place_id)

rm(station, train, weather, hour_to_round)

#### Mentés ####
save(full_set, file="C:/Bubi_challenge/Tibi/full_set_byday.csv")
