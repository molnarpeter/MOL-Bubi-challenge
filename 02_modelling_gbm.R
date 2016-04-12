# packages
library("data.table")
library("caret")

# memória kiürítése
rm(list=ls())

no_top_routes <- 150

# working directory megadása
working_directory <- 'C:/Bubi_challenge/Tibi'
setwd(working_directory)
save_dir <- "C:/Bubi_challenge/Tibi"

# IDCG calc
#source('C:/Bubi_challenge/Tibi')

# beolvasás
load("full_set_byday")

# months
full_set$month <- as.character(substr(full_set$day,6,7))

# train és test set
train <- full_set[flag_train == T & as.numeric(substr(full_set$day,6,7))>=2,]
test  <- full_set[flag_train == F,]

# route
train$route <- paste0(train$s_place_id,"-",train$e_place_id)
test$route  <- paste0(test$s_place_id ,"-", test$e_place_id)

# a legjobb útvonalak
top_routes <- train[,.(usage = sum(usage)),
                    by = .(route)]

setorder(top_routes,-usage)
top_routes[,rank := sequence(.N)]
top_routes$top <- ifelse(top_routes$rank <= no_top_routes, 1,0)

# lista a topból
top_routes_list <- unlist(as.list(top_routes[top == 1, .(route)]))

###################
#### MODELLING ####
###################

# predikciót gyûjtó változó
train$usage_pred <- -999
test$usage_pred  <- -999

# right hand side variables - ami nem kell, azt kell felsorolni
rhs <- colnames(train)
rhs <- rhs[!(rhs %in% c("day","s_place_id","e_place_id","s_place_name", "usage",
                        "e_place_name","flag_train","tempm_avg_0",
                        "tempm_avg_6","tempm_avg_12","tempm_avg_18",
                        "tempm_avg","route","usage_pred","flag_own_test","usage_pct"))]
#rhs
howManyVars   <- length(rhs)
varlist_to_rf <- paste(rhs,collapse="+")
rm(rhs)

gbmGrid <- expand.grid(interaction.depth = (1:5) ,
                       n.trees = (1:10)*10,
                       shrinkage = .01,
                       n.minobsinnode = 1
)


for(i in c(1:length(top_routes_list))){
  print(paste("itt tart:", i))
  
  #set.seed(42)
  model_rf <- train(as.formula(paste0("usage~", varlist_to_rf)),
                    data      = train[route == top_routes_list[i],], 
                    method    = "gbm", 
                    distribution="poisson",
                    metric    = "RMSE",
                    maximize  = F,                                   # a logical: should the metric be maximized or minimized?
                    tuneGrid  = gbmGrid,
                    verbose = FALSE,
                    trControl = trainControl(method        = "LOOCV",
                                             #number        = 4,
                                             allowParallel = TRUE))
                    
  
  # model elmentése
  assign(paste0("fit_",substr(top_routes_list[i],1,4),"_",
                substr(top_routes_list[i],6,9)),
         model_rf,
         envir = .GlobalEnv)
  
  save(list  = paste0(         "fit_",substr(top_routes_list[i],1,4),"_",substr(top_routes_list[i],6,9)), 
       file  = paste0(save_dir,"fit_",substr(top_routes_list[i],1,4),"_",substr(top_routes_list[i],6,9),".rda"),
       envir = .GlobalEnv )    
  
  # predikció
  train[route      == top_routes_list[i],
        usage_pred := predict(model_rf, 
                              newdata = train[route == top_routes_list[i],])]
  
  test[ route      == top_routes_list[i],
        usage_pred := predict(model_rf, 
                              newdata =  test[route == top_routes_list[i],])]
  
  # modell törlése
  rm(list = paste0("fit_",substr(top_routes_list[i],1,4),"_",substr(top_routes_list[i],6,9)))
}
rm(i, model_rf, howManyVars)

#####################
#### PERFORMANCE ####
#####################

train$score <- train$usage_pred
#NDCG_calc(dataset = train)

#####################
#### SUBMISSION #####
#####################
submission <- test[,.(day,route, usage_pred)]

# sorrend megadása
setorder(submission, day,-usage_pred)

# sorrend változó létrehozása
submission[,rank := sequence(.N), by = c("day")]

submission <- submission[rank<= 100,.(day,route,rank)]
submission$rank <- 101-submission$rank


write.table(submission, 
            paste0(save_dir,"submission_faszom.csv"),
            sep = ',',
            row.names = FALSE,
            col.names = FALSE)
