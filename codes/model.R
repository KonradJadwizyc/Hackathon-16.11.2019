# get data
data<- full_data 

# prepare target - oriented columns in proprer way
data<-data %>% mutate(target = log(touristic_popularity)) %>% select( -touristic_popularity)
data$TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_Finlandia<-NULL
data$wikipedia_density<- log(data$wikipedia_density)
data$nr_tourists<-log(data$nr_tourists+1)
data$nr_noclegi<-log(data$nr_noclegi+1)
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("Imprezy_masowe.", colnames(data))], .funs = function(x){ log(x+1)}) 
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("TURYSTYCZNE_OBIEKTY.", colnames(data))], .funs = function(x){ log(x+1)}) 
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("TARGOWISKA.", colnames(data))], .funs = function(x){ log(x+1)}) 
data<-data %>% rename(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_zagranica_ogolem=TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("budzet_wydatki_kultura_", colnames(data))], .funs = function(x){ log(x+1)})
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("DOCHODY_BUDZETOW_MIAST.", colnames(data))], .funs = function(x){ log(x+1)}) 
data<-data %>% select( -Imprezy_masowe.liczba_imprez_sportowe)
data<-data %>% select( -Imprezy_masowe.Liczba_uczestnikow_imprez_sportowe)
data<-data %>% mutate_at(.,.vars= colnames(data)[grepl("budzet_wydatki", colnames(data))], .funs = function(x){ log(x+1)}) 
data$accommodations_per1k_given<-log(data$accommodations_per1k_given+1)
data$tourists_using_accommodation_per1k<-log(data$tourists_using_accommodation_per1k+1)


# split test data
data_test<-data %>% filter(!complete.cases(.)) %>% filter(year == 2018)
data<-data %>% filter(complete.cases(.))


# save city name
data_test_city<- data_test$city
data_city<- data$city
# resign from learn sample as categorical variable
data$city<-NULL


# check if xgboost feature selection is valid
if (!all((data %>% lapply(.,class) %>% unlist ) %in% c('numeric','double'))) { 
  stop('zbior zawiera zmienne kategorialne')
  
}

if (!file.exists(paste0(ROOT_CODES, 'names_lin_mod.RData'))){

  # use only sample to select most important
nr_ucz <- sample(1:59, 50 )
xgb_data_ucz <- xgboost::xgb.DMatrix(as.matrix(data[ nr_ucz, ] %>% select (-target)) , label = data[ nr_ucz, ]$target)
# xgb_data_test<- xgboost::xgb.DMatrix(as.matrix(data[-nr_ucz, ] %>% select (-target)) , label = data[-nr_ucz, ]$target) # as an explainable modle only will not be used with test set

# model selection
params = list(
  eta = .01,
  max_depth = 2
)
xgb_init<- xgboost(xgb_data_ucz, nrounds = 1000,params = params ,verbose=T)

# select columns
feature_impo <- xgb.importance(xgb_init$feature_names,xgb_init) 
feature_impo<-feature_impo %>% tbl_df %>% arrange(-Gain)
names_lin_mod <- (feature_impo[,'Feature'] %>% head(15))[[1]]
names_lin_mod<- c(names_lin_mod, 'target')

# save columns after manual selection and view of zgb model
save(names_lin_mod, file = paste0(ROOT_CODES, 'names_lin_mod.RData'))

}else{
  load( paste0(ROOT_CODES, 'names_lin_mod.RData'))
}


# create linear model
lin_mod_attraction <- lm(target~. , data[,names_lin_mod]) 

lin_mod_attraction %>% summary 
# broom::tidy(lin_mod_attraction %>% summary ) %>% save_csv_email()

pred_train<- predict( lin_mod_attraction,(data[-nr_ucz,])) %>% exp




# final model scoring
pred = exp(predict( lin_mod_attraction,(data_test)))


# save_output
output_df_1 = data.frame('city' = data_test_city, 'touristic_popularity'=  pred)
save(output_df_1,file= paste0(ROOT_CODES, 'output_df_1.RData'))




# check if data were queried in proper way (simirlar to train set)
bind_rows(data_test[,names_lin_mod],data[,names_lin_mod]) %>% View






# # # # # #        experimental section  below  - PLEASE OMIT  



# obs <- data[-nr_ucz,]$target %>% exp
# MAPE(pred, obs)
# MAE(pred, obs)
# plot(pred, obs)
# points(cbind((1:100)*100,(1:100)*100), col='red', type='l')


# data[,names_lin_mod] %>% save_xlsx_email()





# # # # # Sprawdzanie wynikow  na losowej probie
# results<- c()
# for(i in 1:1000){
#   set.seed(i)
#   nr_ucz <- sample(1:59, 58 )
#   lin_mod_attraction <- lm(target~. , data[nr_ucz,names_lin_mod])
#   # data[nr_ucz,names_lin_mod] %>% View
#   lin_mod_attraction %>% summary
#   pred<- predict( lin_mod_attraction,(data[-nr_ucz,])) %>% exp
# 
#   obs <- data[-nr_ucz,]$target %>% exp
#   results<- c(results,MAE(obs,pred) )
# }
# results %>% summary
# results %>% quantile(., probs = seq(0,1, length.out = 11))


