




full_data<-full_data  %>%   
  mutate(nr_tourists = wikipedia_nr_of_people / 1000 *tourists_using_accommodation_per1k ) %>%
  mutate(nr_noclegi = wikipedia_nr_of_people / 1000 *accommodations_per1k_given ) 
full_data$city %>% unique %>% sort


dane_prosty_model<- full_data %>% filter(year ==2018  &nr_tourists>0) %>% select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem )
# dane_prosty_model$nr_tourists_log<-log(dane_prosty_model$nr_tourists)
# dane_prosty_model$TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem_log<-log(dane_prosty_model$TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)




prosty_lin_mod<- lm(nr_tourists~., dane_prosty_model) 
est_liczba_turystow <- predict(prosty_lin_mod, (full_data %>% filter(year >=2013 &city == 'poznan') %>% select( TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem )))
est_liczba_turystow 
dane_est_nr_tourist_poznan = full_data %>% filter(year >=2013 &city == 'poznan') %>% select( year , TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)
dane_est_nr_tourist_poznan$est_liczba_turystow <- est_liczba_turystow
lm_est_poznan_2019<-  lm(est_liczba_turystow~year , dane_est_nr_tourist_poznan)



pred_data_poznan_przyszlosc<-data.frame(year = c(2019,2020))
przewidywania_poznan_2019<- predict(lm_est_poznan_2019 , pred_data_poznan_przyszlosc)
pred_data_poznan_przyszlosc$pred<-przewidywania_poznan_2019

full_data %>% filter(year ==2018 ) %>% 
  select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem ) %>% 
  cor


dane_obs_log<- full_data %>% filter(year ==2018 &nr_tourists>0) %>% 
  select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem ) %>% 
  mutate(nr_tourists_log=log(nr_tourists+1),
         TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem_log=log(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem+1))

dane_obs<- full_data %>% filter(year ==2018 &nr_tourists>0) %>% 
  select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem ) 
  # mutate(nr_tourists=log(nr_tourists+1),
  #        TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem=log(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem+1))

dane_obs %>% plot()
dane_obs_log %>% save_csv_email()

pred_data_poznan_przyszlosc%>% save_csv_email()


# 
# 
# prosty_lin_mod %>% summary()
# 
# prosty_lin_mod<- lm(nr_tourists_log~TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem_log, dane_prosty_model) 
# est_liczba_turystow <- predict(prosty_lin_mod, (full_data %>% filter(year >=2013 &city == 'poznan') %>% 
#                                                   select( TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem )))
# est_liczba_turystow 
# dane_est_nr_tourist_poznan = full_data %>% filter(year >=2013 &city == 'poznan') %>% select( year , TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)
# dane_est_nr_tourist_poznan$est_liczba_turystow <- est_liczba_turystow
# lm_est_poznan_2019<-  lm(est_liczba_turystow~year , dane_est_nr_tourist_poznan)
# przewidywania_poznan_2019<- predict(lm_est_poznan_2019 , data.frame(year = 2019))
# 
# 
# full_data %>% filter(year ==2018 ) %>% 
#   select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem ) %>% 
#   cor
# 
# 
# full_data %>% filter(year ==2018 &nr_tourists>0) %>% 
#   select(nr_tourists, TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem ) %>% 
#   # mutate(nr_tourists=log(nr_tourists+1), 
#   #        TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem=log(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem+1)) %>% 
#   plot
# 
# 





# 
# full_data %>% filter(year >=2013 &city == 'poznan') %>% select (year , est_liczba_turystow)
# 
# est_liczba_turystow %>% barplot
# prosty_lin_mod %>% summary
# 
# 
# 
# dane_pred<- full_data %>% filter(year >=2013 ) %>% select(year)
# est_liczba_turystow %>% barplot
# 
# 
# 
# 
# liczba_na<- full_data %>% filter(!is.na(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem) & year >= 2012) %>% lapply(.,is.na) %>% lapply(.,sum) %>% unlist
# nazwy_50_col_turystyka<- liczba_na[(liczba_na==0)]
# 
# # nazwy_50_col_turystyka<-c('nr_tourists',nazwy_50_col_turystyka)
# 
# dane_liczba_turystow_zagranica <- full_data  %>% filter (!is.na(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem) & year >= 2012 )
# dane_liczba_turystow_zagranica<-dane_liczba_turystow_zagranica[, names(nazwy_50_col_turystyka)]
# dane_liczba_turystow_zagranica<- dane_liczba_turystow_zagranica %>% rename(target  = TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)
# dane_liczba_turystow_zagranica<-dane_liczba_turystow_zagranica %>% select(-city)
# 
# 
# 
# model_2019_names <- c('nr_tourists',names(nazwy_50_col_turystyka))
# dane_liczba_turystow_2018<- full_data %>% filter(year ==2018 )
# dane_2019_model <- dane_liczba_turystow_2018[,model_2019_names]
# dane_2019_model<- dane_2019_model %>% select(-city)
# 
# 
# 
# library(randomForest)
# random_forest<- randomForest::randomForest(x = dane_2019_model %>% select( - nr_tourists,-year) %>% as.data.frame,
#                            y  = dane_2019_model$nr_tourists, ntree=50)
# 
# colnames(dane_2019_model %>% select( - nr_tourists,-year))
# dane_liczba_turystow_zagranica[,model_2019_names]
# predict(random_forest,)
# 
# 
# mac_impo<- importance(random_forest) 
# mac_impo<-mac_impo %>% tbl_df 
# mac_impo$nazwa<- rownames(importance(random_forest) )
# mac_impo %>% tbl_df %>% arrange(-IncNodePurity)
#  
# lin_mod_cols_2019_final<- mac_impo$nazwa %>% head(15)
# # lin_mod_cols_2019_final<- c('nr_tourists',lin_mod_cols_2019_final)
# # lin_mod_cols_2019_final<-unique(lin_mod_cols_2019_final)
# 
# lin_mod_2019_imputate <- lm (nr_tourists~.,  dane_2019_model[,c('nr_tourists',lin_mod_cols_2019_final)])
# lin_mod_2019_imputate %>% summary
# 
# 
# 
# options(scipen = 999)
# dane_liczba_turystow_zagranica[,lin_mod_cols_2019_final]
# 
# 
# predict(lin_mod_2019_imputate,dane_liczba_turystow_zagranica[,lin_mod_cols_2019_final] )
# lin_mod_2019_imputate
# 
# 
# 
#  
# dane_2019_model %>% View
#  
# lin_mod_2019 <- lm(nr_tourists~., dane_2019_model %>% select(-city))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# full_data$year <-as.numeric(full_data$year )
# 
# # liczba_na <- full_data %>% lapply(., is.na) %>% lapply(., sum ) %>% unlist
# # dane_model_TURYSTYCZNE_OBIEKTY <- full_data[, names(liczba_na)[(liczba_na <1500)]]
# # dane_model_TURYSTYCZNE_OBIEKTY %>% rename(target  = TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem)
# 
# 
# full_data %>% select(year, contains('TURYSTYCZNE_OBIEKTY.')) %>% 
#   filter(complete.cases(.)) %>% 
#   group_by(year) %>% summarise_all(is.na)
# 
# 
# 
# 
# 
# 
# 
# full_data$TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem %>% is.na %>% sum
# 
# full_data %>% dim
# 
# 
# liczba_na<- full_data %>% filter(!is.na(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem) & year >= 2013) %>% lapply(.,is.na) %>% lapply(.,sum) %>% unlist
# 
# nazwy_50_col_turystyka<- liczba_na[(liczba_na==0)]
# 
# dane_liczba_turystow_zagranica <- full_data  %>% filter (!is.na(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem) & year >= 2013 )
# dane_liczba_turystow_zagranica<-dane_liczba_turystow_zagranica[, names(nazwy_50_col_turystyka)]
# 
# dane_liczba_turystow_zagranica %>% dim
# 
# dane_liczba_turystow_zagranica %>% rename()
# 
# 
# dane_liczba_turystow_zagranica
# 
# 
# 
# 
# full_data %>% filter(year==2018 ) %>% 
#   select(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem, touristic_popularity) %>% 
#   mutate(touristic_popularity = log(touristic_popularity+1)) %>% 
#   filter(complete.cases(.))  %>% cor
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# full_data %>% select(year, contains('TURYSTYCZNE_OBIEKTY.')) %>% 
# 
# 
# 
# full_data %>% filter(!is.na(TURYSTYCZNE_OBIEKTY.schroniska_mlodziezowe_miejsca_noclegowe_caloroczne))
# 
# 
# full_data %>% select(contains('TURYSTYCZNE_OBIEKTY.')) %>% lapply(.,is.na) %>% lapply(.,sum) %>% unlist
# 
# full_data %>% 
# 
# 
# 
# full_data %>% select(TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_ogolem,nr_tourists) %>% 
#   filter(complete.cases(.)) %>% cor
# 
# 
# 
# 
# full_data
# dane_model_TURYSTYCZNE_OBIEKTY %>%  filter(complete.cases(.)) 
# 
# dane_model_TURYSTYCZNE_OBIEKTY %>% colnames
# 
# dane_model_TURYSTYCZNE_OBIEKTY
# 
# 
# (full_data$nr_noclegi %>% is.na) %>% sum
# 
# 
# full_data %>% select(! is.numeric(full_data) )
# 
# 
# asdf<- c('nr_noclegi', sample((names(full_data)), 10 ) )
# (full_data[,asdf] %>% filter(complete.cases(.)) %>% cor)[,1]
# 
# 
# budzet_wydatki_gospodarka_komunalna
# 
# 
# library(Hmisc)
# 
# 
# data<- full_data %>% select( contains('DOCHODY_BUDZETOW_MIAST.Dochody_na_mieszkanca_dochody'))
# data<- data %>% filter(complete.cases(.))
# 
# prc_data <- prcomp(data)
# n_pca_dims <- floor(log(dim(data)[2], base = 1.7))
# 
# prc_data$ x %>% str
# 
# prc_data %>% summary
# dim(data)[2]
# n_pca_dims <- floor(log(dim(data)[2], base = 1.7))
# 
# full_data$accommodations_per1k_given %>% na.omit %>% hist
# 
# 
# 
# data
# 
# 
# 
# 
# full_data  %>% 
#   select(tourists_using_accommodation_per1k, accommodations_per1k_given)  %>% 
#   filter(complete.cases(.)) %>% 
#   mutate(nr_nocy = accommodations_per1k_given / tourists_using_accommodation_per1k) %>% 
#   select(nr_nocy) %>% .[[1]] %>% plot
# 
# 
# 
# 
# # full_data$nr_noclegi<- 
#   full_data
# 
# 
# full_data  %>% select(tourists_using_accommodation_per1k, accommodations_per1k_given,wikipedia_nr_of_people, city) 
# 
# 
# 
# full_data<-full_data  %>%   
#   mutate(nr_tourists = wikipedia_nr_of_people / 1000 *tourists_using_accommodation_per1k ) %>%
#   mutate(nr_noclegi = wikipedia_nr_of_people / 1000 *accommodations_per1k_given ) 
# 
# 
# 
# 
#   
#   
#   full_data  %>% 
#     select(tourists_using_accommodation_per1k, accommodations_per1k_given,wikipedia_nr_of_people, city)  %>% 
#     filter(complete.cases(.)) %>% 
#     mutate(nr_tourists = wikipedia_nr_of_people / 1000 *tourists_using_accommodation_per1k ) %>%
#     mutate(nr_noclegi = wikipedia_nr_of_people / 1000 *accommodations_per1k_given ) %>% select(nr_tourists)%>% apply(.,2,sum)
#   
#   
#   
#   
# full_data$wikipedia_nr_of_people %>% na.omit
# full_data$wikipedia_nr_of_avaliable_languages%>% na.omit
# 
# full_data %>% filter((accommodations_per1k_given ))
# 
# full_data %>% 
# 
# 
# 
# 
# 
# 
# 
