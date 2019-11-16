rowSums(osiemnascie[,509:514])


o <- osiemnascie %>% 
  mutate(happy_about_locality_mean = 1* happy_about_locality_1 + 2 * happy_about_locality_2 + 3* happy_about_locality_3 +
           4 * happy_about_locality_4 + 5 * happy_about_locality_5 + 6 * happy_about_locality_6) %>% 
  mutate(safety_level_locality_mean =  1*safety_level_locality_1 + 2 * safety_level_locality_2 + 3* safety_level_locality_3 +
           4 * safety_level_locality_4 + 5 * safety_level_locality_5 + 6 * safety_level_locality_6) %>% 
  mutate(recreation_areas_locality_mean =  1*recreation_areas_locality_1 + 2 * recreation_areas_locality_2 + 3* recreation_areas_locality_3 +
           4 * recreation_areas_locality_4 + 5 * recreation_areas_locality_5 + 6 * recreation_areas_locality_6)%>% 
  mutate(public_transfer_locality_mean =  1*public_transfer_locality_1 + 2 * public_transfer_locality_2 + 3* public_transfer_locality_3 +
           4 * public_transfer_locality_4 + 5 * public_transfer_locality_5 + 6 * public_transfer_locality_6)



names(o)


data_cleaned <- o %>% 
  select(happy_about_locality_mean,
         safety_level_locality_mean,
         recreation_areas_locality_mean,
         public_transfer_locality_4,
         touristic_popularity,
         wikipedia_area,
         wikipedia_nr_of_people,
         wikipedia_city_rights_since,
         DOCHODY_BUDZETOW_MIAST.Dochody_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_majatkowe_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_na_mieszkanca_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_600_Transport_i_lacznosc,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_803_Szkolnictwo_wyzsze,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_926_Kultura_fizyczna_i_sport,
         DOCHODY_BUDZETOW_MIAST.Dotacje_ogolem,
         Domy_i_osrodki_kultury.ogolem,
         Imprezy_masowe.liczba_imprez_ogolem,
         Imprezy_masowe.liczba_imprez_sportowe,
         KINA.Liczba_ludnosci_na_miejsce_w_kinach_stalych,
         KOMUNIKACJA_MIEJSKA.dlugosc_bus_pasow,
         OCHRONA_PRZYRODY.ogolem,
         MUZEA.Muzea_w_gestii_samorzadu_muzea_lacznie_z_oddzialami,
         `SCIEZKI_ROWEROWE.sciezki_rowerowe_(drogi_dla_rowerow)_ogolem`,
         TARGOWISKA.Targowiska_sezonowe,
         TARGOWISKA.Targowiska_stale,
         TERENY_ZIELENI.tereny_zieleni_osiedlowej_powierzchnia_w_miastach,
         TRANSPORT_MORSKI.liczba_statkow_promy,
         `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_*****`,
         `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_***`,
         accommodations_per1k_given)
  
library(xgboost)

t <- 0.7
train_ind <- sample( 1:nrow(touristic), round(t * nrow(touristic)))
train <- touristic[train_ind,]
test <-  touristic[-train_ind,]

target_col <- which(names(train) == "touristic_popularity")
dtrain <- xgb.DMatrix(as.matrix(train[-target_col]), label = train[[target_col]])
dtest <- xgb.DMatrix(as.matrix(test[-target_col]), label = test[[target_col]])

params <- list(booster = "gbtree", objective = "reg:linear", max_depth=8, min_child_weight=12, subsample=0.6, colsample_bytree=0.8, eta=0.003)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 200, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early.stop.round = 20, maximize = F , eval_metric = "mae")


# ols ---------------------------------------------------------------------
cleaned_known <- data_cleaned %>% 
  filter(!is.na(touristic_popularity))


t <- 0.7
train_ind_ols <- sample( 1:nrow(cleaned_known), round(t * nrow(cleaned_known)))
train_o <- cleaned_known[train_ind_ols,]
test_o <-  cleaned_known[-train_ind_ols,]


ols <- lm(touristic_popularity~., train_o)

full_o <- test_o
full_o$predicted <- predict(ols, newdata = test_o)

mean(abs(full_o$residual <- full_o$predicted - full_o$touristic_popularity))
