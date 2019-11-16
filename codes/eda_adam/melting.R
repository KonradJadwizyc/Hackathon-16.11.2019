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
         public_transfer_locality_mean,
         touristic_popularity,
         wikipedia_area,
         wikipedia_nr_of_people,
         wikipedia_city_rights_since,
         DOCHODY_BUDZETOW_MIAST.Dochody_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_majatkowe_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_na_mieszkanca_ogolem,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_600_Transport_i_lacznosc,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_803_Szkolnictwo_wyzsze,
         DOCHODY_BUDZETOW_MIAST.Dotacje_ogolem,
         Domy_i_osrodki_kultury.ogolem,
         Imprezy_masowe.liczba_imprez_ogolem,
         Imprezy_masowe.liczba_imprez_sportowe,
         KINA.Liczba_ludnosci_na_miejsce_w_kinach_stalych,
         KOMUNIKACJA_MIEJSKA.dlugosc_bus_pasow,
         MUZEA.Muzea_w_gestii_samorzadu_muzea_lacznie_z_oddzialami,
         `SCIEZKI_ROWEROWE.sciezki_rowerowe_(drogi_dla_rowerow)_ogolem`,
         TARGOWISKA.Targowiska_sezonowe,
         TARGOWISKA.Targowiska_stale,
         `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_*****`,
         `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_***`,
         accommodations_per1k_given)
  
library(xgboost)
cleaned_known <- data_cleaned %>% 
  filter(!is.na(touristic_popularity))
cleaned_unknown <- data_cleaned %>% 
  filter(is.na(touristic_popularity))

t <- 0.7
train_ind <- sample( 1:nrow(cleaned_known), round(t * nrow(cleaned_known)))
train <- cleaned_known[train_ind,]
test <-  cleaned_known[-train_ind,]


target_col <- which(names(train) == "touristic_popularity")
dtrain <- xgb.DMatrix(as.matrix(train[-target_col]), label = train[[target_col]])
dtest <- xgb.DMatrix(as.matrix(test[-target_col]), label = test[[target_col]])
dfull <- xgb.DMatrix(as.matrix(cleaned_known[-target_col]), label = cleaned_known[[target_col]])


params <- list(booster = "gbtree", objective = "reg:linear", max_depth= 6, min_child_weight=12, subsample=0.6, colsample_bytree=0.8, eta=0.02)
xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early.stop.round = 30, maximize = F , eval_metric = "mae")

xgbpred <- predict(xgb1, dtest)
mean(abs(xgbpred - test$touristic_popularity))



# final train and prediction ----------------------------------------------

dfinal <- xgb.DMatrix(as.matrix(cleaned_unknown[-target_col]), label = cleaned_unknown[[target_col]])

xgb_final <- xgb.train(params = params, data = dfull, nrounds = 200, print_every_n = 10, maximize = F , eval_metric = "mae")

final_preds <- predict(xgb_final, newdata = dfinal)

ciities_f <- o %>% 
  filter(is.na(touristic_popularity)) %>% 
  select(city_n) %>% 
  left_join(dict_city, by = 'city_n') %>% 
  select(city)

RESULTOS <- data.frame(city = ciities_f,
                       
                       touristic_popularity = final_preds)


RESULTOS %>% 
  write.csv("output_df_1.csv")



# explain -----------------------------------------------------------------

explain_xgb <- explain(model = xgb_final, 
                                    data = as.matrix(cleaned_known[-target_col]),
                                    y = cleaned_known[[target_col]],
                                    label = " xgb ")

library("iBreakDown")


mat <- xgb.importance (model = xgb_final)
xgb.plot.importance (importance_matrix = mat[1:20])

predict(explain_xgb, as.matrix(cleaned_unknown[4,-target_col]))

shap_posek <- shap(explain_xgb, as.matrix(cleaned_unknown[4,-target_col]), B = 5)
plot(shap_posek)

shap_posek

xgb_touristic <- ceteris_paribus(explain_xgb, as.matrix(cleaned_unknown[4,-target_col]))
xgb_touristic

var_to_pl <- c("accommodations_per1k_given", 
               "wikipedia_area", 
               "DOCHODY_BUDZETOW_MIAST.Dochody_na_mieszkanca_ogolem",
               "TARGOWISKA.Targowiska_sezonowe",
               "KOMUNIKACJA_MIEJSKA.dlugosc_bus_pasow",
               "safety_level_locality_mean")
plot(xgb_touristic, variables = var_to_pl) +
  show_observations(xgb_touristic, variables = var_to_pl) +
  ggtitle("Ceteris Paribus Profiles", "For the random forest model and the Titanic dataset")



variable_importance(explain_xgb, type = "raw")

# ols ---------------------------------------------------------------------


t <- 0.7
train_ind_ols <- sample( 1:nrow(cleaned_known), round(t * nrow(cleaned_known)))
train_o <- cleaned_known[train_ind_ols,]
test_o <-  cleaned_known[-train_ind_ols,]


ols <- lm(touristic_popularity~. - DOCHODY_BUDZETOW_MIAST.Dochody_majatkowe_ogolem - 
            DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_600_Transport_i_lacznosc -
            DOCHODY_BUDZETOW_MIAST.Dochody_ogolem -
            DOCHODY_BUDZETOW_MIAST.Dotacje_ogolem -
            DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_600_Transport_i_lacznosc -
            Imprezy_masowe.liczba_imprez_sportowe -
            DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_803_Szkolnictwo_wyzsze -
            Domy_i_osrodki_kultury.ogolem -
            wikipedia_city_rights_since  -
            public_transfer_locality_mean -
            KINA.Liczba_ludnosci_na_miejsce_w_kinach_stalych -
            wikipedia_area
            , train_o)

summary(ols)
full_o <- test_o
full_o$predicted <- predict(ols, newdata = test_o)

mean(abs(full_o$residual <- full_o$predicted - full_o$touristic_popularity))


write.csv(data_cleaned,"./data/precessd/data_cleaned.csv")
save_csv_email(data_cleaned)


