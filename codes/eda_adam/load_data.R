source("./codes/functions.R")
source("./codes/packages.R")

ROOT <- "./data/input/"

load(paste0(ROOT,"distances.Rdata"))
load(paste0(ROOT,"full_data.Rdata"))
load(paste0(ROOT,"preferences.Rdata"))


names(full_data)
distances

owoce <- unique(full_data$city)
dict_city <- data.frame(city = owoce, city_n = 1:length(owoce))


full_data$year <- as.numeric(full_data$year)
full_data_2 <- full_data %>% 
  left_join(dict_city, by = 'city') %>% 
  select(-city)

full_data %>% names()


ols <- lm(touristic_popularity ~., touristic)
summary(ols)
map(touristic, ~sum(is.na(.x)))


library(corrplot)
from <- which(names(full_data_2) == "TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_Austria")
to <- (names(full_data_2) == "TURYSTYCZNE_OBIEKTY.udzielone_noclegi_turystom_zagranicznym_Wlochy")




which(!map_lgl(full_data, is.numeric))

cols_to_rm <- c(from:to, 509:540, 548:566)



# pca ---------------------------------------------------------------------
full_data_3 <- map_dfc(full_data_2, ~ifelse(is.na(.x),-1,.x))
pca <- prcomp(full_data_3, center = TRUE, scale. = TRUE, rank = 10)
rotation <- pca$rotation

p1 <- as.matrix(pca$rotation)

osiemnascie <- full_data_2 %>%  
  filter(year == 2018)

udzielone_noclegi_turystom_zagranicznym_sum <- rowSums(osiemnascie[,from:to], na.rm = T)

touristic <- full_data_2[!is.na(full_data_2$touristic_popularity),]

brakujace <- setdiff(osiemnascie$city_n,touristic$city_n)

