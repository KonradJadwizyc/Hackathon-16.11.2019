# getScriptName()


load(paste0(ROOT_DATA,'input/distances.Rdata'))
load(paste0(ROOT_DATA,'input/full_data.Rdata'))
load(paste0(ROOT_DATA,'input/preferences.Rdata'))

full_data<-full_data  %>%
  mutate(nr_tourists = wikipedia_nr_of_people / 1000 *tourists_using_accommodation_per1k ) %>%
  mutate(nr_noclegi = wikipedia_nr_of_people / 1000 *accommodations_per1k_given )
full_data$year<-as.numeric(full_data$year)
