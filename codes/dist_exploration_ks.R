
distances<
  distances %>% class
full_data %>% class
distances %>% class
preferences %>% class



jujube
raspberry


distances_ks<- distances %>% na.zero( ) %>% select( - city)
distances_ks %>% lapply(., as.numeric) %>% tbl_df %>% lapply(., is.na) %>% lapply(.,sum)
odleglosci_miedzy_miastami<- distances %>% na.zero( ) %>% select( - city) %>% as.matrix()%>% %>% as.dist()

