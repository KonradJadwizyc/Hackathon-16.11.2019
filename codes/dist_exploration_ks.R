
  distances %>% class
full_data %>% class
distances %>% class
preferences %>% class


distances %>% select(city, raspberry, lime) %>% View




distances_ks<- distances %>% na.zero( ) %>% select( - city)
distances_ks %>% lapply(., as.numeric) %>% tbl_df %>% lapply(., is.na) %>% lapply(.,sum)
odleglosci_miedzy_miastami<- distances %>% na.zero( ) %>% select( - city) %>% as.matrix()%>% as.dist()




distances_ks<- distances %>% na.zero( ) 
distances_ks<-distances_ks %>% lapply(., as.numeric) %>% tbl_df %>% na.zero( )%>% is.na %>% any
asdf<- distances_ks %>% as.matrix()

asdf %>% head
asdf %>% dim
