
  distances %>% class
full_data %>% class
distances %>% class
preferences %>% class


distances %>% select(city, raspberry, lime) %>% View




distances_ks<- distances %>% na.zero( ) %>% select( - city)
# distances_ks %>% lapply(., as.numeric) %>% tbl_df %>% lapply(., is.na) %>% lapply(.,sum)
odleglosci_miedzy_miastami<- distances %>% na.zero( ) %>% select( - city) %>% as.matrix()%>% as.dist()




distances_ks<- distances %>% select (-city) %>% na.zero( ) 
distances_ks<-distances_ks %>% lapply(., as.numeric) %>% tbl_df %>% na.zero( )
dist_mat<- distances_ks %>% as.matrix()
dist_mat<-dist_mat%>% as.dist

tsne_data<- Rtsne::Rtsne(dist_mat, theta = 0, perplexity = 20,
                         is_distance = T,
                         pca= F, max_iter = 100000, verbose=T)
tsne_data$Y %>% plot

tsne_data$Y %>% ggplot()

names(distances_ks)

asdf<-tsne_data$Y %>% as.data.frame()
asdf$label = names(distances_ks)

ggplot(asdf,aes(V1,V2, label =label) )+
  geom_point()+
  geom_label()










