

get_EDA_col_numeric<-function (vec) {
  print("numeric")
  as.table(print(summary(vec)))
  print(" ")
  print("odchylenie i wariancja")
  print(" ")
  print(paste0('odchylenie: ' , as.character( sd(vec))))
  print(paste0('wariancja: ' , as.character( var(vec))))
  print(" ")
  print("braki danych")
  print(sum(is.na(vec)))
  print("+++++++++++++++++++++++++++++++++++++++++")
} 


get_EDA_col_categorical<-function (vec) {
  print("categorical")
  as.table(print(sort(table(vec))))
  
  print("braki danych")
  
  print(mean(is.na(vec)))
  print("+++++++++++++++++++++++++++++++++++++++++")
  
} 


apply_eda <- function (x)  {
  
  if(class(x) %in% c('numeric', 'double')){
    return(get_EDA_col_numeric(x))
 
  }else {
    return(get_EDA_col_categorical(x))
    
  }
    
 
}





get_EDA_all_hist_plot <- function (x, table_name, name_hist) {
  if (class (x) %in% c('numeric', 'double')){
  hist(x,
       main =paste0(table_name," : ",name_hist),
       xlab = name_hist)}
}




# # # # #  Przyklady wywolania
# 
mtcars %>% lapply(.,apply_eda)
#mtcars %>% lapply(.,get_EDA_all_hist_plot)
# 
# name<- 'iris'
# mapply(get_EDA_all_hist_plot, get(name), name, names(get(name)))
# 
# name<- 'mtcars'
# mapply(get_EDA_all_hist_plot, get(name), name, names(get(name)))
# get(name)




