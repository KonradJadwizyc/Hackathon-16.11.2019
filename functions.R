factor_to_character_dt<-function(dt){
  
  return(dict_curr_big %>% lapply(.,function(x){ if(is.factor(x) )   {return(as.character(x)) } else return(x) } ) %>% data.frame(., stringsAsFactors = F) %>% data.table())
  
}







measure_time<-function(text, n=100 ) {
  
  t1=proc.time()
  
  expr<- parse(text = text)
  
  for (i in 1:n) {eval(expr) }
  
  t2=proc.time()
  
  return( (t2-t1)[3])
  
  
  
}







usun_polskie_znaki <- function(x) {
  
  translate_vector<-function(vec){
    
    return( stringi::stri_trans_general(vec, "latin-ascii"))
    
  }
  
  if (all(class(x ) == c("data.table" ,"data.frame"))){
    
    ret_x<-   lapply(x, function(a){ if (class(a) == 'character') {return(translate_vector(a) ) } else {return (a )} }) %>% data.frame %>% data.table()
    
  }else{
    
    ret_x<- translate_vector(x)
    
  }
  
  ret_x
  
}





eraseDuplicatedColumnsByMerge<-function(data.table){
  
  dane_daily_promo<-dane_daily_promo %>% tbl_df()%>% select (-contains(".y")) %>% data.table()
  
  colnames(dane_daily_promo)<- stringi::stri_replace_all_fixed(colnames(dane_daily_promo),".x","")
  
  return(dane_daily_promo)
  
}







dodatnie <-function (vec){
  
  return(vec [vec >0 ])
  
}





pozycje_obs_odst<-function(vec, type = c('both','lower','upper')){
  
  if (type[1] == 'both'){
    
    ogr_gorne <- mean(vec) + 3 * sd (vec)
    
    ogr_dolne <- mean(vec) - 3 * sd (vec)
    
    ret_vec<-  (vec <= ogr_dolne | vec >= ogr_gorne)
    
    return(ret_vec)}
  
  else if (type[1] == 'upper'){
    
    ogr_gorne <- mean(vec) - 3 * sd (vec)
    
    ret_vec<- vec >= ogr_gorne
    
    return(ret_vec)
    
  }else if (type[1] == 'lower') {
    
    ogr_dolne <- mean(vec) - 3 * sd (vec)
    
    ret_vec<-  vec <= ogr_dolne
    
    return(ret_vec)
    
  }else {
    
    error("Niepoprawny typ")
    
  }
  
}











get_auc_precision<- function(obs,pred, top_perc = .05){
  
  tmp_res<- (data.table  (cbind(obs,pred)))[order(-pred)]
  
  l_zlapanych <- tmp_res[1:(nrow(tmp_res) * top_perc)]$obs %>% sum 
  
  return(l_zlapanych / sum(obs))
  
}











save_send_email<- function(df){
  
  df %>% write.xlsx(., file  = paste0(ROOT_OUTPUT, 'temp.xlsx'))
  
  sender <- "przyklady_Azure@zabka.pl" # Replace with a valid address
  
  recipients <- c("krzysztof.sikorski@zabka.pl") # Replace with one or more valid addresses
  
  
  
  email <- send.mail(from = sender,
                     
                     to = recipients, encoding = "utf-8",
                     
                     subject="export Azure",
                     
                     body = 'Eksport z Azure',
                     
                     smtp = list(host.name = "poczta.zabka.pl", port = 25),
                     
                     attach.files = paste0(ROOT_OUTPUT, 'temp.xlsx'),
                     
                     authenticate = FALSE,
                     
                     send = FALSE)
  
  email$send()
  
}

















get_env_space_table<-function(){
  
  obiekty<- ls(envir= .GlobalEnv)
  
  rozmiar<- obiekty %>% lapply(.,function(x) {  format(object.size( get(x)) ,'Mb') }) %>% unlist %>% stri_replace_all_fixed(.," Mb","")%>% as.numeric()
  
  return(data.table ('obiekty' = obiekty, 'rozmiar_Mb' = rozmiar) %>% .[ order(-rozmiar)])}



get_objects_size<-function(){
  
  
  
  names<- ls(envir= .GlobalEnv)
  
  objects<- data.frame(
    
    'size_Mb' = names %>% lapply( ., function(x) {object.size( get(x) )%>% format(.,'Mb')}) %>%
      
      lapply (.,  function(x) {stri_split_fixed(x,pattern=' ')}) %>% lapply(.,first)%>% lapply(.,first) %>% lapply(.,as.numeric) %>% unlist,
    
    'name' = names)
  
  
  
  objects<-objects %>% data.table() %>% .[order(size_Mb )]
  
  return(objects)
  
  
  
}











odpytaj<- function(zapytanie_fun, default_dsn_name = DSN_NAME){
  
  con <- odbcConnect(dsn=default_dsn_name)
  
  temp_table<- sqlQuery(con, zapytanie_fun,believeNRows=F,stringsAsFactors = F)
  
  temp_table<-data.table(temp_table)
  
  odbcClose(con)
  
  return(temp_table)
  
}



getmode <- function(v) {
  
  uniqv <- unique(v)
  
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}





lonlat_to_xy <- function(dt, lon_name, lat_name){
  
  coordinates(dt) <- c(lon_name, lat_name)
  
  proj4string(dt) <- CRS('+init=epsg:4326')
  
  dt <- spTransform(dt, CRS('+init=epsg:2180'))
  
  as.data.table(dt)
  
}

R2 <- function(actual,predict) {
  
  1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))}

MAPE<- function(obs,pred){
  
  mean(abs((pred- obs) / obs))
  
}

MAE<- function(obs,pred){
  
  mean(abs((pred- obs) ))
  
}

get_env_space<- function(){
  
  (ls(envir= .GlobalEnv) %>% lapply( ., function(x) {object.size( get(x) )}) %>% unlist %>% sum)/1024/1024
  
}



'%!in%' <- function(x,y)!('%in%'(x,y))





# # # File location

# "C:/Projekty/pomocnicze/funkcje.R"

install_and_load_packages <- function(required_packages){
  
  for(pkg in setdiff(required_packages, installed.packages()[,"Package"])){
    
    install.packages(pkg)
    
  }
  
  for(pkg in required_packages){
    
    library(pkg, character.only=TRUE)
    
  }
  
}



cz_dod<-function(x) {
  
  
  
  x[x<0]<-0
  
  x
  
}





define_presto_session <- function(user,
                                  
                                  output_path = '/sbdata/automated_upload/facings_20180919/',
                                  
                                  host='172.20.0.37',
                                  
                                  port=9191,
                                  
                                  schema='ks',
                                  
                                  catalog='hive'){
  
  
  
  presto_query <- function(query, save_as=NULL){
    
    con <- dbConnect(
      
      RPresto::Presto(),
      
      host=host,
      
      port=port,
      
      user=user,
      
      schema=schema,
      
      catalog=catalog
      
    )
    
    
    
    res <- data.table(dbGetQuery(con, query))
    
    dbDisconnect(con)
    
    
    
    if(!is.null(save_as)){
      
      query_date <- gsub('-', '_', Sys.Date())
      
      
      
      full_name <- paste0(query_date, '_', save_as)
      
      
      
      i <- 1
      
      while(dir.exists(paste0(output_path, full_name, '_', i))){
        
        i <- i+1
        
      }
      
      full_name <- paste0(full_name, '_', i)
      
      
      
      dir.create(paste0(output_path, full_name))
      
      
      
      fwrite(res, paste0(output_path, full_name, '/', full_name, '.csv'))
      
      
      
      fileConn <- file(paste0(output_path, full_name, '/', full_name, '.sql'))
      
      write(query, fileConn)
      
      close(fileConn)
      
      
      
      cat('Query and output saved at', paste0(output_path, full_name, '/'), '\n')
      
    }
    
    
    
    return(res)
    
  }
  
  
  
  return(
    
    presto_query
    
  )
  
}



getmode <- function(v) {
  
  uniqv <- unique(v)
  
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}





change_colnames<- function(df){
  
  colnames(df )<-colnames(df )%>%
    
    stringi::stri_replace_all_fixed(.,"–"," ") %>%
    
    stringi::stri_replace_all_fixed(.,"."," ") %>%
    
    stringi::stri_replace_all_fixed(.,"-","")  %>%
    
    stringi::stri_replace_all_fixed(.,"  "," ")%>%
    
    stringi::stri_replace_all_fixed(.,"  "," ")%>%
    
    stringi::stri_replace_all_fixed(.,"  "," ")%>%
    
    stringi::stri_replace_all_fixed(.,"  "," ")%>%
    
    stringi::stri_replace_all_fixed(.,"?","")  %>%
    
    stringi::stri_replace_all_fixed(.," ","_")
  
  colnames(df )<-colnames(df ) %>% stringi::stri_trans_general(.,id="latin-ascii")
  
  return(df)
  
}



showAtMap<-function(a,b){
  
  if (a<b ){
    
    x<- a
    
    y<-b} else {
      
      y<-a
      
      x<-b
      
    }
  
  link<- sprintf("https://www.google.pl/maps/search/%2.6f+%2.6f",y,x)
  
  browseURL(link)
  
}





# # # # # # #  Call for adstock functions

# source("C:/Projekty/pomocnicze/ad_stock.R")





'%!in%' <- function(x,y)!('%in%'(x,y))



# timetosaygoodby()

timetosaygoodby<-function(){
  
  if ("con" %in% ls( envir = .GlobalEnv )  )  {dbDisconnect(con)}
  
  rm(list=ls(envir = .GlobalEnv),envir =.GlobalEnv)
  
}



getScriptName<- function(){
  
  rstudioapi::getSourceEditorContext()$path
  
}

# print(":++++++++++")

# print(getScriptName())



load_packages<-function(){
  
  check.packages <- function(pkg){
    
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    
    if (length(new.pkg))
      
      install.packages(new.pkg, dependencies = TRUE)
    
    sapply(pkg, require, character.only = TRUE)
    
  }
  
  packages<-c("dplyr"  ,  "lubridate"  ,"Hmisc","stringi","openxlsx","zoo")
  
  # packages<-c("ggplot2", "sqldf", "lme4", "caret","DBI", "data.table", "readxl",
  
  #             "RPresto", "optimx", "reshape", "grid","scales", "gridExtra")
  
  (packs<-check.packages(packages))
  
  
  
  return(all(packs))
  
}



na.mean<-function(object){
  
  
  
  object[is.na(object)]<- mean(object,na.rm=T)
  
  return(object)
  
}



na.zero<-function(object){
  
  return(object %>% na.replace(.,replacement = 0))
  
}

na.large<-function(object){
  
  return(object %>% na.replace(.,replacement = 999999999999999))
  
}



na.small<-function(object){
  
  return(object %>% na.replace(.,replacement = -999999999999999))
  
}





na.replace<- function(df, replacement="colmean"){
  
  if (replacement=="colmean"){
    
    if (!all(class(df) ==c("tbl_df"   ,  "tbl"  ,     "data.frame"))) cat( "na.replace dla replacement='colmean' jest zaimplementowane tylko dla klasy 'tbl_df' ")
    
    return(df %>% lapply( na.mean) %>% tbl_df)
    
  }else{
    
    df[is.na(df)] <- replacement
    
    return(df)
    
  }
  
}



get_save_xlsx_filename<-function(folder_save){
  
  
  
  if( !endsWith((folder_save) , "/ ")) folder_save<-paste0(folder_save, "/")
  
  
  
  lista_plikow<-list.files(folder_save)
  
  lista_plikow<-lista_plikow[!grepl(pattern = "^~", x = lista_plikow)]
  
  lista_plikow<-lista_plikow[ grepl(pattern = "\\d", x = lista_plikow)]
  
  
  
  if (length(lista_plikow) == 0) full_filename<-paste0(folder_save,"aux_00001.xlsx" )
  
  nr<- substr(lista_plikow , 5, 9) %>% as.numeric() %>% max +1
  
  filename<- sprintf("aux_%05d.xlsx",nr)
  
  full_filename<-paste0(folder_save,filename )
  
  
  
  return(full_filename)
  
}

save_xlsx_view <- function (df,folder_save= AD_HOC_FILES){
  
  
  
  full_filename<- get_save_xlsx_filename(folder_save)
  
  openxlsx::write.xlsx(df,full_filename)
  
  shell.exec(full_filename)
  
}





decimal_rest<-function(x){x - floor(x)}



getCorrectedLongitude<- function(df, colnames=c("Longitude" ,"Latitude")){
  
  
  
  vec_cor_lon<-df[[colnames[1]]] /  cos((df[[colnames[2]]]/180) *pi)
  
  return(vec_cor_lon)
  
}







lonlat_to_xy <- function(dt, lon_name, lat_name){
  
  coordinates(dt) <- c(lon_name, lat_name)
  
  proj4string(dt) <- CRS('+init=epsg:4326')
  
  dt <- spTransform(dt, CRS('+init=epsg:2180'))
  
  as.data.table(dt)
  
}

R2 <- function(actual,predict) {
  
  1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2))}

MAPE<- function(obs,pred){
  
  mean(abs((pred- obs) / obs))
  
}

MAE<- function(obs,pred){
  
  mean(abs((pred- obs) ))
  
}

get_env_space<- function(){
  
  (ls(envir= .GlobalEnv) %>% lapply( ., function(x) {object.size( get(x) )}) %>% unlist %>% sum)/1024/1024
  
}



'%!in%' <- function(x,y)!('%in%'(x,y))





cz_dod<-function(x) { max(x, 0 )}





# # # File location

# "C:/Projekty/pomocnicze/funkcje.R"

install_and_load_packages <- function(required_packages){
  
  for(pkg in setdiff(required_packages, installed.packages()[,"Package"])){
    
    install.packages(pkg)
    
  }
  
  for(pkg in required_packages){
    
    library(pkg, character.only=TRUE)
    
  }
  
}



cz_dod<-function(x) {
  
  
  
  x[x<0]<-0
  
  x
  
}





define_presto_session <- function(user,
                                  
                                  output_path = '/sbdata/automated_upload/facings_20180919/',
                                  
                                  host='172.20.0.25',
                                  
                                  port=9191,
                                  
                                  schema='ks',
                                  
                                  catalog='hive'){
  
  
  
  presto_query <- function(query, save_as=NULL){
    
    con <- dbConnect(
      
      RPresto::Presto(),
      
      host=host,
      
      port=port,
      
      user=user,
      
      schema=schema,
      
      catalog=catalog
      
    )
    
    
    
    res <- data.table(dbGetQuery(con, query))
    
    dbDisconnect(con)
    
    
    
    if(!is.null(save_as)){
      
      query_date <- gsub('-', '_', Sys.Date())
      
      
      
      full_name <- paste0(query_date, '_', save_as)
      
      
      
      i <- 1
      
      while(dir.exists(paste0(output_path, full_name, '_', i))){
        
        i <- i+1
        
      }
      
      full_name <- paste0(full_name, '_', i)
      
      
      
      dir.create(paste0(output_path, full_name))
      
      
      
      fwrite(res, paste0(output_path, full_name, '/', full_name, '.csv'))
      
      
      
      fileConn <- file(paste0(output_path, full_name, '/', full_name, '.sql'))
      
      write(query, fileConn)
      
      close(fileConn)
      
      
      
      cat('Query and output saved at', paste0(output_path, full_name, '/'), '\n')
      
    }
    
    
    
    return(res)
    
  }
  
  
  
  return(
    
    presto_query
    
  )
  
}



getCorrectedLongitude<- function(df, colnames=c("Longitude" ,"Latitude")){
  
  
  
  vec_cor_lon<-df[[colnames[1]]] /  cos((df[[colnames[2]]]/180) *pi)
  
  return(vec_cor_lon)
  
}





showAtMap<-function(a,b){
  
  if (a<b ){
    
    x<- a
    
    y<-b} else {
      
      y<-a
      
      x<-b
      
    }
  
  link<- sprintf("https://www.google.pl/maps/search/%2.6f+%2.6f",y,x)
  
  browseURL(link)
  
}





# # # # # # #  Call for adstock functions

# source("C:/Projekty/pomocnicze/ad_stock.R")





'%!in%' <- function(x,y)!('%in%'(x,y))



# timetosaygoodby()

timetosaygoodby<-function(){
  
  if ("con" %in% ls( envir = .GlobalEnv )  )  {dbDisconnect(con)}
  
  rm(list=ls(envir = .GlobalEnv),envir =.GlobalEnv)
  
}



getScriptName<- function(){
  
  rstudioapi::getSourceEditorContext()$path
  
}

# print(":++++++++++")

# print(getScriptName())



load_packages<-function(){
  
  check.packages <- function(pkg){
    
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    
    if (length(new.pkg))
      
      install.packages(new.pkg, dependencies = TRUE)
    
    sapply(pkg, require, character.only = TRUE)
    
  }
  
  packages<-c("dplyr"  ,  "lubridate"  ,"Hmisc","stringi","openxlsx","zoo")
  
  # packages<-c("ggplot2", "sqldf", "lme4", "caret","DBI", "data.table", "readxl",
  
  #             "RPresto", "optimx", "reshape", "grid","scales", "gridExtra")
  
  (packs<-check.packages(packages))
  
  
  
  return(all(packs))
  
}



na.mean<-function(object){
  
  
  
  object[is.na(object)]<- mean(object,na.rm=T)
  
  return(object)
  
}



na.zero<-function(object){
  
  return(object %>% na.replace(.,replacement = 0))
  
}

na.large<-function(object){
  
  return(object %>% na.replace(.,replacement = 999999999999999))
  
}



na.small<-function(object){
  
  return(object %>% na.replace(.,replacement = -999999999999999))
  
}





na.replace<- function(df, replacement="colmean"){
  
  if (replacement=="colmean"){
    
    if (!all(class(df) ==c("tbl_df"   ,  "tbl"  ,     "data.frame"))) cat( "na.replace dla replacement='colmean' jest zaimplementowane tylko dla klasy 'tbl_df' ")
    
    return(df %>% lapply( na.mean) %>% tbl_df)
    
  }else{
    
    df[is.na(df)] <- replacement
    
    return(df)
    
  }
  
}


save_csv_email <- function (df,folder_save= AD_HOC_FILES){
  
  full_filename<- get_save_xlsx_filename(folder_save)
  full_filename<- stringi::stri_replace_all_fixed(full_filename, '.xlsx','.csv')
  write.csv2(df,full_filename)
  send_email_file(full_filename)
}
save_xlsx_email <- function (df,folder_save= AD_HOC_FILES){
  
  full_filename<- get_save_xlsx_filename(folder_save)
  openxlsx::write.xlsx(df,full_filename)
  send_email_file(full_filename)
}
send_email_file<-function(fname ){
  
  sender <- "sasiedzkisasiad@gmail.com"
  recipients <- c("sasiedzkisasiad@gmail.com")
  mailR::send.mail(from = sender,
                   to = recipients,
                   subject = "Elo Mordo",
                   body = "Elo Mordeczki",
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                               user.name = "sasiedzkisasiad@gmail.com",            
                               passwd = "s@s1edzk1s@s1@d", ssl = TRUE),
                   authenticate = TRUE,
                   attach.files =  fname,
                   send = TRUE)
}


get_save_xlsx_filename<-function(folder_save){
  
  if( !endsWith((folder_save) , "/ ")) folder_save<-paste0(folder_save, "/")
  
  lista_plikow<-list.files(folder_save)
  lista_plikow<-lista_plikow[!grepl(pattern = "^~", x = lista_plikow)]
  lista_plikow<-lista_plikow[ grepl(pattern = "\\d", x = lista_plikow)]
  
  if (length(lista_plikow) == 0) return(paste0(folder_save,"aux_00001.xlsx" ))
  nr<- substr(lista_plikow , 5, 9) %>% as.numeric() %>% max +1
  filename<- sprintf("aux_%05d.xlsx",nr)
  full_filename<-paste0(folder_save,filename )
  
  return(full_filename)
}
save_xlsx_view <- function (df,folder_save= AD_HOC_FILES){
  
  full_filename<- get_save_xlsx_filename(folder_save)
  openxlsx::write.xlsx(df,full_filename)
  shell.exec(full_filename)
}





getCorrectedLongitude<- function(df, colnames=c("Longitude" ,"Latitude")){
  
  
  
  vec_cor_lon<-df[[colnames[1]]] /  cos((df[[colnames[2]]]/180) *pi)
  
  return(vec_cor_lon)
  
}



# getScriptName()





erase_duplicated_cols<-  function(df, del = T){
  
  duplicated_cols_to_del<- names(table(colnames(df)))[(table(colnames(df))>1)]
  
  inds_of_del_col<-c()
  
  for (col_duplicated in duplicated_cols_to_del){ 
    
    inds_of_duplicated_cols<- which(colnames(df) == col_duplicated) 
    
    inds_of_del_col<- c(inds_of_del_col, inds_of_duplicated_cols[inds_of_duplicated_cols!=first  (inds_of_duplicated_cols)])
    
  }
  
  
  
  if (del ){
    
    for (iter in sort(inds_of_del_col, decreasing = T)) {
      
      df[[iter]]<- NULL
      
    }
    
    return(df)
    
  }else{
    
    return(inds_of_del_col)
    
  }
  
}











