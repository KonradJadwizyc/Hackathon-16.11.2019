# # # Projekt konkursowy na hackaton dn. 2019-11-16 organizowany przez firme Analyx
# # # Druzyna: Sasiedzi
# W skladzie: 
# Paulina Kalinowska
# Adam Ewert-Krzemieniewski
# Konrad Jadwizyc
# Krzysztof Sikorski



# Znalezienie root
      if ('stringi' %in% installed.packages()[,1]) {library(stringi)}else{
        install.packages('stringi')
        library(stringi)
      }
      cur_dir<- getwd()
      if( !endsWith(cur_dir, '/'))  { cur_dir<-paste0(cur_dir,'/')}
      ROOT<- stri_replace_all_fixed( cur_dir, 'codes/', '')
      rm(cur_dir)
      if (file.exists(paste0(ROOT, 'codes/config/setup_personal.R' ))){
      source(paste0(ROOT, 'codes/config/setup_personal.R' ))}else{
        warning('zapytaj Krzycha o mozliwosc wysylania maila automatycznie')
      }



# # # Ustawienia sciezek wzglednych 

ROOT_CODES <- paste0(ROOT , 'codes/')
ROOT_DATA  <- paste0(ROOT , 'data/')
ROOT_OUTPUT<- paste0(ROOT , 'output/')

source(paste0(ROOT_CODES,'functions.R'))
source(paste0(ROOT_CODES,'packages.R'))




