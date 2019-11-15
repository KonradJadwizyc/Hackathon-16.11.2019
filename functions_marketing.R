
smooth_simple <- function(vec, p = .5){
  vec_pom<- vec
  if (length(vec) <=1 ) stop('za malo obserwacji')
  for( i in 2:length(vec)){
    vec_pom [i] = vec[i] + vec_pom[i-1] * p
    
  }
  return(vec_pom)
}

norm_0_1<-function(vec){
  if (sd(vec) == 0 ) return (vec)
  
  vec <- vec - min (vec)
  vec <- vec / max( vec)
  return(vec)
  
}



smooth_exp <- function(vec, p = .5, lr = .1){
  
  vec_pom<- c(0,norm_0_1(vec))
  vec<-c(0,vec)
  if (length(vec) <=1 ) stop('za malo obserwacji')
  for( i in 2:length(vec_pom)){
    # for( i in 2:6){
    vec_pom [i] =1- exp ( - lr * vec[i] )  +   vec_pom[i-1] * p
    
  }
  return(vec_pom[-1])
}




smooth_log <- function(vec, p = .5){
  vec[vec==0] <- max(vec) / 1000
  vec_pom<- log(vec)
  if (length(vec) <=1 ) stop('za malo obserwacji')
  for( i in 2:length(vec)){
    # for( i in 2:6){
    vec_pom [i] =log(vec[i])  +   vec_pom[i-1] * p
    
  }
  return(vec_pom)
}