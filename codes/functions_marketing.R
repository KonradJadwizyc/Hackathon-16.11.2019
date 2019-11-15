
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




getSigmoidForOneVar<- function(v1, v2=NULL, target_vec, n_rand = 1000){
  
  vec_max_norm<-(v1-mean(v1))/sd(v1)
  
  mult_random<- runif(n_rand, 0,2 )
  ind<- lapply( mult_random, function(x) {cor(sigmoid(vec_max_norm *x,1 ,0) , target_vec)}) %>% unlist %>% abs %>% which.max()
  vec_max_norm<-vec_max_norm * mult_random[ind]
  print(cor(vec_max_norm,target_vec))
  print(mult_random[ind])
  
  # if (!  ( is.null(v2) )){
  #   vec_min_norm<-(vec_min_var-mean(vec_min_var))/sd(vec_min_var)
  # 
  #   mult_random<- runif(n_rand, 0,2 )
  #   ind<- lapply( mult_random, function(x) {cor(sigmoid(vec_max_norm,1 ,0)*sigmoid(vec_min_norm*x,1 ,0) , target_vec)}) %>% unlist %>% abs %>% which.max()
  #    print(mult_random[ind])
  #    
  #    vec_joined<-(sigmoid(vec_max_norm,1 ,0)*sigmoid(vec_min_norm*mult_random[ind],1 ,0))
  # }
  # 
  
  
  return(vec_max_norm)
}


getSigmoidForTwoVars<- function(v1, v2=NULL, target_vec, n_rand = 1000){
  
  
  if( is.null(v2) ) { 
    vec_max_var<- v1
  }else{
    if (var(v1 ) >var(v2)) {
      vec_max_var<- v1
      vec_min_var<- v2
    }else{
      vec_max_var<- v2
      vec_min_var<- v1
    }
  }
  
  vec_max_norm<-(vec_max_var-mean(vec_max_var))/sd(vec_max_var)
  
  
  
  mult_random<- cbind(runif(n_rand, 0,2 ),runif(n_rand, 0,2 ))
  ind<- lapply( seq_len(nrow(mult_random)), function(x) {cor(sigmoid(vec_max_norm *mult_random[x,1],1 ,0)*sigmoid(vec_max_norm *mult_random[x,2],1 ,0) , target_vec)}) %>% unlist %>% abs %>% which.max()
  vec_max_norm<-vec_max_norm * mult_random[ind]
  out_vec<- sigmoid(vec_max_norm *mult_random[ind,1],1 ,0)*sigmoid(vec_max_norm *mult_random[ind,2],1 ,0)
  
  return(out_vec)
}