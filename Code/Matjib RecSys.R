my_matjib <- function(){
  
  try=NULL; rate=NULL; try_all=NULL; rate_all=NULL
  
  repeat {
    try <- sample(setdiff(cb_data_after_carpentry$Name,try),1) 
    rate <- readline(prompt = paste("Rate your perference from 1 to 5. If you want to skip, write SKIP. Your rate for",try,"is: "))
    
    if(rate == "SKIP"){try = NULL;rate = NULL}
    
    try_all <- c(try_all, try)
    rate_all <- c(rate_all, rate)
    
    if ( length(rate_all) >= 6){
      print("Now you are ready! Thank you for your opinion :)")
      break
    } else print("Sorry, We need more information. Let's try more!")
    
  }
  
  
  x <- rep(NA,2658) %>% as.numeric(); index=NULL
  rate_all <- rate_all %>% as.integer()
  
  for (i in 1:length(try_all)){
    index <- c(index,which(cb_data_after_carpentry$Name == try_all[i] ))
    x[index[i]] = rate_all[i]
  }
  
  mydata <<- x
  names(mydata) <<- cb_data_after_carpentry$Name
}

# Final Function
matjib <- function(x, n, method="UBCF", cafeout = F){
  
  if (method == "UBCF"){
    
    x_u <- x %>% as.matrix() %>% t
    x_u <- as(x_u,'realRatingMatrix')
    UBCF <- Recommender(r, method="UBCF", parameter="Pearson")
    p <- predict(UBCF, x_u, data=r, type="ratings")
    
    expected_rating <- p@data@x 
    names(expected_rating) <- setdiff(p@data@Dimnames[[2]], names(x[!is.na(x)]))
    
    recommend_list <- expected_rating %>% sort(decreasing = T) %>% head(n)
    
  } else if (method == "IBCF") {
    
    x_i <- x %>% as.matrix() %>% t
    x_i <- as(x_i,'realRatingMatrix')
    IBCF <- Recommender(r, method="IBCF", parameter="Pearson")
    p <- predict(IBCF, x_i, data=r, type="ratings")
    
    expected_rating <- p@data@x 
    names(expected_rating) <- setdiff(p@data@Dimnames[[2]],names(x[!is.na(x)]))
    
    recommend_list <- expected_rating %>% sort(decreasing = T) %>% head(n)
    
  } else if (method == "CBF") {
    
    x_c <- x[!is.na(x)]
    
    Res<- names(x_c) #  6 given's Res
    Rating<- x_c   # 6 given's Rating
    
    Res_idx_mat<-matrix(NA,length(x_c),2658)
    idx_given <- NULL
    
    for ( j in 1:length(x_c)){
      Res_idx<-which( cb_data_after_carpentry$Name == Res[j] )
      Res_idx_mat[j,] <- dist_mat_full[Res_idx,]
      idx_given <- c(idx_given, Res_idx)
      #print(j)
    }
    
    sum_set<-apply(Res_idx_mat,2,sum)
    sum_set[which(sum_set==0)] <- 0.00001 # simil sum = 0 case. In this case, sum of expected rating is 0
    
    expected_rating<- (Rating %*% Res_idx_mat)    # matrix multiplication
    
    for( k in 1:length(sum_set)){
      expected_rating[k] <- expected_rating[k]/sum_set[k]
    }
    
    expected_rating_without_given <- expected_rating[,-idx_given]
    names(expected_rating_without_given) <- cb_data_after_carpentry$Name[-idx_given]
    
    recommend_list <- sort(expected_rating_without_given,decreasing = T) %>% head(n)
    
  } else if (method =="ALL"){  
    
    #cf
    x_u <- x %>% as.matrix() %>% t
    x_u <- as(x_u,'realRatingMatrix')
    UBCF <- Recommender(r, method="UBCF", parameter="Pearson")
    p <- predict(UBCF, x_u, data=r, type="ratings")
    
    expected_rating_u <- p@data@x 
    names(expected_rating_u) <- setdiff(p@data@Dimnames[[2]], names(x[!is.na(x)]))
    
    #cb
    x_c <- x[!is.na(x)]
    
    Res<- names(x_c) #  6 given's Res
    Rating<- x_c   # 6 given's Rating
    
    Res_idx_mat<-matrix(NA,length(x_c),2658)
    idx_given <- NULL
    
    for ( j in 1:length(x_c)){
      
      Res_idx <- which( cb_data_after_carpentry$Name == Res[j] )
      Res_idx_mat[j,] <- dist_mat_full[Res_idx,] 
      idx_given <- c(idx_given, Res_idx)
    }
    
    sum_set <- apply(Res_idx_mat,2,sum)
    sum_set[which(sum_set==0)] <- 0.00001 
    
    expected_rating <- (Rating %*% Res_idx_mat)  
    
    for( k in 1:length(sum_set)){
      expected_rating[k] <- expected_rating[k]/sum_set[k]
    }
    
    expected_rating_c <- expected_rating[,-idx_given]
    names(expected_rating_c) <- cb_data_after_carpentry$Name[-idx_given]
    
    #merge
    recommend_set <- rbind(expected_rating_u,expected_rating_c)
    recommend_list <- recommend_set %>% apply(2,mean) %>% 
      sort(decreasing = T) %>% head(n)
    
  } else print("Please write down correct method")
  
  if (cafeout == T) {
    cafe_list <- cb_data$Name[which( cb_data$Menu2 == "카페/커피숍" )]
    recommend_cafe <- names(recommend_list) %in% cafe_list
    recommend_list <- recommend_list[as.logical(1-recommend_cafe)]
  }
  
  recommend_list_final <- data.frame(Restaurants = names(recommend_list), 
                                     Expected_rating = as.numeric(recommend_list) )
  return (recommend_list_final)
  
}

my_matjib() #Now you can use 'mydata' for x!!!!

matjib(mydata, 5, method="UBCF", cafeout = T)
matjib(mydata, 9, method="UBCF", cafeout = T)
matjib(mydata, 5, method="ALL", cafeout = T)