for (i in 1:TN){
  idx_real<-which(cf_data_final$ID == test_user[i])
  
  if ( length(idx_real) != 6){idx_given<-sample(idx_real,6)}  # select 6 given 
  
  idx_eval<-idx_real[which(idx_real %in% idx_given == F)]
  
  
  idx_real_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_real,]$Restaurant)
  idx_given_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_given,]$Restaurant)
  idx_eval_set_hat[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_eval,]$Restaurant)
  idx_eval_set_cf[[i]] <- idx_eval
  
  Res<-cf_data_final$Restaurant[idx_given]  #  6 given's Res
  Rating<-cf_data_final$Rating[idx_given]   # 6 given's Rating
  
  Res_idx_mat<-matrix(NA,length(Res),2658)
  
  for ( j in 1:length(Res)){
    Res_idx<-which( cb_data$Name == Res[j] )
    Res_idx_mat[j,] <- dist_mat_full[Res_idx,]   # Our Similarity Matrix!!!!!!!!!
    #print(j)
  }
  
  sum_set<-apply(Res_idx_mat,2,sum)
  
  sum_set[which(sum_set==0)] <- 0.00001 # simil sum = 0 case. In this case, sum of expected rating is 0
  
  expected_rating<- (Rating %*% Res_idx_mat)    # matrix multiplication
  
  for( k in 1:length(sum_set)){
    expected_rating[k]<-expected_rating[k]/sum_set[k]
  }
  
  eval_frame[i,]<-expected_rating
  print(i)
}

# Simulation
CB_simulation <- function(x){
  if ( length(x) < 6 ) stop('Given Restaurants should be over 6 !!!') 
  if ( sum( names(x) %in% cb_data_after_carpentry$Name ) != length(x) ) {
    stop('Some of Restaurants are not in the cb_data_after_carpentry')}
  
  Res<- names(x) # Given 6
  Rating<- x   # Given's Rating
  
  
  Res_idx_mat<-matrix(NA,length(x),2658)
  idx_given <- NULL
  
  for ( j in 1:length(x)){
    Res_idx<-which( cb_data$Name == Res[j] )
    Res_idx_mat[j,] <- dist_mat_full[Res_idx,] # Similarity Matrix
    idx_given <- c(idx_given, Res_idx)
    #print(j)
  }
  
  sum_set<-apply(Res_idx_mat,2,sum)
  
  sum_set[which(sum_set==0)] <- 0.00001 # Similarity sum = 0 case. In this case, sum of expected rating is 0
  
  expected_rating<- (Rating %*% Res_idx_mat) # Matrix multiplication
  
  for( k in 1:length(sum_set)){
    expected_rating[k]<-expected_rating[k]/sum_set[k]
  }
  
  names(expected_rating)<-cb_data_after_carpentry$Name
  name_list<-names(expected_rating)
  
  expected_rating_without_given <- expected_rating[,-idx_given]
  names(expected_rating_without_given) <- name_list[-idx_given]
  
  # Method 1 : Recommend Top 10 (default)
  recommend_list_1<-sort(expected_rating_without_given,decreasing = T) %>% head(10)
  
  # Method 2 : Expected Rating = 5
  recommend_list_2<-expected_rating_without_given [ which (expected_rating_without_given==5)]
  
  recommend_list_final<-data.frame(Restaurants =  names(recommend_list_1), Expected_rating = as.numeric(recommend_list_1) )
  return(recommend_list_final)
  
}