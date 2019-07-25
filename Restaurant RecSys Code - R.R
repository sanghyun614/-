# Matjib Recommendatin System

# Data Carpentry ----------------------------------------------------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(Matrix)
library(recommenderlab)

# CF data Carpentry
cf_data <- fread("cf_data.csv") # nrow = 144544, ID = 8505, Restaurant = 17134

cf_data <- cf_data[complete.cases(cf_data), ] #104448, ID = 6718, Restaurant = 15849
cf_data <-  cf_data %>% group_by(ID,Restaurant) %>% summarize(Rating=max(Rating)) %>% ungroup #104042, ID = 6718, Restaurant = 15849
cf_data <- as.data.frame(cf_data)

r <- as(cf_data, 'realRatingMatrix') # 6718 x 15849
r <- r[,colCounts(r) > 5] # 6718 x 3849
r <- r[rowCounts(r) > 5] # 1427 x 3849
dim(r)

# CB data Carpentry
cb_data <- fread("cb_data.csv") # Restaurant = 29352

cb_data <- cb_data %>% group_by(Name) %>% slice(which.max(Rating)) # Step1) 중복된 식당 중 평점이 높은 식당만 남김 (27065개)

cb_data <- cb_data %>% filter(Rating != 0) # Step2) 평점이 0인 애들은 삭제

cb_data <- cb_data %>% filter( nchar(About) >= 100 ) # Step3) About이 100자 이상인 애들 

left_over_Restaurant <- cb_data$Name # 남은 식당의 이름

cf_data_copy <- cf_data[cf_data$Restaurant %in% left_over_Restaurant, ] # CB에서 남은 식당만으로 CF 데이터 구축  

Res_list_for_cb <- cf_data_copy %>% group_by(Restaurant) %>% summarize(count=n()) %>% filter(count > 5) %>% select(Restaurant) 
# 6번 이상 리뷰를 받은 식당

cf_data_copy2 <- cf_data_copy[cf_data_copy$Restaurant %in% Res_list_for_cb$Restaurant, ]

ID_list_for_cb_eval <- cf_data_copy2 %>% group_by(ID) %>% summarize(count=n()) %>% filter(count > 6) # 6번 이상 리뷰한 애들
Res_list_based_on_ID <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ] %>% select(Restaurant) %>% unique 
# 6번 이상 리뷰한 애들이 간 식당

cf_data_final <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ]

cb_data <- cb_data[cb_data$Name %in% Res_list_based_on_ID$Restaurant, ] # 6번 이상 리뷰한 애들이 간 식당만 남김

write.csv(cb_data, "cb_data_after_carpentry.csv", row.names=F)

## CB Text Mining

library(slam)
library(data.table)
library(bit64)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rlang)
library(tm)
library(stopwords)
library(caret)
library(stringr)
library(SnowballC)
library(readr)

# Read Data
cb_data_3 <- fread("cb_data_about_eng.csv")
relist <- fread("cb_data_after_carpentry.csv")

relist_name <- relist$Name
cb_data_3 <- cb_data_3[cb_data_3$Name %in% relist_name, ]

# Text Wrangling
about<-cb_data_3$About
length(about)

cb_data_3$No <- paste("No", 1:nrow(cb_data_3),sep="")

names(about) <- paste("No", 1:length(about), sep="")

docs.corp <- Corpus(VectorSource(about))

N_list<-length(about)

for ( i in N_list){
  docs.corp[[i]]$content <- paste(docs.corp[[i]]$content,collapse = " ")
}

## Lowercase
docs.corp <- tm_map(docs.corp, content_transformer(tolower))

## Remove special characters
docs.corp <- tm_map(docs.corp, removePunctuation)
for (j in 1:N_list) docs.corp[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_?ӡ???????]", " ", docs.corp[[j]]) 
for (j in 1:N_list) docs.corp[[j]] <- gsub("¡¯", " ", docs.corp[[j]]) 
for (j in 1:N_list) docs.corp[[j]] <- gsub("¡±", " ", docs.corp[[j]]) 
for (j in 1:N_list) docs.corp[[j]] <- gsub("¡°", " ", docs.corp[[j]]) 
for (j in 1:N_list) docs.corp[[j]] <- gsub("\"", " ", docs.corp[[j]]) 
for (j in 1:N_list) docs.corp[[j]] <- gsub(" NA", "", docs.corp[[j]]) #NA
for (j in 1:N_list) docs.corp[[j]] <- gsub("\t", "", docs.corp[[j]]) #\t
for (j in 1:N_list) docs.corp[[j]] <- gsub("\'", "", docs.corp[[j]])

## Remove numbers
docs.corp <- tm_map(docs.corp, removeNumbers)

## Remove stopwords
for (i in 1:N_list){docs.corp[[i]]<-removeWords(as.character(docs.corp[[i]]), stopwords("en"))}
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can");
docs.corp <- tm_map(docs.corp, removeWords, newstopwords)

## Stemming
docs.corp <- tm_map(docs.corp, stemDocument)

## Remove blank spaces
docs.corp <- tm_map(docs.corp, stripWhitespace)

# TF-IDF 
dtm_About <- DocumentTermMatrix(docs.corp, control=list(weighting = weightTfIdf))

Res <- dtm_About$dimnames$Docs
word <- dtm_About$dimnames$Terms

# Create empty dataframe
final_cb <- data.frame( No = Res, key_word_1 = rep(NA,length(Res)),
                        key_word_2 = rep(NA,length(Res)),key_word_3 = rep(NA,length(Res)),
                        key_word_4 = rep(NA,length(Res)),key_word_5 = rep(NA,length(Res)))
final_cb <-final_cb %>% mutate_all(as.character)
str(final_cb)

# Fill in the Blank
for (i in 1:length(Res)) {
  final_cb[i,c(2:6)] <- colnames(as.matrix(dtm_About[i,]))[order(as.matrix(dtm_About[i,]),decreasing=T)][1:5]
}

# Merge with original CB data

Final_data_eng <- cbind(cb_data_3,final_cb)
Final_data_eng <- Final_data_eng[,-c(5,6,7)]
Final_data_eng <- Final_data_eng %>% separate(Location, c("Location1", "Location2"), sep="> ")
Final_data_eng <- Final_data_eng %>% separate(Menu, c("Menu1", "Menu2"), sep="> ")
View(Final_data_eng[1:10,])
write.csv(Final_data_eng, "CB_data_eng.csv", row.names=F)

# Calculating Distance -----------------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(proxy)

cb_data <- fread("CB_data_eng.csv")
glimpse(cb_data)
cb <- cb_data[,-c(1,2)]
glimpse(cb)

cb_copy <- cb
dist_mat <- matrix(NA, 2658,2658)

for (i in 1:nrow(cb)) {
  for (j in 1:(nrow(cb)-i)) {
    cb_copy <- cb
    onehot_filter <- cb[i,]
    cb_copy[i+j,1] <- ifelse(cb_copy[i+j,1]==onehot_filter[,1], "1", "0")
    cb_copy[i+j,2] <- ifelse(cb_copy[i+j,2]==onehot_filter[,2], "1", "0")
    cb_copy[i+j,3] <- ifelse(cb_copy[i+j,3]==onehot_filter[,3], "1", "0")
    cb_copy[i+j,4] <- ifelse(cb_copy[i+j,4]==onehot_filter[,4], "1", "0")
    cb_copy[i+j,5] <- ifelse(cb_copy[i+j,5] %in% onehot_filter[,5:9], "1", "0")
    cb_copy[i+j,6] <- ifelse(cb_copy[i+j,6] %in% onehot_filter[,5:9], "1", "0")
    cb_copy[i+j,7] <- ifelse(cb_copy[i+j,7] %in% onehot_filter[,5:9], "1", "0")
    cb_copy[i+j,8] <- ifelse(cb_copy[i+j,8] %in% onehot_filter[,5:9], "1", "0")
    cb_copy[i+j,9] <- ifelse(cb_copy[i+j,9] %in% onehot_filter[,5:9], "1", "0")
    standard_vector <- rep(1,9)
    dist_mat[i+j,i] <- simil(rbind(standard_vector,as.numeric(cb_copy[i+j,])), method="Cosine", by_rows=T)
  }
  print(i)
}

# Recommendation System  ---------------------------------------------------------------------------------------------------------------
# CF Modeling --------------------------------------------------------------------------------------------------------------------------------------------

cf_data <- fread(file.choose())
cf_data <- cf_data[complete.cases(cf_data),]

cf_data <-  cf_data %>% group_by(ID,Restaurant) %>% 
  summarize(Ratingss=max(Rating))

cf_data_ID <- cf_data %>% group_by(ID) %>% summarize(count=n()) %>% 
  select(ID) %>% as.matrix %>% as.vector
cf_data_Res <- cf_data %>% group_by(Restaurant) %>% summarize(count=n()) %>%
  select(Restaurant) %>% as.matrix %>% as.vector

# Delete filter 

cf_data_new <- subset(cf_data, ID %in% cf_data_ID & Restaurant %in% cf_data_Res)
cf_data_new <- cf_data_new[complete.cases(cf_data_new)]

cf_data_final <- as.data.frame(cf_data_new )

rm(cf_data_ID,cf_data_Res,cf_data,cf_data_new)

# Final data
r <- as(cf_data_final, 'realRatingMatrix')
r <- r[,colCounts(r) > 5]
r <- r[rowCounts(r) > 5]
dim(r)

# Visualization (not important)
s_users <- similarity(r[1:20,], method = "cosine", which = "users")
image(as.matrix(s_users))
s_items <- similarity(r[,1:20], method = "cosine", which = "items")
image(as.matrix(s_items))

# Visualization (top users, top restaurants)
min_rest <- quantile(rowCounts(r), 0.99)
min_users<- quantile(colCounts(r), 0.99)

image(r[rowCounts(r) > min_rest, colCounts(r) > min_users])

# Remove Again
rm(min_rest, min_users, s_items, s_users)

#collaborate with cb, check number of restaurant 7710
#simulation function / f: readline, read.csv
#final due to Friday

# Making Model(split version)

traintestset <- evaluationScheme(r, method='split',train=0.8, given=6, goodRating=4, k=3) 

# UBCF version
UBCF <- Recommender(getData(traintestset , "train"), method="UBCF", parameter="Cosine")

p1 <- predict(UBCF, getData(traintestset , "known"), type="ratings")
ratinglist <- as.matrix(p1@data)
head(ratinglist[,1:10])

UBCF_E <- calcPredictionAccuracy(p1, getData(traintestset , "unknown"), byUser=FALSE)
UBCF_E

# IBCF version
IBCF <- Recommender(getData(traintestset , "train"), method="IBCF", parameter="Cosine")

p2 <- predict(IBCF, getData(traintestset , "known"), type="ratings")

IBCF_E <- calcPredictionAccuracy(p2, getData(traintestset , "unknown"))
IBCF_E

# Evaluation with graphs


alCF <- list("item-based CF_cosine" = list(name="IBCF",
                                           param=list(method="Cosine")),
             "item-based CF_pearson" = list(name="IBCF",
                                            param=list(method="Pearson")),
             "user-based CF_cosine" = list(name="UBCF",
                                           param=list(method="Cosine")),
             "user-based CF_pearson" = list(name="UBCF",
                                            param=list(method="Pearson")))

traintestset <- evaluationScheme(r, method='cross-validation', train=0.8,
                                 given=5, goodRating=4, k=4)

result1 <- evaluate(x = traintestset, 
                    method = alCF, 
                    n = seq(10, 100, by=10))

result2 <- evaluate(x = traintestset, 
                    method = alCF, 
                    type = "ratings")

# Result

par(mfrow=c(1,2))
plot(result1, annotate = c(1,4), legend = "topleft", main = "ROC curve")
plot(result1, "prec/rec", annotate = c(1,4), legend = "bottomright", main = "Precision-Recall")

# Item-based: finding k - number of items to consider 

traintestset <- evaluationScheme(r, method='cross-validation', train=0.8,
                                 given=5, goodRating=4, k=4)

vector_k <- c(5, 10, 30)

mod1 <- lapply(vector_k, function(k, l) { list(name = "IBCF", parameter = list(method = "cosine", k = k)) })
names(mod1) <- paste0("IBCF_cos_k_", vector_k)
mod2 <- lapply(vector_k, function(k, l) { list(name = "IBCF", parameter = list(method = "pearson", k = k )) })
names(mod2) <- paste0("IBCF_pea_k_", vector_k)

mod_I <- append(mod1, mod2) 

# Takes a lot of time!
list_results <- evaluate(x = traintestset, 
                         method = mod_I,
                         n = seq(10, 100, by=10))

# Graph
par(mfrow=c(1,2))
plot(list_results, annotate = 1, legend = "topleft", main = "ROC curve")
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright", main = "Precision-Recall")

# CB Modeling --------------------------------------------------------------------------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(recommenderlab)

# CF data Carpentry
cf_data <- fread("cf_data.csv") # nrow = 144544, ID = 8505, Restaurant = 17134

cf_data <- cf_data[complete.cases(cf_data), ] #104448, ID = 6718, Restaurant = 15849
cf_data <-  cf_data %>% group_by(ID,Restaurant) %>% summarize(Rating=max(Rating)) %>% ungroup #104042, ID = 6718, Restaurant = 15849
cf_data <- as.data.frame(cf_data)

r <- as(cf_data, 'realRatingMatrix') # 6718 x 15849
r <- r[,colCounts(r) > 5] # 6718 x 3849
r <- r[rowCounts(r) > 5] # 1427 x 3849
dim(r)

# CB data Carpentry
cb_data <- fread("cb_data.csv") # Restaurant = 29352

cb_data <- cb_data %>% group_by(Name) %>% dplyr::slice(which.max(Rating)) # Step1) 중복된 식당 중 평점이 높은 식당만 남김 (27065개)

cb_data <- cb_data %>% filter(Rating != 0) # Step2) 

cb_data <- cb_data %>% filter( nchar(About) >= 100 ) # Step3) About이 100자 이상인 애들 

left_over_Restaurant <- cb_data$Name # 남은 식당의 이름

cf_data_copy <- cf_data[cf_data$Restaurant %in% left_over_Restaurant, ] # CB에서 남은 식당만으로 CF 데이터 구축  

Res_list_for_cb <- cf_data_copy %>% group_by(Restaurant) %>% summarize(count=n()) %>% filter(count > 5) %>% select(Restaurant) # 6번 이상 리뷰를 받은 식당

cf_data_copy2 <- cf_data_copy[cf_data_copy$Restaurant %in% Res_list_for_cb$Restaurant, ]

ID_list_for_cb_eval <- cf_data_copy2 %>% group_by(ID) %>% summarize(count=n()) %>% filter(count > 6) # 6번 이상 리뷰한 애들
Res_list_based_on_ID <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ] %>% select(Restaurant) %>% unique # 6번 이상 리뷰한 애들이 간 식당

cf_data_final <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ]

cb_data <- cb_data[cb_data$Name %in% Res_list_based_on_ID$Restaurant, ] # 6번 이상 리뷰한 애들이 간 식당만 남김

# Evaluation
# (given=6)

cb_data_after_carpentry <- fread("cb_data_after_carpentry.csv")

# SIMIL matrix 

#FITB<-seq(0,1,length.out = 10000)
#simil_fake <- matrix(sample(FITB,2658*2658,replace = T) ,nrow=2658)  # simil fake
#diag(simil_fake)<-1


#dist_mat_lower<-fread('dist_mat.csv')  # make dist_mat to symmatric matrix 
#for ( i in 1:nrow(dist_mat_lower)) {
#idx<-which(is.na(dist_mat_lower[i,])==T )
#dist_mat_lower[i,idx]<-0

#}
#dist_mat_upper<-t(dist_mat_lower)
#dist_mat<- dist_mat_lower + dist_mat_upper
#diag(dist_mat)<-1
# write.csv(dist_mat,'dist_mat_full.csv',row.names = F)

dist_mat<-fread("dist_mat_full.csv")
dist_mat_full<-as.matrix(dist_mat)

# Take users from CF
user_n<-nlevels(factor(cf_data_final$ID)) #length(unique(cf_data_final$ID))
selected_user<-levels(factor(cf_data_final$ID))

# Test (if TN = 300 )

TN= 300
# set.seed(1234)
test_user<-selected_user[sample(user_n,TN)]
idx_real_set<-vector(TN,mode = "list")
idx_given_set<-vector(TN,mode = "list")

idx_eval_set_hat<-vector(TN,mode = "list")  
idx_eval_set_cf<-vector(TN,mode = "list")  # index for caculating error 

eval_frame<-as.data.frame(matrix(NA,TN,2658))  # our final Eval frame! TN x Restaurant matrix

colnames(eval_frame)<- cb_data_after_carpentry$Name 

# Contents-Based Filtering Function

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

eval_frame<-eval_frame %>% as_tibble

# Calculate Error

# MSE
mse_bag<-rep(NA,TN)

for ( i in 1:TN){
  index_cf<-idx_eval_set_cf[[i]]
  index_cb<-idx_eval_set_hat[[i]]
  mse_bag[i]<-sum(( cf_data_final$Rating[index_cf] - eval_frame[i,index_cb] )^2)
}
big_N<-0
for ( i in 1:TN){
  N<-length(idx_eval_set_cf[[i]])
  big_N <- N + big_N
}

cb_mse<-sum(mse_bag)/big_N;cb_mse  ## Final MSE


# MAE
mae_bag<-rep(NA,TN)

for ( i in 1:TN){
  index_cf<-idx_eval_set_cf[[i]]
  index_cb<-idx_eval_set_hat[[i]]
  mae_bag[i]<-sum(abs( cf_data_final$Rating[index_cf] - eval_frame[i,index_cb] ))
}
big_N<-0
for ( i in 1:TN){
  N<-length(idx_eval_set_cf[[i]])
  big_N <- N + big_N
}

cb_mae<-sum(mae_bag)/big_N;cb_mae  ## Final MSE

## Precision / Recall ##
# criterion Rating : 4

cf_data_con<-cf_data_final
eval_frame_con<-eval_frame

cf_data_con$Rating[which(cf_data_con$Rating < 4 )] <- 0
cf_data_con$Rating[which(cf_data_con$Rating >=4 )] <-1


confusion_table<-as_tibble(matrix( NA,TN,4 ))
colnames(confusion_table) <- c('TT','TF','FT','FF')

for ( i in 1:TN){
  index_cf<-idx_eval_set_cf[[i]]
  index_cb<-idx_eval_set_hat[[i]]
  
  eval_frame_con[i,index_cb] [which ( eval_frame_con[i,index_cb] <4 )] <- 0
  eval_frame_con[i,index_cb] [which ( eval_frame_con[i,index_cb] >=4 )] <- 1
  
  sum_set<- cf_data_con$Rating[index_cf] + eval_frame_con[i,index_cb] 
  confusion_table$TT[i]<-length(which(sum_set==2) ) ## TT
  confusion_table$FF[i]<-length(which(sum_set==0) ) ## FF
  
  minus_set<- cf_data_con$Rating[index_cf] - eval_frame_con[i,index_cb] 
  confusion_table$TF[i]<-length(which(minus_set==1) ) ## TF
  confusion_table$FT[i]<-length(which(minus_set==-1) ) ## FT
  
}

head(confusion_table)
TT<-sum(confusion_table$TT)
TF<-sum(confusion_table$TF)
FT<-sum(confusion_table$FT)
FF<-sum(confusion_table$FF)

# Making confusiom Matrix
confusion_matrix<-matrix(c(TT,TF,FT,FF),2,byrow = T) 
row.names(confusion_matrix)<-c('Real_T','Real_F')
colnames(confusion_matrix) <- c('Model_T','Model_F')

# Precision
cb_precision<-TT/(TT + FT )

# Recall
cb_recall<-TT/(TT + TF )

# Accuracy
cb_accuracy<-(TT+FF)/(TT+TF+FT+FF)

# F1-score
cb_F1score<-(2*cb_precision*cb_recall) / (cb_precision + cb_recall)

## Confusion Matrix
CB_confusion_matrix<-list(Confusion_matrix=confusion_matrix,
                          Summary = c(Precision = cb_precision, Recall = cb_recall,
                                      Accuracy = cb_accuracy, F1_Score = cb_F1score))
CB_confusion_matrix

### CB FINAL SCORE ###

CB_FINAL_SOCRE<-list(Model = 'Content Based Filtering' ,
                     Test_sample = TN, Scoring_criteria = 4,
                     Evaluation_N = big_N,
                     Given = 6,
                     Confusion_matrix=confusion_matrix,
                     Summary = c(Precision = cb_precision, Recall = cb_recall,
                                 Accuracy = cb_accuracy, F1_Score = cb_F1score),
                     MAE = cb_mae, MSE = cb_mse)

CB_FINAL_SOCRE

table(cf_data_con$Rating)
sd(cf_data_con$Rating)
range(eval_frame[1:nrow(eval_frame),])

# Simulation

CB_simulation <- function(x){
  if ( length(x) < 6 ) stop('Given Restaurants should be over 6 !!!') 
  if ( sum( names(x) %in% cb_data_after_carpentry$Name ) != length(x) ) {
    stop('Some of Restaurants are not in the cb_data_after_carpentry')}
  
  Res<- names(x) #  6 given's Res
  Rating<- x   # 6 given's Rating
  
  
  Res_idx_mat<-matrix(NA,length(x),2658)
  idx_given <- NULL
  
  for ( j in 1:length(x)){
    Res_idx<-which( cb_data$Name == Res[j] )
    Res_idx_mat[j,] <- dist_mat_full[Res_idx,]   # Our Similarity Matrix!!!!!!!!!
    idx_given <- c(idx_given, Res_idx)
    #print(j)
  }
  
  sum_set<-apply(Res_idx_mat,2,sum)
  
  sum_set[which(sum_set==0)] <- 0.00001 # simil sum = 0 case. In this case, sum of expected rating is 0
  
  expected_rating<- (Rating %*% Res_idx_mat)    # matrix multiplication
  
  for( k in 1:length(sum_set)){
    expected_rating[k]<-expected_rating[k]/sum_set[k]
  }
  
  names(expected_rating)<-cb_data_after_carpentry$Name
  name_list<-names(expected_rating)
  
  expected_rating_without_given <- expected_rating[,-idx_given]
  names(expected_rating_without_given) <- name_list[-idx_given]
  
  # 자 갈림길이다. 
  # 방법 1 : 상위 30개 추천이다.(지금 default)
  recommend_list_1<-sort(expected_rating_without_given,decreasing = T) %>% head(10)
  
  # 방법 2 : 예측평점 5인 놈들
  recommend_list_2<-expected_rating_without_given [ which (expected_rating_without_given==5)]
  
  recommend_list_final<-data.frame(Restaurants =  names(recommend_list_1), Expected_rating = as.numeric(recommend_list_1) )
  return(recommend_list_final)
  
}

# Test1 : 평점 9개 입력한 유저
test_user_1 <- c(4,5,2,3,1,4,5,3,5)
names(test_user_1) <- c('보나세라','뱅가','리북집','무삼면옥','마실','동명','장모님해장국','자하손만두','인천식당')

CB_simulation(test_user_1)

# Test2 : 평점 6개 입력한 유저
test_user_2 <- c(4,5,2,3,1,4)
names(test_user_2) <- c('묵호회집','미가미','리북집','벽제갈비','노아베이커리','동명')

CB_simulation(test_user_2)

# Test3 : 평점 4개 입력한 유저 (오류 1번)
test_user_3 <- c(4,5,2,3)
names(test_user_3) <- c('묵호회집','미가미','리북집','벽제갈비')

CB_simulation(test_user_3)

# Test4 :  cb_data_after_carpentry 에 없는 식당 입력한 유저 (오류 2번)

test_user_4 <- c(4,5,2,3,1,2,4)
names(test_user_4) <- c('한상현 못생김','강지원 멍청이','전희연 거짓말쟁이',
                        '안우진 바보','백우현 통계 마스터','다음 회식은','어디로갈까')

CB_simulation(test_user_4)

# Evaluation --------------------------------------------------------------------------------------------------------------------------------------------

# CF data Carpentry
cf_data <- fread("cf_data.csv") # nrow = 144544, ID = 8505, Restaurant = 17134

cf_data <- cf_data[complete.cases(cf_data), ] #104448, ID = 6718, Restaurant = 15849
cf_data <-  cf_data %>% group_by(ID,Restaurant) %>% summarize(Rating=max(Rating)) %>% ungroup #104042, ID = 6718, Restaurant = 15849
cf_data <- as.data.frame(cf_data)

# CB data Carpentry
cb_data <- fread("cb_data.csv") # Restaurant = 29352

cb_data <- cb_data %>% group_by(Name) %>% dplyr::slice(which.max(Rating)) # Step1) 중복된 식당 중 평점이 높은 식당만 남김 (27065개)

cb_data <- cb_data %>% filter(Rating != 0) # Step2) 

cb_data <- cb_data %>% filter( nchar(About) >= 100 ) # Step3) About이 100자 이상인 애들 

left_over_Restaurant <- cb_data$Name # 남은 식당의 이름

cf_data_copy <- cf_data[cf_data$Restaurant %in% left_over_Restaurant, ] # CB에서 남은 식당만으로 CF 데이터 구축  

Res_list_for_cb <- cf_data_copy %>% group_by(Restaurant) %>% summarize(count=n()) %>% filter(count > 5) %>% select(Restaurant) # 6번 이상 리뷰를 받은 식당

cf_data_copy2 <- cf_data_copy[cf_data_copy$Restaurant %in% Res_list_for_cb$Restaurant, ]

ID_list_for_cb_eval <- cf_data_copy2 %>% group_by(ID) %>% summarize(count=n()) %>% filter(count > 6) # 6번 이상 리뷰한 애들
Res_list_based_on_ID <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ] %>% select(Restaurant) %>% unique # 6번 이상 리뷰한 애들이 간 식당

cf_data_final <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ]

cb_data <- cb_data[cb_data$Name %in% Res_list_based_on_ID$Restaurant, ] # 6번 이상 리뷰한 애들이 간 식당만 남김

#final data
r <- as(cf_data_final, 'realRatingMatrix')
r <- r[rowCounts(r) > 5]
dim(r)

#making model(split version)

set.seed(9)
traintestset <- evaluationScheme(r, method='split',train=0.8, given=6, goodRating=4, k=3) 
IDlist_CF <- getData(traintestset , "known")@data@Dimnames[[1]]

# UBCF version ------------------------------------------------------------------------------------------------------------------------------------
UBCF <- Recommender(getData(traintestset , "train"), method="UBCF", parameter="Cosine")

UBCF_pred_rating <- predict(UBCF, getData(traintestset , "known"), type="ratings")
UBCF_pred_topNList <- predict(UBCF, getData(traintestset , "known"), type="topNList")

UBCF_E_rating <- calcPredictionAccuracy(UBCF_pred_rating, getData(traintestset , "unknown"), byUser=FALSE)
UBCF_E_topNList <- calcPredictionAccuracy(UBCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

# IBCF version------------------------------------------------------------------------------------------------------------------------------------
IBCF <- Recommender(getData(traintestset , "train"), method="IBCF", parameter="Cosine")

IBCF_pred_rating <- predict(IBCF, getData(traintestset , "known"), type="ratings")
IBCF_pred_topNList <- predict(IBCF, getData(traintestset , "known"), type="topNList")

IBCF_E_rating <- calcPredictionAccuracy(IBCF_pred_rating, getData(traintestset , "unknown"))
IBCF_E_topNList <- calcPredictionAccuracy(IBCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

# IBCF + UBCF------------------------------------------------------------------------------------------------------------------------------------
HCF <- HybridRecommender(UBCF, IBCF)

HCF_pred_topNList <- predict(HCF, getData(traintestset , "known"), type="topNList")

UBCF_rating_for_HCF <- as.matrix(UBCF_pred_rating@data)
IBCF_rating_for_HCF <- as.matrix(IBCF_pred_rating@data)

UBCF_rating_for_HCF[UBCF_rating_for_HCF==0] <- NA
IBCF_rating_for_HCF[IBCF_rating_for_HCF==0] <- NA

HCF_pred_rating <- (UBCF_rating_for_HCF + IBCF_rating_for_HCF) / 2

HCF_E_rating <- calcPredictionAccuracy(as(HCF_pred_rating,"realRatingMatrix"), getData(traintestset, "unknown"))
HCF_E_topNList <- calcPredictionAccuracy(HCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

# CB ---------------------------------------------------------------------------------------------------------------------------------------

cb_data_after_carpentry <- fread("cb_data_after_carpentry.csv")

dist_mat<-fread("dist_mat_full.csv")
dist_mat_full<-as.matrix(dist_mat)

user_n <- length(unique(cf_data_final$ID))
selected_user <- unique(cf_data_final$ID)

TN= length(IDlist_CF)

test_user <- IDlist_CF

idx_real_set<-vector(TN,mode = "list")
idx_given_set<-vector(TN,mode = "list")

idx_eval_set_hat<-vector(TN,mode = "list")  
idx_eval_set_cf<-vector(TN,mode = "list")  # index for caculating error 

eval_frame<-as.data.frame(matrix(NA,TN,2658))  # our final Eval frame! TN x Restaurant matrix

colnames(eval_frame) <-  cb_data_after_carpentry$Name
rownames(eval_frame) <- IDlist_CF

# Contents-Based Filtering Function

for (i in 1:TN){
  idx_real <- which(cf_data_final$ID == test_user[i])
  
  if ( length(idx_real) != 6){idx_given<-sample(idx_real,6)}  # select 6 given 
  
  idx_eval<-idx_real[which(idx_real %in% idx_given == F)]
  
  
  idx_real_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_real,]$Restaurant)
  idx_given_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_given,]$Restaurant)
  idx_eval_set_hat[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_eval,]$Restaurant)
  idx_eval_set_cf[[i]] <- idx_eval
  
  Res <- cf_data_final$Restaurant[idx_given]  #  6 given's Res
  Rating <- cf_data_final$Rating[idx_given]   # 6 given's Rating
  
  Res_idx_mat <- matrix(NA,length(Res),2658)
  
  for ( j in 1:length(Res)){
    Res_idx <- which( cb_data$Name == Res[j] )
    Res_idx_mat[j,] <- dist_mat_full[Res_idx,]   # Our Similarity Matrix!!!!!!!!!
    #print(j)
  }
  
  sum_set <- apply(Res_idx_mat,2,sum)
  
  sum_set[which(sum_set==0)] <- 0.00001 # simil sum = 0 case. In this case, sum of expected rating is 0
  
  expected_rating <- (Rating %*% Res_idx_mat)    # matrix multiplication
  
  for( k in 1:length(sum_set)){
    expected_rating[k] <- expected_rating[k]/sum_set[k]
  }
  
  eval_frame[i,] <- expected_rating
  print(i)
}

eval_frame <- as.data.frame(eval_frame)
rownames(eval_frame) <- test_user

CB_E_rating <- calcPredictionAccuracy(as(as.matrix(eval_frame),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- eval_frame

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CB_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CB_E_topNList <- colMeans(CB_E_topNList, na.rm=TRUE)

CB_E_topNList

# CB+UBCF------------------------------------------------------------------------------------------------------------------------------------

CBUBCF_pred <- (eval_frame + UBCF_rating_for_HCF) / 2

CBUBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBUBCF_pred),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- (eval_frame + as.matrix(UBCF_pred_rating@data)) / 2

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBUBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBUBCF_E_topNList <- colMeans(CBUBCF_E_topNList, na.rm=TRUE)

CBUBCF_E_topNList

# CB+IBCF------------------------------------------------------------------------------------------------------------------------------------

IDlist_CF <- getData(traintestset , "known")@data@Dimnames[[1]]
CBIBCF_pred <- (eval_frame + IBCF_rating_for_HCF) / 2

CBIBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBIBCF_pred), "realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- (eval_frame + as.matrix(IBCF_pred_rating@data)) / 2

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBIBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBIBCF_E_topNList <- colMeans(CBIBCF_E_topNList, na.rm=TRUE)

CBIBCF_E_topNList

# IBCF + UBCF + CB------------------------------------------------------------------------------------------------------------------------------------

CBUIBCF_pred <- (eval_frame + UBCF_rating_for_HCF + IBCF_rating_for_HCF) / 3

CBUIBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBUIBCF_pred),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- CBUIBCF_pred

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBUIBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBUIBCF_E_topNList <- colMeans(CBUIBCF_E_topNList, na.rm=TRUE)

CBUIBCF_E_topNList

# Summarize Results ----------------------------------------------------------------------------------------------------------------------------

df <- data.frame(MAE = c(UBCF_E_rating[3], IBCF_E_rating[3], CB_E_rating[3], HCF_E_rating[3], CBUBCF_E_rating[3], CBIBCF_E_rating[3], CBUIBCF_E_rating[3]),
                 Precision = c(UBCF_E_topNList[5], IBCF_E_topNList[5], CB_E_topNList[5], HCF_E_topNList[5],
                               CBUBCF_E_topNList[5], CBIBCF_E_topNList[5], CBUIBCF_E_topNList[5]),
                 Recall = c(UBCF_E_topNList[6], IBCF_E_topNList[6], CB_E_topNList[6], HCF_E_topNList[6],
                            CBUBCF_E_topNList[6], CBIBCF_E_topNList[6], CBUIBCF_E_topNList[6]))

rownames(df) <- c("UBCF", "IBCF", "CB", "UBCF+IBCF", "UBCF+CB", "IBCF+CB", "UBCF+IBCF+CB")

df

# Final simulation ------------------------------------------------------------------------------------------------------------------------------------

library(readr)
library(tidyverse)
library(Matrix)
library(recommenderlab)
library(data.table)
library(tictoc)


# CF final data
cf_data_for_onefunc <- fread("cf_data_for_onefunc.csv") 
r <- as(cf_data_for_onefunc, 'realRatingMatrix')
dim(r) #1323x2658


# CB final data
dist_mat_full <- fread("dist_mat_full.csv") #2658x2658
dist_mat_full <- dist_mat_full %>% as.matrix()
cb_data <- fread("CB_data_eng.csv") #CB_data_eng
cb_data_after_carpentry <- fread("cb_data_after_carpentry.csv") #2658x5

identical(cb_data_after_carpentry$Name, r@data@Dimnames[[2]]) 

# Data function

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

# Final Simulation Function

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
      Res_idx_mat[j,] <- dist_mat_full[Res_idx,]   # Our Similarity Matrix!!!!!!!!!
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
