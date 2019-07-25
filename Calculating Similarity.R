cb_data <- fread("CB_data_eng.csv")
glimpse(cb_data)
cb <- cb_data[,-c(1,2)]

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