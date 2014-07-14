# make test and training set for weka

twc_calls_w <- read.arff("twc_calls_w.arff")
#sapply(twc_calls_w,class)
rand_sample <- sample(nrow(twc_calls_w),length(rownames(twc_calls_w))%/%3)
train <- twc_calls_w[-rand_sample,] 
test <- twc_calls_w[rand_sample,]
write.arff(x=train,file="train_twc_calls_w.arff",relation="train_twc_calls_w")
write.arff(x=test,file="test_twc_calls_w.arff",relation="test_twc_calls_w")
