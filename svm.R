
## svm

library("e1071", lib.loc = "\\\\marchex/home/sea/lwilliams/private/R/win-library/3.1")

x <- no_nas[,colnames(no_nas)%in%c(predictors,"converted")]
rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
x_train <- x[rand_sample,]
x_test <- x[-rand_sample,]
model <- svm(converted ~.,data=x_train)

results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:1000) {
  x <- no_nas[,colnames(no_nas)%in%c(predictors,"converted")]
  rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
  x_train <- x[rand_sample,]
  x_test <- x[-rand_sample,]
  model <- svm(converted ~.,data=x_train)
  # using a .16 threshold
  results <- test_results(as.numeric(as.character(predict(model,x_test))),x_test$converted,.16)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
}

# plot them. colors from i want hue, just written in...
plot(as.numeric(results_mtx[,1]),col="white",ylim=c(0,1))
lines(as.numeric(results_mtx[,1]),col=rgb(132,222,189,max=255))
lines(as.numeric(results_mtx[,2]),ylim=c(0,1),col=rgb(209,185,212,max=255))
lines(as.numeric(results_mtx[,3]),ylim=c(0,1), col=rgb(209,209,113,max=255))
# draws lines at means
abline(h=colMeans(results_mtx))
colMeans(results_mtx)