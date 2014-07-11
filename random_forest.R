###########################

# trying to use the randomForest package. first clean data, then make model.

library("randomForest", lib.loc="\\\\marchex/home/sea/lwilliams/private/R/win-library/3.1")

#########################

# make 100 of the models to get an average accuracy, precision, and recall

results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:100) {
  x <- no_nas[,colnames(no_nas)%in%predictors]
  y <- as.factor(no_nas[,"converted"])
  rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
  x_train <- x[rand_sample,]
  y_train <- y[rand_sample]
  x_test <- x[-rand_sample,]
  y_test <- y[-rand_sample]
  model <- randomForest(x=x_train,
                        y=y_train, type="classification")
  results <- test_results(as.numeric(as.character(predict(model,x_test))),y_test)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
}
results_mtx
# plot them. colors from i want hue, just written in...
plot(as.numeric(results_mtx[,1]),col="white",ylim=c(0,1))
lines(as.numeric(results_mtx[,1]),col=rgb(132,222,189,max=255))
lines(as.numeric(results_mtx[,2]),ylim=c(0,1),col=rgb(209,185,212,max=255))
lines(as.numeric(results_mtx[,3]),ylim=c(0,1), col=rgb(209,209,113,max=255))
# draws lines at means
abline(h=colMeans(results_mtx))

# i want to know if the values for importance that random forest model gives jump around as
# we take different random samples. so the following code plots the importance of predictors over 100 
# trials of random forest, picking new training and testing sets each time.

importance_df <- data.frame(model$importance)
for (x in 1:100) {
  x <- no_nas[,colnames(no_nas)%in%predictors]
  y <- as.factor(no_nas[,"converted"])
  rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
  x_train <- x[rand_sample,]
  y_train <- y[rand_sample]
  x_test <- x[-rand_sample,]
  y_test <- y[-rand_sample]
  model <- randomForest(x=x_train,y=y_train, ,xtest=x_test,ytest=y_test, type="classification")
  importance_df <- cbind(importance_df, data.frame(model$importance))
}

# sorted table of importances
importance_table <- data.frame(sort(rowMeans(importance_df),decreasing=T))
importance_table <- data.frame(rownames(importance_table),importance_table)
names(importance_table) <- c("predictor", "importance")

plot(as.numeric(importance_df[1,]),ylim=c(0,40), xlim=c(0,110), pch="-",col="white")
lines(as.numeric(importance_df[1,]),ylim=c(0,40), pch="-",col=col[1])
text(x=105,y=as.numeric(importance_df[1,100]),rownames(importance_df)[1], cex=.5)
for (index in 2:27) {
  lines(as.numeric(importance_df[index,]),ylim=c(0,40),col=col[index])
  text(x=105,y=as.numeric(importance_df[index,100]),rownames(importance_df)[index], cex=.5)
}
mtext("Variation in Importance of Predictors over 100 random forest models")
# draw lines at means
abline(h=rowMeans(importance_df),lwd=.01)

# colors for plotting, generated from i want hue
col<- c(rgb(98,238,175,max=255),
        rgb(233,168,215,max=255),
        rgb(234,189,87,max=255),
        rgb(134,199,227,max=255),
        rgb(173,236,117,max=255),
        rgb(168,199,159,max=255),
        rgb(241,164,158,max=255),
        rgb(226,230,93,max=255),
        rgb(84,235,216,max=255),
        rgb(229,174,119,max=255),
        rgb(175,183,235,max=255),
        rgb(213,224,147,max=255),
        rgb(158,220,212,max=255),
        rgb(160,238,198,max=255),
        rgb(91,222,234,max=255),
        rgb(169,229,159,max=255),
        rgb(129,233,138,max=255),
        rgb(224,192,217,max=255),
        rgb(201,198,94,max=255),
        rgb(188,204,207,max=255),
        rgb(208,190,119,max=255),
        rgb(242,161,94,max=255),
        rgb(145,201,115,max=255),
        rgb(74,209,175,max=255),
        rgb(204,231,122,max=255),
        rgb(126,202,164,max=255),
        rgb(192,211,233,max=255))

## table of importances for whole data set

x <- no_nas[,colnames(no_nas)%in%predictors]
y <- as.factor(no_nas[,"converted"])
model <- randomForest(x=x,y=y, type="classification")
importance_table <- data.frame(predictor=rownames(model$importance),importance=model$importance)
data.frame(importance=model$importance[order(model$importance,decreasing=TRUE)],
           row.names=rownames(model$importance)[order(model$importance,decreasing=TRUE)])
