#### logistic regression. change the formula in model. should probably make this a function.

results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:100) {
  rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
  train <- no_nas[rand_sample,]
  test <- no_nas[-rand_sample,]
  model <- glm(as.factor(converted)~conversation_switch_count,
                 family=binomial,data=train)
  predictions <- predict(model,newdata=test, type="response")
  results <- test_results(predictions, test$converted,cutoff=.28)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
}
colMeans(results_mtx)

# plot them. colors from i want hue, just written in...
plot(as.numeric(results_mtx[,1]),col="white",ylim=c(0,1))
lines(as.numeric(results_mtx[,1]),col=rgb(132,222,189,max=255))
lines(as.numeric(results_mtx[,2]),ylim=c(0,1),col=rgb(209,185,212,max=255))
lines(as.numeric(results_mtx[,3]),ylim=c(0,1), col=rgb(209,209,113,max=255))
# draws lines at means
abline(h=colMeans(results_mtx))

############################

## make a sorted table of correlation coefficients for all predictors

# note that all of these calls are billable, so both is_billable and revenue are constant throughout,
# so r won't give a correlation coefficient.
x <- no_nas[,colnames(no_nas)%in%predictors]
cor_df <- data.frame(names(x)[order(cor(x$converted,x), decreasing=TRUE)],cor(x$converted,x)[order(cor(x$converted,x), decreasing=TRUE)])
names(cor_df) <- c("predictor","Rsquared")
rownames <- cor_df$predictor
cor_df <- data.frame(cor_df$Rsquared)
rownames(cor_df) <-rownames
names(cor_df) <- "Rsquared"
cor_df
