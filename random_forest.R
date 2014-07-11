###########################

# trying to use the randomForest package. first clean data, then make model.

library("randomForest", lib.loc="\\\\marchex/home/sea/lwilliams/private/R/win-library/3.1")

# need data without nas

# filter out calls that are outcome_name_toplevel=Misclassified or Other and Misclassified
adt_calls_no_misclassified <- adt_calls[!(adt_calls$outcome_name_toplevel %in% c("Misclassified", "Other and Misclassified")),]

# no all-na columns
#category1, category2, estimated_hold_time_duration, caller_speech_duration_before_ring, caller_speech_duration_after_ring
no_na_cols <- data.frame(adt_calls_no_misclassified[,1:19], # no 20 or 21
                         adt_calls_no_misclassified[,22:45], # no 46 or 47
                         adt_calls_no_misclassified[,48:55], # no 56
                         adt_calls_no_misclassified[,57:58])
# now just get rid of all rows with nas...
no_nas <- na.omit(no_na_cols)

#########################

# random forest
# build a data frame of predictors
predictors <- c("conversation",
                "product_or_service",
                "duration",
                "is_billable",
                "revenue",
                "agent_ring_count",
                "agent_speech_duration",
                "agent_speech_ratio",
                "agent_first_half_speech_ratio",
                "agent_first_speech_time",
                "agent_speech_duration_before_ring",
                "agent_speech_duration_after_ring",
                "agent_silence_duration",
                "agent_silence_ratio",
                "caller_ring_count",
                "caller_ring_count",
                "caller_speech_duration",
                "caller_speech_ratio",
                "caller_first_speech_time",
                "caller_silence_duration",
                "caller_silence_ratio",
                "conversation_switch_count",
                "overlapping_speech_count",
                "overlapping_speech_duration",
                "overlapping_silence_count",
                "overlapping_silence_duration",
                'ivr_duration')

# first try!

x <- no_nas[,colnames(no_nas)%in%predictors]
y <- as.factor(no_nas[,"converted"])
rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
x_train <- x[rand_sample,]
y_train <- y[rand_sample]
x_test <- x[-rand_sample,]
y_test <- y[-rand_sample]
model <- randomForest(x=x_train,
                      y=y_train, type="classification")
test_results(as.numeric(as.character(predict(model,x_test))),y_test)

#####

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
