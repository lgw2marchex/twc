#############################

# just check the accuracy, precision, and recall of using billable as a predictor for conversion

billable <- sum(adt_calls$is_billable==1)
converted <- sum(adt_calls$converted==1)
billable_and_converted <- sum((adt_calls$is_billable ==1) & (adt_calls$converted==1))
not_billable_and_not_converted <- sum((adt_calls$is_billable ==0) & (adt_calls$converted==0))
accuracy_billable <- (billable_and_converted+not_billable_and_not_converted)/length(rownames(adt_calls))
precision_billable <- billable_and_converted/billable
recall_billable <- billable_and_converted/converted


###############################

#### first pass logistic regression using ivr duration and duration, because they aren't na

# make training and testing sets
rand_sample <- sample(nrow(adt_calls),length(rownames(adt_calls))%/%2)
train <- adt_calls[rand_sample,]
test <- adt_calls[-rand_sample,]

logit.1 <- glm(as.factor(converted)~ivr_duration*duration,family=binomial,data=train)
predictions.1 <- predict(logit.1,newdata=test, type="response")
sum(round(predictions.1)==1 & test$converted==1)

predicted <- sum(round(predictions.1)==1)
converted <- sum(test$converted==1)
predicted_and_converted <- sum((round(predictions.1)==1) & (test$converted==1))
not_predicted_and_not_converted <- sum((round(predictions.1)==0) & (test$converted==0))
accuracy_prediction <- (predicted_and_converted+not_predicted_and_not_converted)/length(rownames(test))
precision_prediction <- predicted_and_converted/predicted
recall_prediction <- predicted_and_converted/converted


################################

###### try to look at columns with nas

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

# now do some regression!
rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
train <- no_nas[rand_sample,]
test <- no_nas[-rand_sample,]

logit.3 <- glm(as.factor(converted)~agent_ring_count*agent_speech_duration*
                 agent_speech_ratio*agent_first_speech_time*ivr_duration*duration, family=binomial,data=train)
predictions.3 <- predict(logit.3, newdata=test, type="response")
test_results(predictions.3,test$converted)

predictors <- c("conversation",
                "product_or_service",
                "duration",
                "is_billable",
                "revenue",
                "converted",
                "agent_ring_count",
                "agent_speech_duration",
                "agent_speech_ratio",
                "agent_first_half_speech_ratio",
                "agent_spoke_first",
                "agent_first_speech_time",
                "agent_speech_duration_before_ring",
                "agent_speech_duration_after_ring",
                "agent_silence_duration",
                "agent_silence_ratio",
                "caller_ring_count",
                "caller_ring_count",
                "caller_speech_duration",
                "caller_speech_ratio",
                "caller_spoke_first",
                "caller_first_speech_time",
                "caller_silence_duration",
                "caller_silence_ratio",
                "conversation_switch_count",
                "overlapping_speech_count",
                "overlapping_speech_duration",
                "overlapping_silence_count",
                "overlapping_silence_duration",
                'ivr_duration')

results_each_predictor <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:length(predictors)) {
  results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
  predictor <- predictors[i]
  print(predictor)
  for (j in 1:10) {
    rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
    train <- no_nas[rand_sample,]
    test <- no_nas[-rand_sample,]
    print(sum(test$converted==0))
    print(sum((train$converted==0)))
    model <- glm(as.factor(train$converted)~train[,predictor],family=binomial)
    print(names(newdata))
    predictions <- predict(model,newdata=data.frame(test[,predictor]), type="response")
    results <- test_results(predictions,test$converted,cutoff=.3)
    print(results)
    df <- data.frame(accuracy=results["accuracy"],
                     precision=results["precision"],
                     recall=results["recall"])
    results_mtx <- rbind(results_mtx,df)
  }
  df <- colMeans(results_mtx)
  results_each_predictor <- rbind(results_each_predictor,df)
}

predictor <- predictors[i]
print(predictor)
results_mtx.4 = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (j in 1:10) {
  rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
  train <- no_nas[rand_sample,]
  test <- no_nas[-rand_sample,]
  model <- glm(as.factor(train$converted)~train[,predictor],family=binomial)
  predictions <- predict(logit.4,newdata=test, type="response")
  results <- test_results(predictions,test$converted,cutoff=.3)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
  
  
  logit.4 <- glm(as.factor(converted)~conversation_switch_count,
                 family=binomial,data=train)
  predictions.4 <- predict(logit.4,newdata=test, type="response")
  results.4 <- test_results(predictions.4, test$converted,cutoff=.3)
  df.4 <- data.frame(accuracy=results.4["accuracy"],
                   precision=results.4["precision"],
                   recall=results.4["recall"])
  results_mtx.4 <- rbind(results_mtx.4,df.4)
  print(sum(predictions==predictions.4)/length(predictions))
  print(logit.4$coefficients==model$coefficients)
}

################################

#random forest says that conversation_switch_count is most important, followed by overlapping_silence_count

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

rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
train <- no_nas[rand_sample,]
test <- no_nas[-rand_sample,]

# logistic regression with conversation_switch_count and overlapping_silence_count)
logit.4 <- glm(as.factor(converted)~conversation_switch_count*overlapping_silence_count*agent_speech_duration*duration,
               family=binomial,data=train)
predictions.4 <- predict(logit.4,newdata=test, type="response")
test_results(predictions.4, test$converted,cutoff=.4)

###############################
results_each_predictor <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
# see how the logistic model varies with different randomly chosen training/testing samples:
# do a bunch and record them!





results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:100) {
  rand_sample <- sample(nrow(no_nas),length(rownames(no_nas))%/%2)
  train <- no_nas[rand_sample,]
  test <- no_nas[-rand_sample,]
  logit.4 <- glm(as.factor(converted)~conversation_switch_count,
                 family=binomial,data=train)
  predictions.4 <- predict(logit.4,newdata=test, type="response")
  results <- test_results(predictions.4, test$converted,cutoff=.28)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
}
colMeans(results_mtx)
results_each_predictor <- rbind(results_each_predictor, colMeans(results_mtx))
results_each_predictor


predictors_names <- c("conversation",
                      "product_or_service",
                      "duration",
                      "is_billable",
                      "revenue",
                      "converted",
                      "agent_ring_count",
                      "agent_speech_duration",
                      "agent_speech_ratio",
                      "agent_first_half_speech_ratio",
                      "agent_spoke_first",
                      "agent_first_speech_time",
                      "agent_speech_duration_before_ring",
                      "agent_speech_duration_after_ring",
                      "agent_silence_duration",
                      "agent_silence_ratio",
                      "caller_ring_count",
                      "caller_speech_duration",
                      "caller_speech_ratio",
                      "caller_spoke_first",
                      "caller_first_speech_time",
                      "caller_silence_duration",
                      "caller_silence_ratio",
                      "conversation_switch_count",
                      "overlapping_speech_count",
                      "overlapping_speech_duration",
                      "overlapping_silence_count",
                      "overlapping_silence_duration",
                      'ivr_duration')




# plot them. colors from i want hue, just written in...
plot(as.numeric(results_mtx[,1]),col="white",ylim=c(0,1))
lines(as.numeric(results_mtx[,1]),col=rgb(132,222,189,max=255))
lines(as.numeric(results_mtx[,2]),ylim=c(0,1),col=rgb(209,185,212,max=255))
lines(as.numeric(results_mtx[,3]),ylim=c(0,1), col=rgb(209,209,113,max=255))
# draws lines at means
abline(h=colMeans(results_mtx))

############################

## make a sorted table of correlation coefficients for all predictors

x <- no_nas[,colnames(no_nas)%in%predictors_names]
cor_df <- data.frame(names(x)[order(cor(x$converted,x), decreasing=TRUE)],cor(x$converted,x)[order(cor(x$converted,x), decreasing=TRUE)])
names(cor_df) <- c("predictor","Rsquared")
rownames <- cor_df$predictor
cor_df <- data.frame(cor_df$Rsquared)
rownames(cor_df) <-rownames
names(cor_df) <- "Rsquared"
