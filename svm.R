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

## svm

x <- no_nas[,colnames(no_nas)%in%predictors_names]
rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
x_train <- x[rand_sample,]
x_test <- x[-rand_sample,]
model <- svm(converted ~.,data=x_train)

results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:1000) {
  x <- no_nas[,colnames(no_nas)%in%predictors_names]
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