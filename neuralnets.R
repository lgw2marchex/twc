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
y <- no_nas[,"converted"]
rand_sample <- sample(nrow(x),length(rownames(x))%/%2)
x_train <- x[rand_sample,]
y_train <- y[rand_sample]
x_test <- x[-rand_sample,]
y_test <- y[-rand_sample]
model <- nnet(x=x_train,y=y_train, size=10)
# not getting any predicted conversions...dunno why not
test_results(as.numeric(as.character(predict(model,x_test))),y_test)
