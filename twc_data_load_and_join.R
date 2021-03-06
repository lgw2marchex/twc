# twc_data_load_and_join
# makes data frame called no_nas which can be used for preliminary investigations
# also makes vector of predictors which is used in modeling to indicate which columns of no_nas to use
# note that predictors includes 

require(sqldf)

# remember to set workingDir to where your data is!
WORKINGDIR <- "C:/Users/lwilliams/twc"
setwd(WORKINGDIR)

twc_call_scoring <- read.table("twc_call_scoring.csv",sep=",",header=TRUE)  
twc_call_dna  <- read.table("twc_call_dna.csv",sep=",",header=TRUE)

# sapply(twc_call_scoring,class)
# sapply(twc_call_dna,class)

twc_calls<-sqldf("select
                 job_name,completion_date,listener_id,outcome_name,outcome_name_toplevel,
                 start_time,is_billable,revenue,distributor,client_name,
                 tracking_phone,state,parent,child,affil_parent,mca_industry,
                 dna_classification,assignment_id,li_url,
                 a2.* from twc_call_scoring a1
                 inner join twc_call_dna a2
                 on a1.call_id = a2.call_id")

# see what's na
# sapply(twc_calls,function(x) sum(is.na(x)))


# make a dummy variable for converted
twc_calls$converted <- as.numeric(twc_calls$outcome_name_toplevel=="Sale")

# i'm deleting caller_speech_duration_after ring and caller_speech_duration_before_ring because
# almost all of them are na but the ones that aren't are weird. and i'm deleting estimated_hold_time_duration
# because we only have it for a few calls.
no_nas <- subset(twc_calls, select = -c(caller_speech_duration_after_ring,
                                                  caller_speech_duration_before_ring,
                                                  estimated_hold_time_duration))

# i'm guessing that nas in caller_first_speech_time means that the caller never spoke... 
# i'm going to delete those rows for now, along with the two rows that have nas for almost everything.

no_nas <- na.omit(no_nas)

# make a tab-separated file that can be put into the extractWeka python script.

write.table(data.frame(rep("PFC",length(unique(twc_calls$call_id))),
  unique(twc_calls$call_id),
  rep("eeee",length(unique(twc_calls$call_id)))),
  file="blah.txt",
  row.names=FALSE, 
  col.names=FALSE,
  sep="\t",
  quote=FALSE)

# these are the predictors from adt. but i'm taking is_billable and revenue out because
# they are constant in this data set...
predictors_names <- c("conversation",
                      "product_or_service",
                      "duration",
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
                      "ivr_duration")


# and these are the predictors that are in predictors_names and also in the twc data set
predictors <- predictors_names[predictors_names %in% names(no_nas)]

# missing caller_first_half_speech_ratio and analyzed_call_duration, so i'll add those...
predictors <- c(predictors,"caller_first_half_speech_ratio","analyzed_call_duration")
