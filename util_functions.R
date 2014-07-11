test_results <- function(predictions, actual, cutoff=.5) {
  predicted <- sum(adjustable_round(predictions, cutoff)==1)
#   print("predicted positives")
#   print(predicted)
  converted <- sum(actual==1)
#   print("actual positives")
#   print(converted)
  predicted_and_converted <- sum((adjustable_round(predictions,cutoff)==1) & (actual==1))
#   print("both actual and predicted positives")
#   print(predicted_and_converted)
  not_predicted_and_not_converted <- sum((adjustable_round(predictions,cutoff)==0) & (actual==0))
  # accuracy: true positives+true negatives/total pop
  accuracy_prediction <- (predicted_and_converted+not_predicted_and_not_converted)/length(predictions)
  #precision:true positives/predited positives
  precision_prediction <- predicted_and_converted/predicted
  #recall:true positives/actual positives
  recall_prediction <- predicted_and_converted/converted
  return(c(accuracy=accuracy_prediction,precision=precision_prediction,recall=recall_prediction))
}
# returns 1 if x >= cutoff, else 0
adjustable_round <- function(x, cutoff=.5) {
  as.numeric(x>=cutoff)
}