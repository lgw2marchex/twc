# guess all conversions


y <- no_nas$converted
x <- rep(1,length(y))
results <- test_results(x,y)


# guess randomly 

results_mtx = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("accuracy", "precision", "recall"))), stringsAsFactors=F)
for (i in 1:100) {
  y <- no_nas$converted
  x<- sample(0:1, length(y),replace=T)
  results <- test_results(x,y)
  df <- data.frame(accuracy=results["accuracy"],
                   precision=results["precision"],
                   recall=results["recall"])
  results_mtx <- rbind(results_mtx,df)
}

# use billability


y <- no_nas$converted
x<- no_nas$is_billable
results <- test_results(x,y)




#######

# parallel coordinates

par_coord <- merge(importance_table,cor_df, by="predictor")
rownames(par_coord) <- par_coord$predictor
par_coord <- par_coord[,2:3]
names(par_coord) <- c("Importance", "Rsquared")
par_coord$Rsquared <- abs(par_coord$Rsquared)
parcoord(par_coord,var.label=TRUE,col=col)
mtext("Importance vs. Rsquared for all predictors")
