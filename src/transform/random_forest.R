library(randomForest)
library(reprtree)

set.seed(19848)

train <- sample(1:nrow(dat), 350)
dat.rf <- randomForest(calls ~ .,
                       data = dat,
                       # na.action = na.roughfix,
                       na.action = na.omit,
                       # subset = train,
                       ntree = 500,
                       mtry = 12,
                       importance = TRUE,
                       do.trace = 200)
dat.rf
plot(dat.rf)
varImpPlot(dat.rf)
library(reprtree)
reprtree:::plot.getTree(dat.rf, depth=5, digits=4)
plot(partykit::ctree(calls ~ ., dat))

library(evtree)
plot(evtree(calls ~ ., dat))

oob.err <- double(15)
test.err <- double(15)
#mtry is no of Variables randomly chosen at each split
for(mtry in 1:15) {
  rf <- randomForest(
    calls ~ . ,
    data = dat,
    subset = train,
    na.action = na.roughfix,
    mtry = mtry,
    ntree = 400
  )
  oob.err[mtry] <- rf$mse[400] #Error of all Trees fitted
  
  pred <- predict(rf, dat[-train, ]) #Predictions on Test Set for each Tree
  test.err[mtry] <- with(dat[-train, ], mean((calls - pred) ^ 2)) #Mean Squared Test Error
  cat(mtry, " ") #printing the output to the console
}

matplot(
  1:mtry ,
  cbind(oob.err, test.err),
  pch = 19 ,
  col = c("red", "blue"),
  type = "b",
  ylab = "Mean Squared Error",
  xlab = "Number of Predictors Considered at each Split"
)
legend(
  "topright",
  legend = c("Out of Bag Error", "Test Error"),
  pch = 19,
  col = c("red", "blue")
)
