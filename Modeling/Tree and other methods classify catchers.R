


#set percent of dat used for training
smp_size <- floor(0.66 * nrow(catcher.data.classifier))

## set the seed to make your partition reproducible
set.seed(123)

#get the random indices for training rows
train_ind <- sample(seq_len(nrow(catcher.data.classifier)), size = smp_size)

#set training and test sets
train <- catcher.data.classifier[train_ind,]

test = catcher.data.classifier[-train_ind,]





#list the models you want to train in order
#many available models found here: https://topepo.github.io/caret/train-models-by-tag.html


algorithmList <- c('adaboost',
                   'naive_bayes')


#set the method of resampling
fitControl <- trainControl(
  
  #method
  method = 'cv',                   # k-fold cross validation
  
  #number of folds
  number = 5,  
  
  #you have to do this to evaluate your training results
  savePredictions = 'final'
) 


# fitControl.rf <- trainControl(
#   
#   #method
#   method = 'oob',                   # k-fold cross validation
#   
#   #number of folds
#   number = 1000,  
#   
#   #you have to do this to evaluate your training results
# ) 


success.models.rf <- train(x =train[,2:ncol(train)-1],     #specify training set
                               y = train$success,
                               trControl=fitControl,     #specify resampling method
                               methodList='rf',
                               metric = 'Accuracy',
                               maximize = T,
                               importance = T,
                               allowParallel = TRUE

                               # specify algorithm list - to do one method, just list it in quotes
                               
)



# success.models.xgdart <- 
#   train(form = success ~ .,    #set your formula the '.' means all variables
#         data=train,     #specify training set
#         trControl=fitControl,     #specify resampling method
#         methodList='naive_bayes',
#         metric = 'accuracy',
#         maximize = FALSE
#         
# 
#   )


success.models <- caretList(x =train[,2:ncol(train)-1],     #specify training set
                            y = train$success,
                            trControl=fitControl,     #specify resampling method
                            methodList=algorithmList,
                            metric = 'Accuracy',
                            maximize = T,
                            allowParallel = TRUE
                            

                            # specify algorithm list - to do one method, just list it in quotes
                            
)

#get your resampled performanced measures (performance within folds)
results <- resamples(success.models)

#print results
summary(results)



# creat another resampling method
# stackControl <- trainControl(
#   method = 'cv',                   # k-fold cross validation
#   number = 5,  
#   
#   savePredictions = 'final'      # saves predictions for optimal tuning parameter
# ) 
# 
# 
# 
# # this is a linear regression on the predictions from the other four, very effective
# stack.glm.success <- caretStack(success.models, 
#                                 method = "glm", 
#                                 trControl=stackControl) 



#make your predictions on test set for reach model

adaboost.pred = predict(success.models$adaboost, test)
naive_bayes.pred = predict(success.models$naive_bayes, test)

rf.pred = predict(success.models.rf, test)

# xgdart.pred = predict(success.models.xgdart, test)



confusion <- function(yhat, y, quietly = FALSE){
  if(!quietly)
    if(!is.factor(y) & is.factor(yhat))
      y <- as.factor(y)
    if(!all.equal(levels(yhat), levels(y)))
      stop("Factor levels of yhat and y do not match.")
    confusion_mat <- table(yhat, y, deparse.level = 2)
    stats <- data.frame(sensitivity = confusion_mat[1, 1]/sum(confusion_mat[, 1]),
                        specificity = confusion_mat[2, 2]/sum(confusion_mat[, 2]))
    return(stats)
}
# Many actual survivors predicted to die


#confutions
conf.naive_bayes <- confusion(yhat = naive_bayes.pred, y = test$success, quietly = FALSE)
conf.adaboost = confusion(yhat = adaboost.pred, y = test$success, quietly = FALSE)
conf.rf = confusion(yhat = rf.pred, y = test$success, quietly = FALSE)


#accuracies

accuracy.naive_bayes = mean(naive_bayes.pred == test$success)
accuracy.naive_bayes
accuracy.adaboost = mean(adaboost.pred == test$success)
accuracy.adaboost

accuracy.rf = mean(rf.pred == test$success)
accuracy.rf

# #create a matrix to store the out of sample results
# success.accuracy.matrix = matrix(nrow = 5, ncol = 2)
# success.accuracy.matrix[1,1] = 'naive_bayes'
# success.accuracy.matrix[2,1] = 'bartMachine'
# success.accuracy.matrix[3,1] = 'adaboost'
# success.accuracy.matrix[4,1] = 'rf'
# success.accuracy.matrix[5,1] = 'Stacked'
# 
# success.accuracy.matrix[1,2] = accuracy.naive_bayes
# success.accuracy.matrix[2,2] = accuracy.bartMachine
# success.accuracy.matrix[3,2] = accuracy.adaboost
# success.accuracy.matrix[4,2] = accuracy.rf
# success.accuracy.matrix[5,2] = accuracy.stacked
# 
# success.accuracy.matrix = as.data.frame(success.accuracy.matrix)
# colnames(success.accuracy.matrix)[1] = 'Model Type'
# colnames(success.accuracy.matrix)[2] = 'Out of Sample accuracy'
# 
# #print results
# print(success.accuracy.matrix)
# 
# 
# 
varImp(success.models.rf)

save.image("~/ML Group Project NFL/WR.TE class Models.RData")

