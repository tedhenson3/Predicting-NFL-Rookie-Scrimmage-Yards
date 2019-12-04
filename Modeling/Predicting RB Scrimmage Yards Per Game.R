


#set percent of dat used for training
smp_size <- floor(0.66 * nrow(rb.data))

## set the seed to make your partition reproducible
set.seed(123)

#get the random indices for training rows
train_ind <- sample(seq_len(nrow(rb.data)), size = smp_size)

#set training and test sets
train <- rb.data[train_ind,]

test = rb.data[-train_ind,]



#list the models you want to train in order
#many available models found here: https://topepo.github.io/caret/train-models-by-tag.html


algorithmList <- c('pcr', 
                   'xgbDART',
                   'pls',
                   'rf')


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


scrimmageYds.per.game.models.rf <- 
  train(form = scrimmageYds.per.game ~ .,    #set your formula the '.' means all variables
                                          data=train,     #specify training set
                                          trControl=fitControl,     #specify resampling method
                                          methodList='rf',
        importance = T,
        metric = 'RMSE',
        maximize = FALSE
        
                                          # specify algorithm list - to do one method, just list it in quotes
                                          
)


# scrimmageYds.per.game.models.xgdart <- 
#   train(form = scrimmageYds.per.game ~ .,    #set your formula the '.' means all variables
#         data=train,     #specify training set
#         trControl=fitControl,     #specify resampling method
#         methodList='xgbDART',
#         metric = 'RMSE',
#         maximize = FALSE
#         
# 
#   )


scrimmageYds.per.game.models <- caretList(form = scrimmageYds.per.game ~ .,    #set your formula the '.' means all variables
                           data=train,     #specify training set
                           trControl=fitControl,     #specify resampling method
             methodList=algorithmList,
             metric = 'RMSE',
             maximize = FALSE
             # specify algorithm list - to do one method, just list it in quotes
                        
                        )

#get your resampled performanced measures (performance within folds)
results <- resamples(scrimmageYds.per.game.models)

#print results
summary(results)



# creat another resampling method
stackControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,  
  
  savePredictions = 'final'      # saves predictions for optimal tuning parameter
) 



# this is a linear regression on the predictions from the other four, very effective
stack.glm.scrimmageYds.per.game <- caretStack(scrimmageYds.per.game.models, 
                               method = "glm", 
                               trControl=stackControl) 



#make your predictions on test set for reach model

stack.pred = predict(stack.glm.scrimmageYds.per.game, test)
pcr.pred = predict(scrimmageYds.per.game.models$pcr, test)
pls.pred = predict(scrimmageYds.per.game.models$pls, test)
rf.pred = predict(scrimmageYds.per.game.models$rf, test)
xgbDART.pred = predict(scrimmageYds.per.game.models$xgbDART, test)

rf.pred = predict(scrimmageYds.per.game.models.rf, test)

# xgdart.pred = predict(scrimmageYds.per.game.models.xgdart, test)



#compute residuals
stack.err = test$scrimmageYds.per.game - stack.pred
pcr.err = test$scrimmageYds.per.game - pcr.pred
pls.err = test$scrimmageYds.per.game - pls.pred
rf.err = test$scrimmageYds.per.game - rf.pred
xgbDART.err = test$scrimmageYds.per.game - xgbDART.pred

#compute rmses
rmse.stacked <- sqrt(mean(stack.err^2))
rmse.pls <- sqrt(mean(pls.err^2))
rmse.xgbDART <- sqrt(mean(xgbDART.err^2))
rmse.rf = sqrt(mean(rf.err^2))
rmse.pcr = sqrt(mean(pcr.err^2))



#create a matrix to store the out of sample results
scrimmageYds.per.game.rmse.matrix = matrix(nrow = 5, ncol = 2)
scrimmageYds.per.game.rmse.matrix[1,1] = 'xgbDART'
scrimmageYds.per.game.rmse.matrix[2,1] = 'pls'
scrimmageYds.per.game.rmse.matrix[3,1] = 'pcr'
scrimmageYds.per.game.rmse.matrix[4,1] = 'rf'
scrimmageYds.per.game.rmse.matrix[5,1] = 'Stacked'

scrimmageYds.per.game.rmse.matrix[1,2] = rmse.xgbDART
scrimmageYds.per.game.rmse.matrix[2,2] = rmse.pls
scrimmageYds.per.game.rmse.matrix[3,2] = rmse.pcr
scrimmageYds.per.game.rmse.matrix[4,2] = rmse.rf
scrimmageYds.per.game.rmse.matrix[5,2] = rmse.stacked

scrimmageYds.per.game.rmse.matrix = as.data.frame(scrimmageYds.per.game.rmse.matrix)
colnames(scrimmageYds.per.game.rmse.matrix)[1] = 'Model Type'
colnames(scrimmageYds.per.game.rmse.matrix)[2] = 'Out of Sample RMSE'

#print results
print(scrimmageYds.per.game.rmse.matrix)



varImp(scrimmageYds.per.game.models.rf)

save.image("~/ML Group Project NFL/RB Models.RData")

