
library(readr)
library(tidyverse)
library(caret)

library(caretEnsemble)


setwd('~/ML Group Project NFL')

combine.college <- read_csv("NFL-CFB-Combine.csv")


combine.included = filter(combine.college, !is.na(combine_Year)) %>% filter(!is.na(cfb_School))

summary = combine.included %>% group_by(combine_Pos) %>% summarise(freq = n())


#remove qb's and fullbacks since we had 24 and 1 respectively

combine.included = combine.included %>% dplyr::filter(combine_Pos != 'QB' & combine_Pos != 'FB')

combine.included$`nfl_Rushing Yds` = ifelse(is.na(combine.included$`nfl_Rushing Yds`),
                              0, combine.included$`nfl_Rushing Yds`)

combine.included$`nfl_Yds` = ifelse(is.na(combine.included$`nfl_Yds`),
                                            0, combine.included$`nfl_Yds`)

combine.included$scrimmageYds = combine.included$`nfl_Rushing Yds` + combine.included$nfl_Yds


model.data = select(combine.included, -c(contains("nfl"), combine_College, contains("combine_Drafted")))


#testing with WR (dominant position)

wr.data = model.data %>% dplyr::filter(combine_Pos == 'WR')

wr.data[is.na(wr.data)] = 0

wr.data = wr.data[,4:ncol(wr.data)] %>% dplyr::select(-combine_School,
                                                      -cfb_Conf,
                                                               -combine_Year,
                                                               -combine_Height,
                                                               -combine_AV)

wr.data = wr.data %>% dplyr::select(scrimmageYds,
                                          everything())

chars <- unlist(lapply(wr.data, is.character))  

wr.data[,chars] = apply(wr.data[,chars], 2, as.factor)




wr.data = wr.data[,-c(which(grepl('_Att', colnames(wr.data))))]


wr.data = wr.data[,-c(which(grepl('_RS_', colnames(wr.data))))]

wr.data = wr.data[,-c(which(grepl('_Scrimmage', colnames(wr.data))))]



#wr.data = wr.data[,-c(which(grepl('_Receiving', colnames(wr.data))))]

wr.data = wr.data[,-c(which(grepl('_Rushing', colnames(wr.data))))]




wr.data = wr.data[,-c(which(grepl('_Freshman', colnames(wr.data))))]
# 
# wr.data = wr.data[,-c(which(grepl('_Sophomore', colnames(wr.data))))]


#receptions collinear with Yds
wr.data = wr.data %>% dplyr::select(-cfb_Senior_Rec,
                                    -cfb_Junior_Rec,
                                    -cfb_Sophomore_Rec)

#wr.data = wr.data[,-c(which(grepl('_Junior', colnames(wr.data))))]


nums <- unlist(lapply(wr.data, is.numeric))  

numeric.data = wr.data[,nums]

library(mctest)
imcdiag(numeric.data, y = numeric.data$scrimmageYds,
        method = 'VIF')

wr.data = wr.data %>% dplyr::select(-combine_Pos)


bad = wr.data[,nearZeroVar(wr.data)]

# wr.data = wr.data %>% dplyr::select(-cfb_Senior_ScrimmagePlays,
#                                           -cfb_Senior_Rec,
#                                           -cfb_Senior_Att)





#set percent of dat used for training
smp_size <- floor(0.66 * nrow(wr.data))

## set the seed to make your partition reproducible
set.seed(123)

#get the random indices for training rows
train_ind <- sample(seq_len(nrow(wr.data)), size = smp_size)

#set training and test sets
train <- wr.data[train_ind,]

test = wr.data[-train_ind,]



#list the models you want to train in order
#many available models found here: https://topepo.github.io/caret/train-models-by-tag.html


algorithmList <- c('lasso', 
                   'lmStepAIC',
                   'pls',
                   'earth')


#set the method of resampling
fitControl <- trainControl(
  
  #method
  method = 'boot',                   # k-fold cross validation
  
  #number of folds
  number = 100,  
  
  #you have to do this to evaluate your training results
  savePredictions = 'final'
) 




scrimmageYds.models <- caretList(form = scrimmageYds ~ .,    #set your formula the '.' means all variables
                           data=train,     #specify training set
                           trControl=fitControl,     #specify resampling method
             methodList=algorithmList  
             # specify algorithm list - to do one method, just list it in quotes
                        
                        )

#get your resampled peearthormanced measures (peearthormance within folds)
results <- resamples(scrimmageYds.models)

#print results
summary(results)



# creat another resampling method
stackControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,  
  
  savePredictions = 'final'      # saves predictions for optimal tuning parameter
) 



# this is a linear regression on the predictions from the other four, very effective
stack.glm.scrimmageYds <- caretStack(scrimmageYds.models, 
                               method = "glm", 
                               trControl=stackControl) 



#make your predictions on test set for reach model

stack.pred = predict(stack.glm.scrimmageYds, test)
lasso.pred = predict(scrimmageYds.models$lasso, test)
pls.pred = predict(scrimmageYds.models$pls, test)
earth.pred = predict(scrimmageYds.models$earth, test)
lmStepAIC.pred = predict(scrimmageYds.models$lmStepAIC, test)


#compute residuals
stack.err = test$scrimmageYds - stack.pred
lasso.err = test$scrimmageYds - lasso.pred
pls.err = test$scrimmageYds - pls.pred
earth.err = test$scrimmageYds - earth.pred
lmStepAIC.err = test$scrimmageYds - lmStepAIC.pred


#compute rmses
rmse.stacked <- sqrt(mean(stack.err^2))
rmse.pls <- sqrt(mean(pls.err^2))
rmse.lmStepAIC <- sqrt(mean(lmStepAIC.err^2))
rmse.earth = sqrt(mean(earth.err^2))
rmse.lasso = sqrt(mean(lasso.err^2))


#create a matrix to store the out of sample results
scrimmageYds.rmse.matrix = matrix(nrow = 5, ncol = 2)
scrimmageYds.rmse.matrix[1,1] = 'lmStepAIC'
scrimmageYds.rmse.matrix[2,1] = 'pls'
scrimmageYds.rmse.matrix[3,1] = 'lasso'
scrimmageYds.rmse.matrix[4,1] = 'EARTH'
scrimmageYds.rmse.matrix[5,1] = 'Stacked'

scrimmageYds.rmse.matrix[1,2] = rmse.lmStepAIC
scrimmageYds.rmse.matrix[2,2] = rmse.pls
scrimmageYds.rmse.matrix[3,2] = rmse.lasso
scrimmageYds.rmse.matrix[4,2] = rmse.earth
scrimmageYds.rmse.matrix[5,2] = rmse.stacked

scrimmageYds.rmse.matrix = as.data.frame(scrimmageYds.rmse.matrix)
colnames(scrimmageYds.rmse.matrix)[1] = 'Model Type'
colnames(scrimmageYds.rmse.matrix)[2] = 'Out of Sample RMSE'

#print results
print(scrimmageYds.rmse.matrix)

varImp(scrimmageYds.models$pls)

#save.image("~/ML Group Project NFL/All Models.RData")

