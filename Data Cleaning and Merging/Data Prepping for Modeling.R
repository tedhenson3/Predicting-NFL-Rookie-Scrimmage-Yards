
library(readr)
library(tidyverse)
library(caret)

library(caretEnsemble)


setwd('~/ML Group Project NFL')

combine.college <- read_csv("NFL-CFB-Combine.csv")

combine.college$combine_Height = gsub('Jul', '7ft', fixed = T, combine.college$combine_Height)

combine.college$combine_Height = gsub('Jun', '6ft', fixed = T, combine.college$combine_Height)

combine.college$combine_Height = gsub('May', '5ft', fixed = T, combine.college$combine_Height)

combine.college$combine_Height = gsub('Apr', '5ft', fixed = T, combine.college$combine_Height)


feet = ifelse(grepl('6ft', combine.college$combine_Height),
              6,
              5)
inches = combine.college$combine_Height
inches = gsub('6ft', '',inches)
inches = gsub('5ft', '',inches)

inches = gsub('-', '',inches, fixed = T)
inches = as.numeric(inches)
combine.college$combine_Height = feet*12 + inches




combine.included = filter(combine.college, !is.na(combine_Year)) %>% filter(!is.na(cfb_School))



combine.included = 
  combine.included %>% dplyr::select(-cfb_Att,
                                     -cfb_ReceivingAvg)


cfb.crap = combine.included[,which(grepl('cfb_', colnames(combine.included)))]

cfb.crap = cfb.crap[,4:ncol(cfb.crap)]


cfb.means = data.frame(matrix(ncol = 12, nrow = nrow(cfb.crap)))

cfb.sums = data.frame(matrix(ncol = 12, nrow = nrow(cfb.crap)))

colnames(cfb.means) = gsub('Senior', 'Mean_', 
                           colnames(cfb.crap)[seq(from = 1, by= 5, to = ncol(cfb.crap))])

colnames(cfb.sums) = gsub('Senior', 'Sum_', 
                          colnames(cfb.crap)[seq(from = 1, by= 5, to = ncol(cfb.crap))])

my.mean = function(x){
  
  return(mean(x, na.rm = T))
}


my.sum = function(x){
  
  return(sum(x, na.rm = T))
}

for(i in 1:12){
  
  cfb.means[,i] = apply(X = cfb.crap[,i:c(i+4)], MARGIN = 1, FUN = my.mean)
  
  cfb.sums[,i] = apply(X = cfb.crap[,i:c(i+4)], MARGIN = 1, FUN = my.sum)
  
  
}


combine.included = cbind(combine.included,
                         cfb.means,
                         cfb.sums)


summary = combine.included %>% group_by(combine_Pos) %>% summarise(freq = n())


#remove qb's and fullbacks since we had 24 and 1 respectively

combine.included = combine.included %>% dplyr::filter(combine_Pos != 'QB' & combine_Pos != 'FB')

combine.included$`nfl_Rushing Yds` = ifelse(is.na(combine.included$`nfl_Rushing Yds`),
                                            0, combine.included$`nfl_Rushing Yds`)

combine.included$`nfl_Yds` = ifelse(is.na(combine.included$`nfl_Yds`),
                                    0, combine.included$`nfl_Yds`)


#PREDICTING ON SCRIMMAGE YARDS PER GAME
combine.included$scrimmageYds.per.game = 
  (combine.included$`nfl_Rushing Yds` + combine.included$nfl_Yds) /  combine.included$nfl_G


model.data = select(combine.included, -c(contains("nfl"), combine_College, contains("combine_Drafted")))


#testing with WR (dominant position)

flex.data = model.data %>% dplyr::filter(combine_Pos  %in% c('WR', 'RB', 'TE'))


#only make college data 0s, as we don't want 0 bench press reps (it needs to stay NA)
flex.data = flex.data %>% mutate_at(.vars = which(grepl('cfb_',
                                                        colnames(flex.data))), 
                                    .funs = funs(ifelse(is.na(.), 0, .)))



flex.data = flex.data%>% dplyr::select(-combine_School,
                                       -combine_Year,
                                       #-combine_Height,
                                       -combine_AV)

na.counts.flex.data = colSums(is.na(flex.data))


#only grab players with complete combine data
flex.data = flex.data[complete.cases(flex.data),]


flex.data = flex.data[,4:ncol(flex.data)] 

player.conf = flex.data$cfb_Conf

#need to remove due to interaction issues
flex.data = flex.data %>% dplyr::select(-cfb_Conf)


flex.data = flex.data %>% dplyr::select(scrimmageYds.per.game,
                                        everything())


nums <- unlist(lapply(flex.data, is.numeric))  

numeric.data = flex.data[,nums]

library(mctest)
imcdiag(numeric.data, y = numeric.data$scrimmageYds.per.game,
        method = 'VIF')




# flex.data$combine_Pos = as.factor(flex.data$combine_Pos)


#creating dummy variables
flex.data$is.catcher = ifelse(flex.data$combine_Pos %in% c('WR',
                                                           'TE'),
                              1, 0)


flex.data$is.wr = ifelse(flex.data$combine_Pos %in% c('WR'),
                         1, 0)
flex.data$is.rb = ifelse(flex.data$combine_Pos == 'RB',
                         1, 0)

flex.data$is.te = ifelse(flex.data$combine_Pos == 'TE',
                         1, 0)

#have dummy variables so can now get rid of factored position variable
flex.data = flex.data %>% dplyr::select(-combine_Pos)


# only have enough obs for power five, otherwise just make it other
player.conf = ifelse(player.conf %in% 
                       c('American',
                         'Big East',
                         'CUSA',
                         'Ind',
                         'MAC',
                         'MWC',
                         'Sun Belt'),
                     'Non Power 5',
                     player.conf)

flex.data$cfb_Conf = as.factor(player.conf)

bad.columns = flex.data[, nearZeroVar(flex.data)]


#got rid of freshman, rs freshman seasons as that was most of the near zero var issue

#very few players had 4 our more seasons of data, and it did not improve predictions anyway

flex.data  = flex.data[,-c(which(grepl('_Freshman', colnames(flex.data))))]

#below shows there are now zero near zero var columns
bad.columns = flex.data[, nearZeroVar(flex.data)]


rb.data = flex.data %>% filter(is.rb == 1) %>% select(-is.wr,
                                                      -is.te,
                                                      -is.catcher,
                                                      -is.rb)

bad.columns = rb.data[, nearZeroVar(rb.data)]


catcher.data = flex.data %>% filter(is.catcher == 1) %>% dplyr::select(
  -is.catcher,
  -is.rb)
bad.columns = catcher.data[, nearZeroVar(catcher.data)]



# 
# 
# flex.data.classifier = flex.data
# 
# for(i in 1:nrow(flex.data.classifier)){
#   if(flex.data.classifier$scrimmageYds[i] < 480){
#     flex.data.classifier$success[i] = 0
#   }else{
#     flex.data.classifier$success[i] = 1
#   }
# }
# 
# 
rb.data.classifier = rb.data

for(i in 1:nrow(rb.data)){
  if(rb.data.classifier$scrimmageYds[i] < 480){
    rb.data.classifier$success[i] = 0
  }else{
    rb.data.classifier$success[i] = 1
  }
}
rb.data.classifier = rb.data.classifier %>% dplyr::select(-scrimmageYds)
# 
catcher.data.classifier = catcher.data

for(i in 1:nrow(catcher.data)){
  if(catcher.data.classifier$scrimmageYds[i] < 480){
    catcher.data.classifier$success[i] = 0
  }else{
    catcher.data.classifier$success[i] = 1
  }
}


catcher.data.classifier = catcher.data.classifier %>% dplyr::select(-scrimmageYds)
