
library(readr)
library(tidyverse)

setwd("~/ML Group Project NFL")
cfb_receiving_2009 = read_csv('2009_cfb_receiving.csv')
cfb_receiving_2010 = read_csv('2010_cfb_receiving.csv')

cfb_receiving_2011 = read_csv('2011_cfb_receiving.csv')
cfb_receiving_2012 = read_csv('2012_cfb_receiving.csv')
cfb_receiving_2013 <- read_csv("2013_cfb_receiving.csv")
cfb_receiving_2014 <- read_csv("2014_cfb_receiving.csv")
cfb_receiving_2015 <- read_csv("2015_cfb_receiving.csv")
cfb_receiving_2016 <- read_csv("2016_cfb_receiving.csv")
cfb_receiving_2017 <- read_csv("2017_cfb_receiving.csv")

colnames(cfb_receiving_2016) = colnames(cfb_receiving_2013)
colnames(cfb_receiving_2017) = colnames(cfb_receiving_2013)

colnames(cfb_receiving_2013) = gsub('_2013', '', colnames(cfb_receiving_2013))
colnames(cfb_receiving_2014) = gsub('_2014', '', colnames(cfb_receiving_2014))
colnames(cfb_receiving_2015) = gsub('_2015', '', colnames(cfb_receiving_2015))
colnames(cfb_receiving_2016) = gsub('_2013', '', colnames(cfb_receiving_2016))
colnames(cfb_receiving_2017) = gsub('_2013', '', colnames(cfb_receiving_2017))
colnames(cfb_receiving_2012) = colnames(cfb_receiving_2017)
colnames(cfb_receiving_2010) = colnames(cfb_receiving_2010)
colnames(cfb_receiving_2009) = colnames(cfb_receiving_2009)





cfb_receiving_2009$Season = 2009
cfb_receiving_2010$Season = 2010
cfb_receiving_2011$Season = 2011

cfb_receiving_2012$Season = 2012
cfb_receiving_2013$Season = 2013
cfb_receiving_2014$Season = 2014
cfb_receiving_2015$Season = 2015
cfb_receiving_2016$Season = 2016
cfb_receiving_2017$Season = 2017

cfb_receiving = rbind(cfb_receiving_2009,
  cfb_receiving_2010,
  cfb_receiving_2011,
  cfb_receiving_2012,
                      cfb_receiving_2013,
                      cfb_receiving_2014,
                      cfb_receiving_2015,
                      cfb_receiving_2015,
                      cfb_receiving_2016,
                      cfb_receiving_2017)

cfb_receiving = cfb_receiving %>% dplyr::arrange(Player, desc(Season)) %>%
  dplyr::select(Player, Season,
                School,
                Conf,
                everything())

cfb_receiving = cfb_receiving %>% dplyr::select(-Rk)



cfb_receiving = cfb_receiving[!duplicated(cfb_receiving),]

cfb_receiving = cfb_receiving %>% dplyr::filter(Player != 'Player' & Player != 'Rk')




cfb_rushing_2009 <- read_csv("2009_cfb_rushing.csv")
cfb_rushing_2010 <- read_csv("2010_cfb_rushing.csv")
cfb_rushing_2011 <- read_csv("2011_cfb_rushing.csv")

cfb_rushing_2012 <- read_csv("2012_cfb_rushing.csv")
cfb_rushing_2013 <- read_csv("2013_cfb_rushing.csv")
cfb_rushing_2014 <- read_csv("2014_cfb_rushing.csv")
cfb_rushing_2015 <- read_csv("2015_cfb_rushing.csv")
cfb_rushing_2016 <- read_csv("2016_cfb_rushing.csv")
cfb_rushing_2017 <- read_csv("2017_cfb_rushing.csv")


colnames(cfb_rushing_2016) = colnames(cfb_rushing_2013)
colnames(cfb_rushing_2017) = colnames(cfb_rushing_2013)

colnames(cfb_rushing_2013) = gsub('_2013', '', colnames(cfb_rushing_2013))
colnames(cfb_rushing_2014) = gsub('_2014', '', colnames(cfb_rushing_2014))
colnames(cfb_rushing_2015) = gsub('_2015', '', colnames(cfb_rushing_2015))
colnames(cfb_rushing_2016) = gsub('_2013', '', colnames(cfb_rushing_2016))
colnames(cfb_rushing_2017) = gsub('_2013', '', colnames(cfb_rushing_2017))
colnames(cfb_rushing_2012) = colnames(cfb_rushing_2017)
colnames(cfb_rushing_2011) = colnames(cfb_rushing_2017)
colnames(cfb_rushing_2010) = colnames(cfb_rushing_2017)
colnames(cfb_rushing_2009) = colnames(cfb_rushing_2017)



cfb_rushing_2009$Season = 2009
cfb_rushing_2010$Season = 2010
cfb_rushing_2011$Season = 2011

cfb_rushing_2012$Season = 2012
cfb_rushing_2013$Season = 2013
cfb_rushing_2014$Season = 2014
cfb_rushing_2015$Season = 2015
cfb_rushing_2016$Season = 2016
cfb_rushing_2017$Season = 2017

cfb_rushing = rbind(cfb_rushing_2009,
  cfb_rushing_2010,
  cfb_rushing_2011,
  cfb_rushing_2012,
                    cfb_rushing_2013,
                    cfb_rushing_2014,
                    cfb_rushing_2015,
                    cfb_rushing_2015,
                    cfb_rushing_2016,
                    cfb_rushing_2017)

cfb_rushing = cfb_rushing %>% dplyr::arrange(Player, desc(Season)) %>%
  dplyr::select(Player, Season,
                School,
                Conf,
                everything())

cfb_rushing = cfb_rushing %>% dplyr::select(-Rk)


cfb_rushing = cfb_rushing[!duplicated(cfb_rushing),]


cfb_rushing = cfb_rushing %>% dplyr::filter(Player != 'Player' & Player != 'Rk')



cfb = full_join(cfb_rushing, cfb_receiving)


cfb$Player = word(cfb$Player, start = 1, end = 1, sep = "-")
cfb$Player = gsub('*', '', cfb$Player, fixed = T)
cfb$Player = gsub('+', '', cfb$Player, fixed = T)



cfb$Player = word(cfb$Player, start = 1, end = 1, sep = "-")
cfb$Player = strsplit(cfb$Player, split = '\\',
                                fixed = T)

cfb$Player= lapply(cfb$Player,
                             function(l) l[[1]])

cfb$Player = gsub('*', '', cfb$Player, fixed = T)
cfb$Player = gsub('+', '', cfb$Player, fixed = T)


# nfl_combine_2013 <- read_csv("2013_nfl_combine.csv") unnecessary and different formatting
nfl_combine_2014 <- read_csv("2014_nfl_combine.csv")
nfl_combine_2015 <- read_csv("2015_nfl_combine.csv")
nfl_combine_2016 <- read_csv("2016_nfl_combine.csv")
nfl_combine_2017 <- read_csv("2017_nfl_combine.csv")
nfl_combine_2018 <- read_csv("2018_nfl_combine.csv")

nfl_combine = rbind(nfl_combine_2014,
                    nfl_combine_2015,
                    nfl_combine_2016,
                    nfl_combine_2017,
                    nfl_combine_2018)


nfl_receiving_2014 <- read_csv("2014_nfl_receiving.csv")
nfl_receiving_2015 <- read_csv("2015_nfl_receiving.csv")
nfl_receiving_2016 <- read_csv("2016_nfl_receiving.csv")
nfl_receiving_2017 <- read_csv("2017_nfl_receiving.csv")
nfl_receiving_2018 <- read_csv("2018_nfl_receiving.csv")


colnames(nfl_receiving_2014) = colnames(nfl_receiving_2015)


nfl_receiving_2014$Season = 2014
nfl_receiving_2015$Season = 2015
nfl_receiving_2016$Season = 2016
nfl_receiving_2017$Season = 2017
nfl_receiving_2018$Season = 2018

nfl_receiving = rbind(nfl_receiving_2014,
                      nfl_receiving_2015,
                      nfl_receiving_2016,
                      nfl_receiving_2017,
                      nfl_receiving_2018)



nfl_receiving = nfl_receiving %>% dplyr::select(-Rk)


nfl_receiving = nfl_receiving[!duplicated(nfl_receiving),]


nfl_receiving = nfl_receiving %>% dplyr::filter(Player != 'Player')



nfl_rushing_2014 <- read_csv("2014_nfl_rushing.csv")
nfl_rushing_2015 <- read_csv("2015_nfl_rushing.csv")
nfl_rushing_2016 <- read_csv("2016_nfl_rushing.csv")
nfl_rushing_2017 <- read_csv("2017_nfl_rushing.csv")
nfl_rushing_2018 <- read_csv("2018_nfl_rushing.csv")
colnames(nfl_rushing_2014) = colnames(nfl_rushing_2015)


nfl_rushing_2014$Season = 2014
nfl_rushing_2015$Season = 2015
nfl_rushing_2016$Season = 2016
nfl_rushing_2017$Season = 2017
nfl_rushing_2018$Season = 2018

nfl_rushing = rbind(nfl_rushing_2014,
                    nfl_rushing_2015,
                    nfl_rushing_2016,
                    nfl_rushing_2017,
                    nfl_rushing_2018)

colnames(nfl_rushing)[which(colnames(nfl_rushing) == "Att"):ncol(nfl_rushing)-1] = paste('Rushing', colnames(nfl_rushing)[which(colnames(nfl_rushing) == "Att"):ncol(nfl_rushing)-1])




nfl_rushing = nfl_rushing %>% dplyr::select(-Rk)


nfl_rushing = nfl_rushing[!duplicated(nfl_rushing),]


nfl_rushing = nfl_rushing %>% dplyr::filter(Player != 'Player')

nfl_rushing$Player = word(nfl_rushing$Player, start = 1, end = 1, sep = "-")
nfl_rushing$Player = gsub('*', '', nfl_rushing$Player, fixed = T)
nfl_rushing$Player = gsub('+', '', nfl_rushing$Player, fixed = T)


nfl_receiving$Player = word(nfl_receiving$Player, start = 1, end = 1, sep = "-")
nfl_receiving$Player = strsplit(nfl_receiving$Player, split = '\\',
                                fixed = T)

nfl_receiving$Player= lapply(nfl_receiving$Player,
                             function(l) l[[1]])
                            
nfl_receiving$Player = gsub('*', '', nfl_receiving$Player, fixed = T)
nfl_receiving$Player = gsub('+', '', nfl_receiving$Player, fixed = T)


nfl = full_join(nfl_rushing,
                nfl_receiving)

nfl = nfl %>% dplyr::select(Player, Season,
                            Tm, everything())



nfl = nfl %>% arrange(Player, Season)


nfl.rookie = nfl %>% group_by(Player) %>% dplyr::filter(row_number()==1)


colnames(nfl.rookie)[2:ncol(nfl.rookie)] = paste('nfl_',
                                   colnames(nfl.rookie)[2:ncol(nfl.rookie)],
                                   sep = '')


colnames(cfb)[2:ncol(cfb)] = paste('cfb_',
                                   colnames(cfb)[2:ncol(cfb)],
                                   sep = '')


cfb = cfb %>% arrange(Player,
                      cfb_Season)

cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_G = cfb_G,
  cfb_Junior_G = lag(cfb_G, n = 1),
  cfb_Sophomore_G = lag(cfb_G, n = 2),
  cfb_Freshman_G = lag(cfb_G, n = 3),
  cfb_RS_Freshman_G = lag(cfb_G, n = 4)

)

cfb = cfb %>% dplyr::select(-cfb_G)


cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_Att = cfb_Att,
  cfb_Junior_Att = lag(cfb_Att, n = 1),
  cfb_Sophomore_Att = lag(cfb_Att, n = 2),
  cfb_Freshman_Att = lag(cfb_Att, n = 3),
  cfb_RS_Freshman_Att = lag(cfb_Att, n = 4)
  
)





cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_RushingYds = cfb_RushingYds,
  cfb_Junior_RushingYds = lag(cfb_RushingYds, n = 1),
  cfb_Sophomore_RushingYds = lag(cfb_RushingYds, n = 2),
  cfb_Freshman_RushingYds = lag(cfb_RushingYds, n = 3),
  cfb_RS_Freshman_RushingYds = lag(cfb_RushingYds, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_RushingYds)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_RushingAvg = cfb_RushingAvg,
  cfb_Junior_RushingAvg = lag(cfb_RushingAvg, n = 1),
  cfb_Sophomore_RushingAvg = lag(cfb_RushingAvg, n = 2),
  cfb_Freshman_RushingAvg = lag(cfb_RushingAvg, n = 3),
  cfb_RS_Freshman_RushingAvg = lag(cfb_RushingAvg, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_RushingAvg)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_RushingTD = cfb_RushingTD,
  cfb_Junior_RushingTD = lag(cfb_RushingTD, n = 1),
  cfb_Sophomore_RushingTD = lag(cfb_RushingTD, n = 2),
  cfb_Freshman_RushingTD = lag(cfb_RushingTD, n = 3),
  cfb_RS_Freshman_RushingTD = lag(cfb_RushingTD, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_RushingTD)


cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_Rec = cfb_Rec,
  cfb_Junior_Rec = lag(cfb_Rec, n = 1),
  cfb_Sophomore_Rec = lag(cfb_Rec, n = 2),
  cfb_Freshman_Rec = lag(cfb_Rec, n = 3),
  cfb_RS_Freshman_Rec = lag(cfb_Rec, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_Rec)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ReceivingYds = cfb_ReceivingYds,
  cfb_Junior_ReceivingYds = lag(cfb_ReceivingYds, n = 1),
  cfb_Sophomore_ReceivingYds = lag(cfb_ReceivingYds, n = 2),
  cfb_Freshman_ReceivingYds = lag(cfb_ReceivingYds, n = 3),
  cfb_RS_Freshman_ReceivingYds = lag(cfb_ReceivingYds, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_ReceivingYds)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ReceivingTD = cfb_ReceivingTD,
  cfb_Junior_ReceivingTD = lag(cfb_ReceivingTD, n = 1),
  cfb_Sophomore_ReceivingTD = lag(cfb_ReceivingTD, n = 2),
  cfb_Freshman_ReceivingTD = lag(cfb_ReceivingTD, n = 3),
  cfb_RS_Freshman_ReceivingTD = lag(cfb_ReceivingTD, n = 4)
)

cfb = cfb %>% dplyr::select(-cfb_ReceivingTD)


cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ScrimmagePlays = cfb_ScrimmagePlays,
  cfb_Junior_ScrimmagePlays = lag(cfb_ScrimmagePlays, n = 1),
  cfb_Sophomore_ScrimmagePlays = lag(cfb_ScrimmagePlays, n = 2),
  cfb_Freshman_ScrimmagePlays = lag(cfb_ScrimmagePlays, n = 3),
  cfb_RS_Freshman_ScrimmagePlays = lag(cfb_ScrimmagePlays, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_ScrimmagePlays)


cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ScrimmageYds = cfb_ScrimmageYds,
  cfb_Junior_ScrimmageYds = lag(cfb_ScrimmageYds, n = 1),
  cfb_Sophomore_ScrimmageYds = lag(cfb_ScrimmageYds, n = 2),
  cfb_Freshman_ScrimmageYds = lag(cfb_ScrimmageYds, n = 3),
  cfb_RS_Freshman_ScrimmageYds = lag(cfb_ScrimmageYds, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_ScrimmageYds)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ScrimmageAvg = cfb_ScrimmageAvg,
  cfb_Junior_ScrimmageAvg = lag(cfb_ScrimmageAvg, n = 1),
  cfb_Sophomore_ScrimmageAvg = lag(cfb_ScrimmageAvg, n = 2),
  cfb_Freshman_ScrimmageAvg = lag(cfb_ScrimmageAvg, n = 3),
  cfb_RS_Freshman_ScrimmageAvg = lag(cfb_ScrimmageAvg, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_ScrimmageAvg)



cfb = cfb %>% group_by(Player) %>% mutate(
  cfb_Senior_ScrimmageTD = cfb_ScrimmageTD,
  cfb_Junior_ScrimmageTD = lag(cfb_ScrimmageTD, n = 1),
  cfb_Sophomore_ScrimmageTD = lag(cfb_ScrimmageTD, n = 2),
  cfb_Freshman_ScrimmageTD = lag(cfb_ScrimmageTD, n = 3),
  cfb_RS_Freshman_ScrimmageTD = lag(cfb_ScrimmageTD, n = 4)
  
)

cfb = cfb %>% dplyr::select(-cfb_ScrimmageTD)

cfb = cfb %>% arrange(Player,
                       desc(cfb_Season)) %>% group_by(Player) %>%
  dplyr::filter(row_number() == 1)




football = left_join(nfl.rookie,
                     cfb, by = 'Player')

nfl_combine = nfl_combine %>% dplyr::select(-Rk)

nfl_combine = nfl_combine %>% dplyr::select(Player,
                                            Year,
                                            everything())

colnames(nfl_combine)[2:ncol(nfl_combine)] = paste('combine',
                                               colnames(nfl_combine)[2:ncol(nfl_combine)],
                                               sep = '_')


football = left_join(football,
                     nfl_combine)

football = football[which(paste(football$Player,
                                 football$combine_School,
                                 sep = ' ') 
                                 != 'Mike Davis Texas'),]

write.csv(football,
          file = 'NFL-CFB-Combine.csv', row.names = F)


