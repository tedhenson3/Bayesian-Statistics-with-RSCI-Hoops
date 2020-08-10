
library(readr)
library(tidyverse)
setwd('~/Bayesian Statistics with RSCI Hoops/Data')




### template for read_csv looper ###
RSCI = data.frame()

for(i in 1998:2020){
  
  rsci = as.data.frame(read_csv(paste('rsci ', i, '.csv', sep = '')))
  colnames(rsci) = rsci[1,]
  rsci = rsci %>% dplyr::select(-`NA`)
  rsci = rsci[-c(1),]
  rsci$Year = i
  colnames(rsci)[which(colnames(rsci) == 'RSCI')] = 'Composite.Ranking'

  
  rsci = rsci[,order(colnames(rsci))]
  
  #money$Season = i
  #assign(paste0("rsci-", i), rsci)
  
  library(plyr)
  RSCI = plyr::rbind.fill(rsci, RSCI)
  
  
}

#### End ####


#### Combining Experts with Scout Service

#testing to see if scout = service
#Dave Telep
rows = RSCI %>% dplyr::filter(!is.na(ESPN) & !is.na(DT))
nrow(rows)

RSCI$ESPN = ifelse(is.na(RSCI$ESPN),
                   RSCI$DT,
                   RSCI$ESPN)

# RC = Rivals
rows = RSCI %>% dplyr::filter(!is.na(Rivals) & !is.na(RC))
nrow(rows)

RSCI$Rivals = ifelse(is.na(RSCI$Rivals),
                   RSCI$RC,
                   RSCI$Rivals)

#### Deselect Experts that are no longer in composite index ####

RSCI = RSCI %>% dplyr::select(-c(RU,
                                 RB,
                                 IR,
                                 FS,
                                 SS,
                                 SN,
                                 FC,
                                 RH,
                                 SI,
                                 AS,
                                 HS,
                                 SC,
                                 `RH/PS`,
                                 HM,
                                 PrepStars,
                                 Scout,
                                 MP,
                                 PS))



## deselect experts that are synonymous with ESPN and Rivals
# Dave Telep now with Spurs, not ESPN
RSCI = RSCI %>% dplyr::select(-RC,
                              -DT)


#### Get rid of useless bio columns ####
RSCI = RSCI %>% dplyr::select(-Rd,
                              -Pk,
                              -WS)





RSCI$Player = gsub('(college)', '', RSCI$Player, fixed = T)


#### Trim ws from scout player ####
RSCI$Player = trimws(RSCI$Player, which = "both")
#### End ####

rating.creator = function(x){
  x = gsub('T', '', x)
  x = as.numeric(x)
  x = -x
  x = x + 101
  x = x / 10
  return(x)
  
}
RSCI$Composite.Rating = rating.creator(x = RSCI$Composite.Ranking)

RSCI$ESPN = rating.creator(x = RSCI$ESPN)
RSCI$Rivals = rating.creator(x = RSCI$Rivals)
RSCI$`247Sports` = rating.creator(x = RSCI$`247Sports`)
RSCI$VC = rating.creator(x = RSCI$VC)


RSCI = RSCI %>% dplyr::select(Player,
                              Year,
                              Composite.Rating,
                              ESPN,
                              Rivals,
                              VC,
                              `247Sports`,
                              College,
                              everything())

RSCI = RSCI %>% dplyr::arrange(desc(Composite.Rating))



#### Creating a college seasons variable
RSCI$Year = as.numeric(RSCI$Year)
RSCI$Draft = as.numeric(RSCI$Draft)
#RSCI$College.Seasons = RSCI$Draft - RSCI$Year


#### Read in NBA Data ####

### template for read_csv looper ###
NBA = data.frame()

nba.year.list = c('98-99',
                  '99-00',
                  '00-01',
                  '01-02',
                  '02-03',
                  '03-04',
                  '04-05',
                  '05-06',
                  '06-07',
                  '07-08',
                  '08-09',
                  '09-10',
                  '10-11',
                  '11-12',
                  '12-13',
                  '13-14',
                  '14-15',
                  '15-16',
                  '16-17',
                  '17-18',
                  '18-19',
                  '19-20')

for(i in 1:length(nba.year.list)){
  
  nba = 
    as.data.frame(
      read_csv(
        paste('Advanced NBA Player Stats ', nba.year.list[i], '.csv', sep = '')))
  #money$Season = i
  colnames(nba)[ncol(nba)] = 'VORP'
  
  #assign(paste0("nba-", nba.year.list[i]), nba)
  
  NBA = rbind(nba, NBA)
  
  
}

#### End ###

#### Fix Player Names ####


player.grabber = function(x){
  
  name = strsplit(x, "\\\\|[^[:print:]]",fixed=FALSE)
  name = trimws(name[[1]][1], which = 'both')
  return(name)
  
  
}
NBA$Player = sapply(NBA$Player, FUN = player.grabber)

#### End ####

NBA = NBA %>% dplyr::select(-Rk, -`X20`, -`X25`)

### will have to come up with fuzzy matching function possibly

RSCI$Player = gsub('Louis Williams', 'Lou Williams', RSCI$Player)
RSCI$Player = gsub('Mohamed Bamba', 'Mo Bamba', RSCI$Player)
RSCI$Player = gsub('Wendell Carter', 'Wendell Carter Jr', RSCI$Player)

RSCI$Player = gsub('.', '', RSCI$Player, fixed = T)
RSCI$Player = gsub("'", "", RSCI$Player, fixed = T)
RSCI$Player = gsub("-", "", RSCI$Player, fixed = T)
library(stringi)
RSCI$Player = stri_trans_general(str = RSCI$Player, 
                                 id = "Latin-ASCII")

NBA$Player = gsub('.', '', NBA$Player, fixed = T)
NBA$Player = gsub("'", "", NBA$Player, fixed = T)
NBA$Player = gsub("-", "", NBA$Player, fixed = T)


NBA$Player = stri_trans_general(str = NBA$Player, 
                                id = "Latin-ASCII")



RSCI = RSCI %>% dplyr::filter(Year < 2019)

RSCI$ID = tolower(RSCI$Player)
NBA$ID = tolower(NBA$Player)
see = anti_join(RSCI, 
                NBA,
                by = c("ID"))
nrow(see)

### players found had either not entered draft or never made league

basketball = left_join(RSCI, 
                       NBA,
                       by = c("ID"))

basketball = basketball %>% dplyr::select(-Player.x)

colnames(basketball)[which(colnames(basketball) == 'Player.y')] = 'Player'

#### have 247 from 2013 onward
#### have VC from 2011 onward
#### have Rivals from 2003 onward
#### have Dave Telep / ESPN from 2000 onward

#basketball = basketball[which(!is.na(basketball$Composite.Rating)),]



basketball$Age= basketball$Age - 18

#get rid of duplicate rows


basketball = basketball %>% dplyr::arrange(Player,
                                           Age,
                                           desc(G))

library(tidyverse)
basketball = basketball %>% 
  dplyr::group_by(Player, Age) %>%
  dplyr::filter(row_number() == 1)

basketball = ungroup(basketball)

basketball = basketball %>% dplyr::select(Player,
                                          ESPN,
                                          Pos,
                                          Tm.y,
                                          Draft,
                                          WS,
                                          everything())

basketball = basketball %>% dplyr::arrange(desc(ESPN),
                                           desc(Draft))

basketball = basketball %>% dplyr::filter(MP > 100)

basketball$y = basketball$`WS/48` * 100



basketball = basketball %>% dplyr::filter(!is.na(Composite.Rating))
#                                             !is.na(`247Sports`) & 
#                                             !is.na(VC))
basketball$Is.C = ifelse(basketball$Pos %in% 
                           c('C', 'C-PF','PF-C'),
                             1, 0)

basketball$Is.Big = ifelse(basketball$Pos %in% 
                           c('C', 'PF',
                             'C-PF','PF-C'),
                         1, 0)

basketball$Is.Forward = ifelse(basketball$Pos %in% 
                             c('PF', 'SF',
                               'PF-SF', 'SF-PF'),
                           1, 0)

basketball$Is.Guard = ifelse(basketball$Pos %in% 
                             c('PG', 'SG',
                               'PG-SG', 'SG-PG'),
                           1, 0)

basketball$Is.Wing = ifelse(basketball$Pos %in% 
                               c('SF', 'SG',
                                 'SF-SG', 'SG-SF'),
                             1, 0)

basketball = basketball %>%
  dplyr::group_by(Player) %>%
  dplyr::filter(row_number() == 1)


basketball$Composite.Rating = scale(basketball$Composite.Rating)
#nrow(basketball)



