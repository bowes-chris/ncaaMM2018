datapath <- './data/'
logpath <- './logs/'
trainpath <- './modeldata/'
year <- 2015

teams <- read.csv(paste(datapath, 'Teams.csv', sep=''))
seasons <- read.csv(paste(datapath, 'Seasons.csv', sep=''))
conferences <- read.csv(paste(datapath, 'TeamConferences.csv', sep=''))
rsdr <- read.csv(paste(datapath, 'RegularSeasonDetailedResults.csv', sep=''))
rscr <- read.csv(paste(datapath, 'RegularSeasonCompactResults.csv', sep=''))
tdr <- read.csv(paste(datapath, 'NCAATourneyDetailedResults.csv', sep=''))
tcr <- read.csv(paste(datapath, 'NCAATourneyCompactResults.csv', sep=''))
Tseeds <- read.csv(paste(datapath, 'NCAATourneySeeds.csv', sep=''))
Tslots <- read.csv(paste(datapath, 'NCAATourneySlots.csv', sep=''))
submission  <- read.csv(paste(datapath, 'SampleSubmissionStage1.csv', sep=''))

rsdrSeason <- rsdr[rsdr$Season == year,]
rscrSeason <- rscr[rscr$Season == year,]

tid <- teams[(teams$FirstD1Season <= year & teams$LastD1Season >= year),]
cid <- conferences[conferences$Season == year,]

mid <- matrix(unlist(strsplit(as.character(submission$ID), '_')), ncol =3)
subseason <- vector(length = 0)
subA <- vector(length = 0)
subB <- vector(length = 0)

for (i in 1:nrow(submission)) {
    temp <- unlist(strsplit(as.character(submission$ID), '_'))
    subseason <- c(subseason, temp[1])
    subA <- c(subA, temp[2])
    subB <- c(subB, temp[3])
}


Wstats <- rsdrSeason[,c("WTeamID", "WLoc", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")]

Lstats <- rsdrSeason[,c("LTeamID", "WLoc", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")]

for (i in 1:length(tid$TeamID)) {
    Wstats$conf[Wstats$WTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    Lstats$conf[Lstats$LTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    tid$conf[i] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
}

aggStats <- tid[c(1,2,5)]

for (i in 1:nrow(tid)) {
  #tid <- aggStats$Team_Id[i]  
  n <- sum(c(rsdrSeason$WTeamID == tid$TeamID[i], rsdrSeason$LTeamID ==  tid$TeamID[i]))  
  #season wins
  aggStats$wins[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WTeamID == tid$TeamID[i])
  aggStats$loss[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LTeamID == tid$TeamID[i])  
  #season fgm, fga, and fgp
  aggStats$fga[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFGA[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFGA[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  aggStats$fgm[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFGM[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFGM[which(rsdrSeason$LTeamID == tid$TeamID[i])])  
  #season 3fgm, 3fga, 3fgp
  aggStats$fga3[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFGA3[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFGA3[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  aggStats$fgm3[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFGM3[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFGM3[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season ftm, fta, ftp
  aggStats$fta[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFTA[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFTA[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  aggStats$ftm[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WFTM[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LFTM[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season Off, Def, Tot reb per game
  aggStats$or[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WOR[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LOR[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  aggStats$dr[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WDR[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LDR[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season assist total
  aggStats$ast[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WAst[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LAst[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season turn over total
  aggStats$to[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WTO[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LTO[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season steal total
  aggStats$stl[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WStl[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LStl[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season blocks total
  aggStats$blk[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WBlk[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LBlk[which(rsdrSeason$LTeamID == tid$TeamID[i])])
  #season personal fouls total
  aggStats$pf[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WPF[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$LPF[which(rsdrSeason$LTeamID == tid$TeamID[i])])
}

#total possessions
aggStats$postot <- aggStats$fga + aggStats$to + (.475 * aggStats$fta) - aggStats$or
#per possession stats, poss/game avg
perPosStats <- cbind(aggStats[,1:5], signif(aggStats[6:18]/(aggStats$wins+aggStats$loss), digits = 4), 
signif(aggStats[19]/(aggStats$wins+aggStats$loss), digits = 4))

tournStat <- perPosStats[perPosStats$Team_Id == tournTeams$Team,]