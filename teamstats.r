datapath <- './data/'
logpath <- './logs/'
trainpath <- './modeldata/'

source('ncaaFunctions.r')
year <- 2017

teams <- read.csv(paste(datapath, 'Teams.csv', sep=''))
seasons <- read.csv(paste(datapath, 'Seasons.csv', sep=''))
conferences <- read.csv(paste(datapath, 'TeamConferences.csv', sep=''))
rsdr <- read.csv(paste(datapath, 'RegularSeasonDetailedResults.csv', sep=''))
rscr <- read.csv(paste(datapath, 'RegularSeasonCompactResults.csv', sep=''))
tdr <- read.csv(paste(datapath, 'NCAATourneyDetailedResults.csv', sep=''))
tcr <- read.csv(paste(datapath, 'NCAATourneyCompactResults.csv', sep=''))
Tseeds <- read.csv(paste(datapath, 'NCAATourneySeeds.csv', sep=''))
Tslots <- read.csv(paste(datapath, 'NCAATourneySlots.csv', sep=''))
submission <- read.csv(paste(datapath, 'SampleSubmissionStage1.csv', sep = ''))
#events <- read.csv(paste(datapath, 'Events_2017.csv', sep = ''))
players <- read.csv(paste(datapath, 'Players_2017.csv', sep = ''))

rsdrSeason <- rsdr[rsdr$Season == year,]
rscrSeason <- rscr[rscr$Season == year,]

tid <- teams[(teams$FirstD1Season <= year & teams$LastD1Season >= year),]
cid <- conferences[conferences$Season == year,]

subseason <- vector(length = 0)
subA <- vector(length = 0)
subB <- vector(length = 0)

for (i in 1:nrow(submission)) {
    temp <- unlist(strsplit(as.character(submission$ID[i]), '_'))
    subseason <- c(subseason, temp[1])
    subA <- c(subA, temp[2])
    subB <- c(subB, temp[3])
}

mid <- data.frame("season" = subseason, "TeamAID" = subA, "TeamBID" = subB)

#Wstats <- rsdrSeason[,c("WTeamID", "WLoc", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    #"WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")]

#Lstats <- rsdrSeason[,c("LTeamID", "WLoc", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    #"LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")]

for (i in 1:length(tid$TeamID)) {
#    Wstats$conf[Wstats$WTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
#    Lstats$conf[Lstats$LTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    tid$conf[i] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
}

aggStats <- tid[c(1, 2, 5)]
for (i in 1:nrow(tid)) {
    #tid <- aggStats$Team_Id[i]  
    n <- sum(c(rsdrSeason$WTeamID == tid$TeamID[i], rsdrSeason$LTeamID ==  tid$TeamID[i]))  
    #season wins
    aggStats$wins[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$WTeamID == tid$TeamID[i])
    aggStats$loss[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LTeamID == tid$TeamID[i])

    aggStats$pts[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rscrSeason$WScore[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rscrSeason$LScore[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    aggStats$ptsa[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rscrSeason$LScore[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rscrSeason$WScore[which(rsdrSeason$LTeamID == tid$TeamID[i])])
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
    #season overtime total
    aggStats$ot[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$NumOT[which(rsdrSeason$WTeamID == tid$TeamID[i])])

    #season opfgm, opfga, and opfgp
    aggStats$opfga[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFGA[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFGA[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    aggStats$opfgm[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFGM[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFGM[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op3fgm, op3fga, op3fgp
    aggStats$opfga3[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFGA3[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFGA3[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    aggStats$opfgm3[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFGM3[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFGM3[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season opftm, opfta, opftp
    aggStats$opfta[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFTA[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFTA[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    aggStats$opftm[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LFTM[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WFTM[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season opOff, opDef, opTot reb per game
    aggStats$opor[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LOR[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WOR[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    aggStats$opdr[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LDR[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WDR[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op assist total
    aggStats$opast[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LAst[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WAst[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op turn over total
    aggStats$opto[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LTO[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WTO[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op steal total
    aggStats$opstl[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LStl[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WStl[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op blocks total
    aggStats$opblk[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LBlk[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WBlk[which(rsdrSeason$LTeamID == tid$TeamID[i])])
    #season op personal fouls total
    aggStats$oppf[which(aggStats$TeamID == tid$TeamID[i])] <- sum(rsdrSeason$LPF[which(rsdrSeason$WTeamID == tid$TeamID[i])]) + sum(rsdrSeason$WPF[which(rsdrSeason$LTeamID == tid$TeamID[i])])
}

tStats <- tid[c(1, 2, 5)]
#team pace
tStats$pace <- unlist(teamPace(aggStats) / teamGames(aggStats))
# team True Shooting Percentage
tStats$tsper <- unlist(trueSPer(aggStats))
# team fga3 per fga
tStats$thrPar <- unlist(threePAr(aggStats))
# team fta per fga
tStats$ftRate <- unlist(ftRate(aggStats))
#tStats$trbPer <- tRePer(aggStats)
tStats$astPer <- unlist(teamAstPer(aggStats))
# total possessions / game
tStats$postot <- unlist(teamPos(aggStats) / teamGames(aggStats))
tStats$oppos <- unlist(teamOpPos(aggStats) / teamGames(aggStats))
# win percentage 
tStats$winp <- unlist(aggStats$wins/ (aggStats$wins + aggStats$loss))
# Offensive Efficiency, Deffinsive Efficiency
tStats$oeff <- unlist(teamOeff(aggStats))
tStats$deff <- unlist(teamDeff(aggStats))
# point diff, eff diff
#tStats$ptdiff <- unlist((aggStats$pts - aggStats$ptsa) / teamGames(aggStats))
#tStats$effdiff <- unlist((teamOeff(aggStats) - teamDeff(aggStats)))# / teamGames(aggStats)
# team DR/OR per
tStats$drPer <- unlist(teamDRPer(aggStats))
tStats$orPer <- unlist(teamORPer(aggStats))
# team TO ratio
tStats$toRat <- unlist(teamTORat(aggStats))
# team effective FG
tStats$eFG <- unlist(effFG(aggStats))
# calculate RPI ranking
#tStats$rpi <- rpi(tStats, rsdrSeason)



rownames(tStats) <- NULL
perPosStats <- cbind(aggStats[c("TeamID","TeamName","conf","wins","loss")],
   aggStats[c("fgm", "fga", "fgm3", "fga3", "ftm", "fta", "or", "dr", "ast", "to", "stl", "blk","pf",
               "ot", "opfga", "opfgm", "opfga3", "opfgm3", "opfta", "opftm", "opor", "opdr", "opast",
               "opto", "opstl", "opblk", "oppf")]) # / aggStats$postot,
#   scale(aggStats["postot"]))
statcolnames <- c("TeamID", "TeamName", "conf", "pace", "tsper", "thrPar", "ftRate", "astPer", "postot", "oppos", "winp", "oeff"
    ,"deff", "ptdiff", "effdiff", "drPer", "orPer", "toRat", "eFG")#,"rpi"

#tournStat <- perPosStats[perPosStats$Team_Id == tournTeams$Team,]
smid <- mid[mid$season == year,]

tStatA <- tStats[0,c("TeamID", "TeamName", "conf", "pace", "tsper", "thrPar", "ftRate", "astPer", "postot", "oppos", "winp", "oeff"
    ,"deff", "ptdiff", "effdiff", "drPer", "orPer", "toRat", "eFG")]
tStatB <- teamStats[0,c("TeamID", "TeamName", "conf", "pace", "tsper", "thrPar", "ftRate", "astPer", "postot", "oppos", "winp", "oeff"
    ,"deff", "ptdiff", "effdiff", "drPer", "orPer", "toRat", "eFG")]

perPosTeamA <- perPosStats[0,c("TeamID", "conf", "fgm", "fga", "fgm3", "fga3", "ftm", "fta", "or", "dr", "ast", "to", "stl", "blk","pf",
                                "ot", "opfga", "opfgm", "opfga3", "opfgm3", "opfta", "opftm", "opor", "opdr", "opast", "opto", "opstl", "opblk", "oppf")]
perPosTeamB <- perPosStats[0,c("TeamID", "conf", "fgm", "fga", "fgm3", "fga3", "ftm", "fta", "or", "dr", "ast", "to", "stl", "blk","pf",
                                "ot", "opfga", "opfgm", "opfga3", "opfgm3", "opfta", "opftm", "opor", "opdr", "opast", "opto", "opstl", "opblk", "oppf")]    
for (i in 1:nrow(smid)) {        
    tStatA <- rbind(tStatA, tStats[teamStats$TeamID == smid$TeamAID[i], c("TeamID", "TeamName", "conf", "pace", "tsper", "thrPar", "ftRate", "astPer", "postot", "oppos", "winp", "oeff",
     "deff", "ptdiff", "effdiff", "drPer", "orPer", "toRat", "eFG")])
    #tStatB <- rbind(tStatB, tStats[tStats$TeamID == smid$TeamBID[i],])
}

#one hot teams
tnames <- perPosTeamA$TeamID[0]
#TnameA <- ifelse(as.numeric(teamA$teamID) == as.numeric(tid[1]),1,0)
#TnameB <- ifelse(as.numeric(teamB$teamID) == as.numeric(tid[1]),1,0)
for (i in 1:length(tid$TeamID)) {    
    temp <- ifelse((perPosTeamA$TeamID == tid$TeamID[i]) | (perPosTeamB$TeamID == tid$TeamID[i]) ,1,0)
    #tempB <- ifelse(as.numeric(teamB$teamID) == as.numeric(tid[i]),1,0)
    tnames <- cbind(tnames, temp)
    #TnameB <- cbind(TnameB, tempB)
}
colnames(tnames) <- tid$TeamName 

#one hot conferences
cnames <- as.numeric(cid$ConfAbbrev[0])
clist <- sort(unique(cid$ConfAbbrev))
for (i in 1:length(clist)) {
    temp <- ifelse((perPosTeamA$conf == clist[i]) | (perPosTeamB$conf == clist[i]), 1, 0)
    #tempB <- ifelse(as.character(teamB$conf) == as.character(conf[i]), 1, 0)
    cnames <- cbind(cnames, temp)
    #cnamesB <- cbind(cnamesB, tempB)
}
colnames(cnames) <- clist

#one hot loc
locs <- data.frame("away" = 0, "home" = 0, "neutral" = 1)
for (i in 1:nrow(perPosTeamA)-1) {
    temp <- locs[i,]
    locs <- rbind(locs, temp)
}

colnames(perPosTeamA) <- paste('TeamA', colnames(perPosTeamA), sep = '')
colnames(perPosTeamB) <- paste('TeamB', colnames(perPosTeamB), sep = '')

test <- cbind(perPosTeamA[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApostot")], 
    perPosTeamB[c("TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor","TeamBdr","TeamBast", "TeamBto",
    "TeamBstl", "TeamBblk", "TeamBpf", "TeamBpostot")], cnames, tnames, locs)

write.csv(test, file = paste(trainpath, 'tourney', year, '.csv', sep = ''), row.names = FALSE)