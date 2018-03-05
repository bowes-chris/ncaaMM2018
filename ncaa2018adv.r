source('ncaaFunctions.r')

datapath <- './data/'
logpath <- './logs/'
trainpath <- './modeldata/'
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

rsdrSeason <- rsdr[rsdr$Season == year,]
rscrSeason <- rscr[rscr$Season == year,]

tid <- teams[(teams$FirstD1Season <= year & teams$LastD1Season >= year),]
cid <- conferences[conferences$Season == year,]

Wstats <- rsdrSeason[,c("WTeamID", "WLoc", "WScore", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")]

Lstats <- rsdrSeason[,c("LTeamID", "WLoc", "LScore", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")]
    
Lstats$WLoc[Wstats$WLoc == 'H'] <- 'A'
Lstats$WLoc[Wstats$WLoc == 'A'] <- 'H'

#add conference
for (i in 1:length(tid$TeamID)) {
    Wstats$conf[Wstats$WTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    Lstats$conf[Lstats$LTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
}
Wstats$WTeamID <-factor(Wstats$WTeamID)
Lstats$LTeamID <-factor(Lstats$LTeamID)

Wstats <- cbind(Wstats[c("WTeamID", "WLoc", "conf")],
    Wstats[c("WScore", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")])#/Wstats$WPos, 
    #Wstats["WPos"])

Lstats <- cbind(Lstats[c("LTeamID", "WLoc", "conf")],
    Lstats[c("LScore", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")])#/Lstats$LPos, 
    #Lstats["LPos"])

statcolnames <- c("teamID", "loc", "conf", "pts", "fgm","fga","fgm3","fga3","ftm","fta",
    "or","dr","ast", "to", "stl", "blk", "pf")#, "pos")

colnames(Wstats) <- statcolnames

colnames(Lstats) <- statcolnames

teamA <- Wstats[0,]
teamB <- Lstats[0,]

for (i in 1:nrow(Wstats)) {
    if(rnorm(1) >=0) {
        teamA <- rbind(teamA, Wstats[i,])
        teamB <- rbind(teamB, Lstats[i,])
    }
    else {
        teamA <- rbind(teamA, Lstats[i,])
        teamB <- rbind(teamB, Wstats[i,])
    }            
}
Wlabel <- factor(Wstats$teamID)
Clabel <- ifelse(as.numeric(teamA$teamID) == as.numeric(Wlabel), 1, 0)

colnames(teamA) <- paste('A', statcolnames, sep = '')
colnames(teamB) <- paste('B', statcolnames, sep = '')

rsGames <- cbind("class" = Clabel, rsdrSeason["NumOT"], teamA, teamB)
rsadvA <- rsGames[c('AteamID','Aconf')]
#game pace
rsadvA$Apace <- unlist(gamePace(rsGames, 'A'))
# game True Shooting Percentage
rsadvA$Atsper <- unlist(gametsper(rsGames, 'A'))
# game fga3 per fga
rsadvA$Athrpar <- unlist(gamethreePAr(rsGames, 'A'))
# team fta per fga
rsadvA$Aftrate <- unlist(gameftRate(rsGames, 'A'))
# team ast per
rsadvA$Aastper <- unlist(gameAstPer(rsGames, 'A'))
# total possessions / game
rsadvA$Apos <- unlist(gamePos(rsGames, 'A'))
rsadvA$Aoppos <- unlist(gamePos(rsGames, 'B'))
# Offensive Efficiency, Deffinsive Efficiency
rsadvA$Aoeff <- unlist(gameOeff(rsGames, 'A'))
rsadvA$Adeff <- unlist(gameDeff(rsGames, 'A'))
# team DR/OR per
rsadvA$Adrper <- unlist(gameDRPer(rsGames, 'A'))
rsadvA$Aorper <- unlist(gameORPer(rsGames, 'A'))
# team TO ratio
rsadvA$Atorat <- unlist(gameTORat(rsGames, 'A'))
# team effective FG
rsadvA$Aefg <- unlist(gameeffFG(rsGames, 'A'))

rsadvB <- rsGames[c('BteamID','Bconf')]
#game pace
rsadvB$Bpace <- unlist(gamePace(rsGames, 'B'))
# game True Shooting Percentage
rsadvB$Btsper <- unlist(gametsper(rsGames, 'B'))
# game fga3 per fga
rsadvB$Bthrpar <- unlist(gamethreePAr(rsGames, 'B'))
# team fta per fga
rsadvB$Bftrate <- unlist(gameftRate(rsGames, 'B'))
# team ast per
rsadvB$Bastper <- unlist(gameAstPer(rsGames, 'B'))
# total possessions / game
rsadvB$Bpos <- unlist(gamePos(rsGames, 'B'))
rsadvB$Boppos <- unlist(gamePos(rsGames, 'A'))
# Offensive Efficiency, Deffinsive Efficiency
rsadvB$Boeff <- unlist(gameOeff(rsGames, 'B'))
rsadvB$Bdeff <- unlist(gameDeff(rsGames, 'B'))
# team DR/OR per
rsadvB$Bdrper <- unlist(gameDRPer(rsGames, 'B'))
rsadvB$Borper <- unlist(gameORPer(rsGames, 'B'))
# team TO ratio
rsadvB$Btorat <- unlist(gameTORat(rsGames, 'B'))
# team effective FG
rsadvB$Befg <- unlist(gameeffFG(rsGames, 'B'))
