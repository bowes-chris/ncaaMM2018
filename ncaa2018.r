library(caret)

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

rsdrSeason <- rsdr[rsdr$Season == year,]
rscrSeason <- rscr[rscr$Season == year,]

Wstats <- rsdrSeason[,c("WTeamID", "WLoc", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")]

Lstats <- rsdrSeason[,c("LTeamID", "WLoc", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")]

#tid <- sort(unique(teams$TeamID[teams$FirstD1Season <= year & teams$LastD1Season >= year]))
tid <- teams[(teams$FirstD1Season <= year & teams$LastD1Season >= year),]
cid <- conferences[conferences$Season == year,]

Lstats$WLoc[Wstats$WLoc == 'H'] <- 'A'
Lstats$WLoc[Wstats$WLoc == 'A'] <- 'H'

Wstats$WPos <- Wstats$WFGA + Wstats$WTO + (.475 * Wstats$WFTA) - Wstats$WOR
Lstats$LPos <- Lstats$LFGA + Lstats$LTO + (.475 * Lstats$LFTA) - Lstats$LOR

#add conference
for (i in 1:length(tid$TeamID)) {
    Wstats$conf[Wstats$WTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    Lstats$conf[Lstats$LTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
}

Wstats$WTeamID <-factor(Wstats$WTeamID)
Lstats$LTeamID <-factor(Lstats$LTeamID)

Wstats <- cbind(Wstats[c("WTeamID", "WLoc", "conf")],
    Wstats[c("WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")],#/Wstats$pos, 
    Wstats["WPos"])

Lstats <- cbind(Lstats[c("LTeamID", "WLoc", "conf")],
    Lstats[c("LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")],#/Lstats$pos, 
    Lstats["LPos"])

statcolnames <- c("teamID", "loc", "conf", "fgm","fga","fgm3","fga3","ftm","fta",
    "or","dr","ast", "to", "stl", "blk", "pf", "pos")

colnames(Wstats) <- statcolnames

colnames(Lstats) <- statcolnames

teamA <- Wstats[0,]
teamB <- Lstats[0,]


#Wnames <- lapply(Wlabel, function (x) { ifelse(teams$Team_Name[as.numeric(teams$Team_Id) == as.numeric(x)],1,0) })

# using for loop, but need to replace with lapply
for (i in 1:nrow(Wstats)) {
#    if (Wstats$loc[i] == 'H') {
#        teamA <- rbind(teamA, Wstats[i,])
#        teamB <- rbind(teamB, Lstats[i,])
#    }
#    else if (Wstats$loc[i] == 'A') {
#        teamA <- rbind(teamA, Lstats[i,])
#        teamB <- rbind(teamB, Wstats[i,])
#    }
#    else if(Wstats$loc[i] == 'N') {
        if(rnorm(1) >=0) {
            teamA <- rbind(teamA, Wstats[i,])
            teamB <- rbind(teamB, Lstats[i,])
        }
        else {
            teamA <- rbind(teamA, Lstats[i,])
            teamB <- rbind(teamB, Wstats[i,])
        }            
#    }
}

Wlabel <- factor(Wstats$teamID)
Clabel <- ifelse(as.numeric(teamA$teamID) == as.numeric(Wlabel), 1, 0)

#tlabel <- ifelse(as.numeric(teamA$teamID[1]) == as.numeric(Wlabel[1]),1,0)
#for(i in 2:length(Wlabel)) {
#    tlabel <- ifelse(as.numeric(teamA$teamID[i]) == as.numeric(Wlabel[i]),1,0)
#}

# one hot home and away
locs <- sort(unique(factor(Wstats$loc)))
Tloc <- Wstats$loc[0]
#Tloc <- ifelse(teamA$loc == locs[1],1,0)
#TlocB <- ifelse(teamB$loc == locs[1],1,0)
for (i in 1:nlevels(Wstats$loc)) {
    temp <- ifelse(teamA$loc == locs[i],1,0)
    #tempB <- ifelse(teamB$loc == locs[i],1,0)
    Tloc <- cbind(Tloc, temp)
    #TlocB <- cbind(TlocB, tempB)
}
colnames(Tloc) <- c("away", "home", "neutral")
#colnames(TlocB) <- c("away", "home", "neutral")

#one hot teams
#tnames <- teams$TeamName[teams$TeamID == tid[1]]
#for (i in 2:length(tid)) {
#    tnames[i] <- teams$TeamName[teams$Team_Id == tid[i]]
#}
tnames <- teamA$teamID[0]
#TnameA <- ifelse(as.numeric(teamA$teamID) == as.numeric(tid[1]),1,0)
#TnameB <- ifelse(as.numeric(teamB$teamID) == as.numeric(tid[1]),1,0)
for (i in 1:length(tid$TeamID)) {
    temp <- ifelse((teamA$teamID == tid$TeamID[i]) | (teamB$teamID == tid$TeamID[i]) ,1,0)
    #tempB <- ifelse(as.numeric(teamB$teamID) == as.numeric(tid[i]),1,0)
    tnames <- cbind(tnames, temp)
    #TnameB <- cbind(TnameB, tempB)
}
colnames(tnames) <- tid$TeamName #tnames
#colnames(TnameB) <- tid$TeamName #tnames

#one hot conferences
#conf <- sort(unique(factor(teams$Conference)))
#cnames <- ifelse(as.character(teamA$conf) == as.character(conf[1]), 1, 0)
#cnamesB <- ifelse(as.character(teamB$conf) == as.character(conf[1]), 1, 0)
cnames <- as.numeric(teamA$conf[0])
clist <- sort(unique(cid$ConfAbbrev))
for (i in 1:length(clist)) {
    temp <- ifelse((teamA$conf == clist[i]) | (teamB$conf == clist[i]), 1, 0)
    #tempB <- ifelse(as.character(teamB$conf) == as.character(conf[i]), 1, 0)
    cnames <- cbind(cnames, temp)
    #cnamesB <- cbind(cnamesB, tempB)
}
colnames(cnames) <- clist
#colnames(cnamesB) <- clist

colnames(teamA) <- paste('TeamA', statcolnames, sep = '')
colnames(teamB) <- paste('TeamB', statcolnames, sep = '')

diffVector <- teamA[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta",
    "TeamAor","TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos")] 
    - teamB[c("TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor","TeamBdr",
    "TeamBast", "TeamBto", "TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")]

#teamA <- teamA[-c(1:3)]
#teamAgameStats <- teamA

#teamB <- teamB[-c(1:3)]
#teamBgameStats <- teamB

#teamA <- scale(teamA, center = TRUE, scale = TRUE)
#posscaleA <- scale(teamA["TeamA_pos"], center = TRUE, scale = TRUE)
#tempA <- cbind(TnameA, cnamesA, TlocA, teamA)
#teamA <- cbind(teamA, tempA)
#teamA <- cbind(teamA[-ncol(teamA)], posscaleA, tempA)

#teamB <- scale(teamB, center = TRUE, scale = TRUE)
#posscaleB <- scale(teamB["TeamB_pos"], center = TRUE, scale = TRUE)
#tempB <- cbind(TnameB, cnamesB, TlocB, teamB)
#teamB <- cbind(teamB, tempB)
#teamB <- cbind(teamB[-ncol(teamB)], posscaleB, tempB)

temp <- cbind("class" = Clabel,  teamA[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos")], 
    teamB[c("TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor","TeamBdr","TeamBast", "TeamBto",
    "TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")], cnames, tnames, Tloc)
#temp <- cbind("class" = Clabel, diffVector, cnames, tnames, Tloc)
training <- createDataPartition(y = temp$class,p = 0.8,list = FALSE)
train <- temp[training,]
test <- temp[-training,]

train[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
    "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
    "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")] <-
    scale(train[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
    "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
    "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")])

test[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
    "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
    "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")] <-
    scale(test[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
    "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
    "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
    "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")])    
#train <- cbind(tempA, tempB)
#train <- cbind("class" = Clabel, train)

write.csv(train, file = paste(trainpath, 'trainScaled', year, '.csv', sep = ''), row.names = FALSE)
write.csv(test, file = paste(trainpath, 'testScaled', year, '.csv', sep = ''), row.names = FALSE)





