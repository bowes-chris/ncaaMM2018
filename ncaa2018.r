library(caret)
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

Wstats <- rsdrSeason[,c("WTeamID", "WLoc", "WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")]

Lstats <- rsdrSeason[,c("LTeamID", "WLoc", "LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")]

#tid <- sort(unique(teams$TeamID[teams$FirstD1Season <= year & teams$LastD1Season >= year]))
tid <- teams[(teams$FirstD1Season <= year & teams$LastD1Season >= year),]
cid <- conferences[conferences$Season == year,]

Lstats$WLoc[Wstats$WLoc == 'H'] <- 'A'
Lstats$WLoc[Wstats$WLoc == 'A'] <- 'H'

#Wstats$WPos <- Wstats$WFGA + Wstats$WTO + (.475 * Wstats$WFTA) - Wstats$WOR
#Lstats$LPos <- Lstats$LFGA + Lstats$LTO + (.475 * Lstats$LFTA) - Lstats$LOR

#add conference
for (i in 1:length(tid$TeamID)) {
    Wstats$conf[Wstats$WTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
    Lstats$conf[Lstats$LTeamID == tid$TeamID[i]] <- as.character(cid$ConfAbbrev[cid$TeamID == tid$TeamID[i]])
}

Wstats$WTeamID <-factor(Wstats$WTeamID)
Lstats$LTeamID <-factor(Lstats$LTeamID)

Wstats <- cbind(Wstats[c("WTeamID", "WLoc", "conf")],
    Wstats[c("WFGM", "WFGA", "WFGM3", "WFGA3", "WFTM", "WFTA",
    "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF")])#/Wstats$WPos, 
    #Wstats["WPos"])

Lstats <- cbind(Lstats[c("LTeamID", "WLoc", "conf")],
    Lstats[c("LFGM", "LFGA", "LFGM3", "LFGA3", "LFTM", "LFTA",
    "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF")])#/Lstats$LPos, 
    #Lstats["LPos"])

statcolnames <- c("teamID", "loc", "conf", "fgm","fga","fgm3","fga3","ftm","fta",
    "or","dr","ast", "to", "stl", "blk", "pf")#, "pos")

colnames(Wstats) <- statcolnames

colnames(Lstats) <- statcolnames

teamA <- Wstats[0,]
teamB <- Lstats[0,]

WadvStats <- Wstats[c(1, 2, 3)]

WadvStats$pos <- teamPos(Wstats)
WadvStats$oef <- teamOeff(Wstats)
WadvStats$def <- teamDeff(Wstats)
WadvStats$pace <- teamPace(Wstats)
WadvStats$tsper <- trueSPer(Wstats)
WadvStats$threepar <- threePAr(Wstats)
WadvStats$ftrate <- ftRate(Wstats)
WadvStats$drper <- teamDRPer(Wstats)
WadvStats$orper <- teamORPer(Wstats)
WadvStats$astper <- teamAstPer(Wstats)
WadvStats$torate <- teamTORat(Wstats)
WadvStats$trebper <- tRePer(Wstats)
WadvStats$efg <- effFG(Wstats)
WadvStats$oppos <- teamOpPos(Wstats)


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

# one hot loc
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

one.hot.teams <- function(a, b, idlist, nlist) {
    tnames <- a$teamID[0]
    for (i in 1:length(idlist)) {
        temp <- ifelse((a$teamID == idlist[i]) | (b$teamID == idlist[i]) ,1,0)
        tnames <- cbind(tnames, temp)    
    }
    colnames(tnames) <- nlist #tnames    
    tnames
}
one.hot.conf <- function(a, b, clist) {
    cnames <- as.numeric(a$conf[0])
    clist <- sort(unique(clist))
    for (i in 1:length(clist)) {
        temp <- ifelse((a$conf == clist[i]) | (b$conf == clist[i]), 1, 0)
        cnames <- cbind(cnames, temp)    
    }
    colnames(cnames) <- clist
    cnames
}
testhot <- one.hot.teams(teamA, teamB, tid$TeamID, tid$TeamName)
testconf <- one.hot.conf(teamA, teamB, cid$ConfAbbrev)

colnames(teamA) <- paste('TeamA', statcolnames, sep = '')
colnames(teamB) <- paste('TeamB', statcolnames, sep = '')

#diffVector <- teamA[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta",
    #"TeamAor","TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos")] 
    #- teamB[c("TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor","TeamBdr",
    #"TeamBast", "TeamBto", "TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")]

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
#training <- createDataPartition(y = temp$class,p = 0.8,list = FALSE)
#train <- temp[training,]
#test <- temp[-training,]

temp[c("TeamApos", "TeamBpos")] <-
     scale(temp[c("TeamApos", "TeamBpos")])

# train[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
#     "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
#     "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
#     "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")] <-
#     scale(train[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
#     "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
#     "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
#     "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")])

# test[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
#     "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
#     "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
#     "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")] <-
#     scale(test[c("TeamAfgm","TeamAfga","TeamAfgm3","TeamAfga3","TeamAftm","TeamAfta","TeamAor",
#     "TeamAdr","TeamAast", "TeamAto", "TeamAstl", "TeamAblk", "TeamApf", "TeamApos",
#     "TeamBfgm","TeamBfga","TeamBfgm3","TeamBfga3","TeamBftm","TeamBfta","TeamBor",
#     "TeamBdr","TeamBast", "TeamBto","TeamBstl", "TeamBblk", "TeamBpf", "TeamBpos")])    
#train <- cbind(tempA, tempB)
#train <- cbind("class" = Clabel, train)

write.csv(temp, file = paste(trainpath, 'trainScaled', year, '.csv', sep = ''), row.names = FALSE)
#write.csv(test, file = paste(trainpath, 'test', year, '.csv', sep = ''), row.names = FALSE)





