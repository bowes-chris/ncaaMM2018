one.hot.teams <- function(a, b, idlist, nlist) {
    tnames <- a$teamID[0]
    for (i in 1:length(idlist)) {
        temp <- ifelse((a$teamID == idlist[i]) | (b$teamID == idlist[i]), 1, 0)
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

one.hot.loc <- function(a, w) {
    loc <- sort(unique(factor(Wstats$loc)))
    lnames <- w[0]
    for (i in 1:length(lnames)) {
        temp <- ifelse(a$loc == loc[i], 1, 0)
        #tempB <- ifelse(teamB$loc == locs[i],1,0)
        lnames <- cbind(lnames, temp)
        #TlocB <- cbind(TlocB, tempB)
    }
    colnames(lnames) <- c("away", "home", "neutral")
    lnames
}

teamPos <- function(a) {
    #Basic Possession Formula=0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]
    0.96 * (a['fga'] + a['to'] + (0.44 * a['fta']) - a['or']) 
}
teamOpPos <- function(a) {
    #Basic Possession Formula=0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]
    0.96 * (a['opfga'] + a['opto'] + (0.44 * a['opfta']) - a['opor'])
}
teamOeff <- function(a) {
    #Offensive Efficiency Formula=100*(Points Scored)/(Possessions)
    (a['pts']) / teamPos(a) #* 100
}
teamDeff <- function(a) {
    (a['ptsa']) / teamOpPos(a) # * 100
}
teamPace <- function(a) {
    #Pace Calculation Formula = [240 / (Team Minutes)] * (Possessionteam + Possessionopponent) / 2    
    (200 * ((teamPos(a) + teamOpPos(a)) / (2 * ((((a['wins'] + a['loss']) * 40) + (a['ot'] * 5)) / 5)))) / 100
}
teamGames <- function(a) {
    a['wins'] + a['loss']
}
trueSA <- function(a) {
    a['fga'] + (0.44 * a['fta'])
}
trueSPer <- function(a) {
    a['pts'] / (2 * trueSA(a))
}
threePAr <- function(a) {
    a['fga3'] / a['fga']
}
ftRate <- function(a) {
    a['fta'] / a['fga']
}
minutesPlayed <- function(a) {
    ((a['wins'] + a['loss']) * 40) + (a['ot'] * 5)
}
teamDRPer <- function(a) {
    # Defensive Rebounding Percentage Formula=(Team Defensive Rebounds)/[(Team Defensive Rebounds)+Opponent's Offensive Rebounds)]
    a['dr'] / (a['dr']+a['opor'])
}
teamORPer <- function(a) {
    # Offensive Rebounding Percentage Formula = (Offensive Rebounds) / [(Offensive Rebounds) + (Opponent �s Defensive Rebounds)]
    a['or'] / (a['or'] + a['opdr'])
}
teamAstPer <- function(a) {
    # Assist Ratio Formula=(Assists)*100)/ [(Field Goal Attempts)+(Free Throw Attempts*0.44)+(Assists)+(Turnovers)]
    (a['ast']) / (a['fga'] + (0.44 * a['fta']) + a['ast'] + a['to']) # * 100
    #100 * AST / (((MP / (Tm MP / 5)) * Tm FG) - FG)
    #100 * (a['ast'])  / (((minutesPlayed(a) / (minutesPlayed(a) /5)) * a['fgm']) - a['fgm'])
}
teamTORat <- function(a) {
    # Turnover Ratio Formula=(Turnovers)*100)/ [(Field Goal Attempts)+(Free Throw Attempts*0.44)+(Assists)+(Turnovers)]
    (a['to'] / (a['fga'] + (0.44 * a['fta']) + a['ast'] + a['to'])) # * 100
}
tRePer <- function(a) {
    # Total Rebound Percentage  100 * (TRB * (Tm MP / 5)) / (MP * (Tm TRB + Opp TRB)).
    ((a['or'] + a['dr']) * (minutesPlayed(a) / 5)) / (200 * ((a['or'] + a['dr']) + (a['opor'] + a['opdr']))) # * 100
}
effFG <- function(a) {
    #Effective Field Goal Percentage; the formula is(FG + 0.5 * 3 P) / FGA.
    (a['fgm'] + (0.5 * a['fgm3'])) / a['fga']
}
getOppList <- function(tmid, rs) {
    tmp <- as.numeric(tmid[0])
    for (i in 1:nrow(rs)) {
        if (rs$WTeamID[i] == tmid) {
            tmp[length(tmp) + 1] <- rs$LTeamID[i]
        }
        else if (rs$LTeamID[i] == tmid) {
            tmp[length(tmp) + 1] <- rs$WTeamID[i]
        }
    }
    tmp
}
getOppWinP <- function(opl) {

}
rpi <- function(ts, rs) {
    #Rating Percentage Index(RPI) Formula = .25 * (Team �s Winning Percentage) 
    #+ .50 * (Opponents � Average Winning Percentage) 
    #+ 0.25 * (Opponents � Opponents � Average Winning Percentage)
    trpi <- ts$winp[0]
    for (i in 1:nrow(ts)) {
        print(paste('Calculating RPI for ', ts$TeamName[i], sep = ''))
        twp <- ts$winp[i]
        opwin <- 0
        topl <- getOppList(ts$TeamID[i], rs)
        for (j in 1:length(topl)) {
            opwin <- opwin + ts$winp[ts$TeamID == topl[j]]
        }
        opopwinp <- 0
        opopnum <- 0
        for (k in 1:length(topl)) {
            optopl <- getOppList(topl[k], rs)
            opopnum <- opopnum + length(optopl)
            for (m in 1:length(optopl)) {
                opopwinp <- opopwinp + ts$winp[ts$TeamID == optopl[m]]
            }
        }
        trpi[i] <- (.25 * twp) + (.50 * (opwin / length(topl))) + (.25 * (opopwinp / opopnum))
        print(paste('RPI =', trpi[i], sep = ''))
    }
    trpi
}