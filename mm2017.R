teams <- read.csv('Teams.csv')
seasons <- read.csv('Seasons.csv')
rsdr <- read.csv('RegularSeasonDetailedResults.csv')
rscr <- read.csv('RegularSeasonCompactResults.csv')
tdr <- read.csv('TourneyDetailedResults.csv')
tcr <- read.csv('TourneyCompactResults.csv')
Tseeds <- read.csv('TourneySeeds.csv')
Tslots <- read.csv('TourneySlots.csv')

rsdr15 <- rsdr[rsdr$Season == 2015,]
rscr15 <- rscr[rscr$Season == 2015,]

aggstat15 <- teams

#rsdr15$postot <- rsdr15$fga + (.475 * rsdr15$fta) - rsdr15$r


for (i in 1:nrow(teams)) {
  
  tid <- aggstat15$Team_Id[i]
  
  n <- sum(c(rsdr15$Wteam == tid, rsdr15$Lteam == tid))
  
  #season wins
  aggstat15$wins[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wteam == tid)
  aggstat15$loss[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Lteam == tid)
  
  #season fgm, fga, and fgp
  aggstat15$fga[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wfga[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lfga[which(rsdr15$Lteam == tid)])
  aggstat15$fgm[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wfgm[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lfgm[which(rsdr15$Lteam == tid)])
  
  
  #season 3fgm, 3fga, 3fgp
  aggstat15$fga3[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wfga3[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lfga3[which(rsdr15$Lteam == tid)])
  aggstat15$fgm3[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wfgm3[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lfgm3[which(rsdr15$Lteam == tid)])

  
  #season ftm, fta, ftp
  aggstat15$fta[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wfta[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lfta[which(rsdr15$Lteam == tid)])
  aggstat15$ftm[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wftm[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lftm[which(rsdr15$Lteam == tid)])

  
  #season Off, Def, Tot reb per game
  aggstat15$Oreb[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wor[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lor[which(rsdr15$Lteam == tid)])
  aggstat15$Dreb[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wdr[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Ldr[which(rsdr15$Lteam == tid)])

  #season assist total
  aggstat15$ast[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wast[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Last[which(rsdr15$Lteam == tid)])
  #season turn over total
  aggstat15$to[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wto[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lto[which(rsdr15$Lteam == tid)])
  #season steal total
  aggstat15$stl[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wstl[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lstl[which(rsdr15$Lteam == tid)])
  #season blocks total
  aggstat15$blk[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wblk[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lblk[which(rsdr15$Lteam == tid)])
  #season personal fouls total
  aggstat15$pf[which(aggstat15$Team_Id == tid)] <- sum(rsdr15$Wpf[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lpf[which(rsdr15$Lteam == tid)])
  
  #season averages
  aggstat15$winp[which(aggstat15$Team_Id == tid)] <- (aggstat15$wins[which(aggstat15$Team_Id == tid)] / (aggstat15$wins[which(aggstat15$Team_Id == tid)]+aggstat15$loss[which(aggstat15$Team_Id == tid)]))*100
  aggstat15$ppg[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Wscore[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Lscore[which(rsdr15$Lteam == tid)]))/n
  aggstat15$fgp[which(aggstat15$Team_Id == tid)] <- (aggstat15$fgm[which(aggstat15$Team_Id == tid)]/ aggstat15$fga[which(aggstat15$Team_Id == tid)]) * 100
  aggstat15$fgp3[which(aggstat15$Team_Id == tid)] <- (aggstat15$fgm3[which(aggstat15$Team_Id == tid)] / aggstat15$fga3[which(aggstat15$Team_Id == tid)]) * 100
  aggstat15$ftp[which(aggstat15$Team_Id == tid)] <- (aggstat15$ftm[which(aggstat15$Team_Id == tid)] / aggstat15$fta[which(aggstat15$Team_Id == tid)]) * 100
  #aggstat15$rebpg[which(aggstat15$Team_Id == tid)] <- (aggstat15$Oreb[which(aggstat15$Team_Id == tid)] + aggstat15$Dreb[which(aggstat15$Team_Id == tid)])/n
  aggstat15$orebpg[which(aggstat15$Team_Id == tid)] <- aggstat15$Oreb[which(aggstat15$Team_Id == tid)] / n 
  aggstat15$drebpg[which(aggstat15$Team_Id == tid)] <- aggstat15$Dreb[which(aggstat15$Team_Id == tid)] / n
  aggstat15$astpg[which(aggstat15$Team_Id == tid)] <- aggstat15$ast[which(aggstat15$Team_Id == tid)] / n
  aggstat15$topg[which(aggstat15$Team_Id == tid)] <- aggstat15$to[which(aggstat15$Team_Id == tid)] / n
  aggstat15$stlpg[which(aggstat15$Team_Id == tid)] <- aggstat15$stl[which(aggstat15$Team_Id == tid)] / n
  aggstat15$blkpg[which(aggstat15$Team_Id == tid)] <- aggstat15$blk[which(aggstat15$Team_Id == tid)] / n
  aggstat15$pfpg[which(aggstat15$Team_Id == tid)] <- aggstat15$pf[which(aggstat15$Team_Id == tid)] / n
  
  #opponent averages
  aggstat15$oppg[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lscore[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wscore[which(rsdr15$Lteam == tid)]))/ n
  aggstat15$oppfgp[which(aggstat15$Team_Id == tid)] <- ((sum(rsdr15$Lfgm[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wfgm[which(rsdr15$Lteam == tid)])) /
                                                           (sum(rsdr15$Lfga[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wfga[which(rsdr15$Lteam == tid)]))) * 100
  aggstat15$oppfgp3[which(aggstat15$Team_Id == tid)] <- ((sum(rsdr15$Lfgm3[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wfgm3[which(rsdr15$Lteam == tid)])) /
                                                           (sum(rsdr15$Lfga3[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wfga3[which(rsdr15$Lteam == tid)]))) * 100
  aggstat15$oppftp[which(aggstat15$Team_Id == tid)] <- ((sum(rsdr15$Lftm[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wftm[which(rsdr15$Lteam == tid)])) /
                                                           (sum(rsdr15$Lfta[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wfta[which(rsdr15$Lteam == tid)]))) * 100
  aggstat15$opporebpg[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lor[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wor[which(rsdr15$Lteam == tid)])) / n
  aggstat15$oppdrebpg[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Ldr[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wdr[which(rsdr15$Lteam == tid)])) / n
  aggstat15$oppast[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Last[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wast[which(rsdr15$Lteam == tid)])) / n
  aggstat15$oppto[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lto[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wto[which(rsdr15$Lteam == tid)])) / n
  aggstat15$oppstl[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lstl[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wstl[which(rsdr15$Lteam == tid)])) / n
  aggstat15$oppblk[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lblk[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wblk[which(rsdr15$Lteam == tid)])) / n
  aggstat15$opppf[which(aggstat15$Team_Id == tid)] <- (sum(rsdr15$Lpf[which(rsdr15$Wteam == tid)]) + sum(rsdr15$Wpf[which(rsdr15$Lteam == tid)])) / n
}

aggstat15 <- na.omit(aggstat15)

train <- aggstat15[which(aggstat15$Team_Id == 0),18:40]

getDiffVector <- function(tid1, tid2) {
  t1 <- as.vector(aggstat15[which(aggstat15$Team_Id == tid1),18:40])
  t2 <- as.vector(aggstat15[which(aggstat15$Team_Id == tid2),18:40])
  
  return (t1-t2)  
}

for (i in 1:nrow(aggstat15)) {
  tid <- aggstat15$Team_Id[i]
  
  #team as winner
  opps <- rscr15$Lteam[which(rsdr15$Wteam == tid)]
  if (length(opps) >0) {
    for (j in 1:length(opps)) {
      gdiff <- getDiffVector(tid, opps[j])
      train <- rbind(train, cbind("team1" = tid, "team2" = opps[j], gdiff, "winner" = 1))
    }
  }
  
  #team as loser
  opps <- rscr15$Wteam[which(rsdr15$Lteam == tid)]
  if (length(opps) >0) {
    for (k in 1:length(opps)) {
      gdiff <- getDiffVector(tid, opps[k])
      train <- rbind(train, cbind("team1" = tid, "team2" = opps[k], gdiff, "winner" = 0))
    }
  }
  
}

mm.model <- glm(winner ~ winp + ppg + fgp + fgp3 +ftp + orebpg + drebpg + astpg + topg + stlpg + blkpg + pfpg
                + oppg + oppfgp + oppfgp3 + oppftp + opporebpg + oppdrebpg + oppast + oppto + oppstl + oppblk + opppf, 
                family = "binomial", data = train)


# testdat <- getDiffVector(1320, 1461)
# testdat <- getDiffVector(1301, 1261)
# testdat <- getDiffVector(1437, 1248)
# testdat <- getDiffVector(1279, 1140)
# testdat <- getDiffVector(1214, 1264)


test.games <- aggstat15[which(aggstat15$Team_Id == 0),18:40]

tournGames <- data.frame("team1"=tcr$Wteam[which(tcr$Season == 2015)], "team2"=tcr$Lteam[which(tcr$Season == 2015)])
for(i in 1:nrow(tournGames)){
  gdiff <- getDiffVector(tournGames$team1[i], tournGames$team2[i])
  test.games <- rbind(test.games, cbind("team1"=tournGames$team1[i], "team2"=tournGames$team2[i], gdiff))
}


pred <- predict.glm(mm.model, newdata = test.games[,2:25], type = "response")

