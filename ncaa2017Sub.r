datapath <- './data/'
logpath <- './logs/'
trainpath <- './modeldata/'
outpath <- './output/'
year <- 2014

submission  <- read.csv(paste(datapath, 'SampleSubmissionStage1.csv', sep=''))
pred2014 <- read.csv(paste(outpath, 'pred2014.csv', sep=''), header=FALSE)
pred2015 <- read.csv(paste(outpath, 'pred2015.csv', sep=''), header=FALSE)
pred2016 <- read.csv(paste(outpath, 'pred2016.csv', sep=''), header=FALSE)
pred2017 <- read.csv(paste(outpath, 'pred2017.csv', sep=''), header=FALSE)

subpred <- pred2014[0]
subpred <- rbind(pred2014, pred2015)
subpred <- rbind(subpred, pred2016)
subpred <- rbind(subpred, pred2017)

submission$Pred <- subpred$V1

write.csv(submission, file = paste(outpath, 'stage1submission.csv', sep = ''), row.names = FALSE)