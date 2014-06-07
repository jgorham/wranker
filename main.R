# This is the main script that will model the games

source('loadData.R')
source('eloplusplus.R')
source('wranker.R')
source('validateFunctions.R')

library(plyr)
library(parallel)
library(ggplot2)
library(reshape)

REG.CONSTANT <- 4
STARTING.YEARS <- 2005:2012
NUM.WORKERS <- 16

makePredictionsFromEloPlusPlus <- function (
    starting.years,
    num.years=2
  ) {

  pred.errors <- mclapply(starting.years, function (starting.year) {
    final.year <- starting.year + num.years - 1
    years <- starting.year:final.year
    seed.no <- seed.hash(starting.year)
    training.data <- getAllGames(years, exclude.last.bowl=TRUE)
    test.data <- getBowlGames(tail(years, 1), expand.rows=FALSE)

    eloplusplus <- getEloPlusPlusRankings(training.data, REG.CONSTANT, max.iter=50)
    c(
      "home.adv"=eloplusplus$home.adv,
      "bowl.year"=final.year,
      "prediction.error"=computePredictionError(test.data, eloplusplus$rankings, eloplusplus$home.adv),
      "mean.sq.error"=computeMSE(test.data, eloplusplus$rankings, eloplusplus$home.adv),
      "num.bowls"=nrow(test.data)
    )
  }, mc.cores = NUM.WORKERS)

  as.data.frame(Reduce(rbind, pred.errors))
}

savePosteriorDrawsFromWranker <- function (
    starting.years,
    num.samples=100,
    num.years=2
  ) {
  for (starting.year in starting.years) {
    final.year <- starting.year + num.years - 1
    years <- starting.year:final.year
    training.data <- getAllGames(years, exclude.last.bowl=TRUE)

    posterior.draws <- mclapply(1:num.samples, function (ii) {
      print(paste("Trial number:", ii))
      seed.no <- seed.hash(starting.year, ii)
      getWrankerRankings(training.data, seed.no=seed.no, max.iter=250)
    }, mc.cores = NUM.WORKERS)

    file.name <- paste("./saved_data/wranker_draws_", starting.year, "_3.Rdata", sep="")
    save('posterior.draws', file=file.name)

    print(paste("Saved draws for year", starting.year))
  }
}

makePredictionsFromWranker <- function (
    starting.years,
    num.years=2
  ) {

  pred.errors <- adply(starting.years, 1, function (starting.year) {
    final.year <- starting.year + num.years - 1
    years <- starting.year:final.year
    test.data <- getBowlGames(tail(years, 1), expand.rows=FALSE)
    file.name <- paste("./saved_data/wranker_draws_", starting.year, ".Rdata", sep="")

    load(file.name)  # posterior.draws now defined
    estimated.probs <- laply(posterior.draws, function (wranker.params) {
      predictProbs(test.data, wranker.params$rankings, wranker.params$home.adv)
    })

    average.pred <- colMeans(estimated.probs)
    average.error <- mean(test.data$won != round(average.pred))
    mean.sq.error <- mean((test.data$won - average.pred)^2)
    average.home.adv <- mean(unlist(lapply(posterior.draws, '[[', 'home.adv')))
    c(
      "home.adv"=average.home.adv,
      "bowl.year"=final.year,
      "prediction.error"=average.error,
      "mean.sq.error"=mean.sq.error,
      "num.bowls"=nrow(test.data)
    )
  })[, -1]

  pred.errors
}

# Error rates
elopp.df <- makePredictionsFromEloPlusPlus(STARTING.YEARS)
savePosteriorDrawsFromWranker(STARTING.YEARS)
wranker.df <- makePredictionsFromWranker(STARTING.YEARS)

all.df <- rbind(
  cbind(elopp.df, model="elo++"),
  cbind(wranker.df, model="wranker")
)
error.df <- melt(
  all.df[, c('bowl.year', 'prediction.error', 'mean.sq.error', 'model')],
  id=c('bowl.year', 'model')
)

ddply(all.df, .(model), function (df) {
  c(ave.pred.error=sum(df$prediction.error * df$num.bowls) / sum(df$num.bowls))
})

png("./figs/eloPlusPlus_vs_wranker_error.png", width=400, height=300)
ggplot(data=error.df, aes(x=bowl.year, y=value)) +
  geom_point(aes(color=model)) +
  geom_line(aes(color=model)) +
  facet_wrap(~ variable) +
  labs(x="Bowl Year",
       y="Error",
       title="Elo++ vs. Wranker") +
  theme_bw()
dev.off()

# Ranking comparison (2013)
years <- 2012:2013

X <- getAllGames(years, exclude.last.bowl=TRUE)
test.data <- getBowlGames(tail(years, 1), expand.rows=FALSE)

eloplusplus <- getEloPlusPlusRankings(X, REG.CONSTANT, max.iter=50)
wranker.file.name <- paste("./saved_data/wranker_draws_", head(years, 1), ".Rdata", sep="")
load(wranker.file.name)  # defines posterior draws

wranker.num.clusters <- ldply(posterior.draws, function (params) {
  length(params$cluster.means)
})

png("./figs/wranker_num_clusters_2013.png", width=300, height=300)
ggplot(wranker.num.clusters, aes(x=V1)) +
    geom_histogram(color=I("blue"), fill=I("white"), size=I(1)) +
    labs(x="Number of clusters", y="Frequency",
         title="# Clusters For 2012-2013 Data") +
    scale_x_discrete() +
    theme_bw()
dev.off()

wranker.vars <- ldply(posterior.draws, function (params) {
  c(
    sigma2=(1/params$inv.sigma2),
    tau2=(1/params$inv.tau2)
  )
})

png("./figs/wranker_variances_2013.png", width=350, height=300)
qplot(data=wranker.vars, x=sigma2, y=tau2, geom="point",
      xlab=paste0(expression(sigma), "2"),
      ylab=paste0(expression(tau), "2"),
      main="Posterior Samples of Variance Parameters") +
    theme_bw()
dev.off()

wranker.rankings <- laply(posterior.draws, function (params) {
  unlist(params$rankings)
})
wranker.ranking.means <- sort(colMeans(wranker.rankings))
wranker.rankings.df <- melt(wranker.rankings)[, -1]
colnames(wranker.rankings.df) <- c('team', 'ranking')
wranker.rankings.df$team <- factor(wranker.rankings.df$team,
                                   levels=names(wranker.ranking.means))

elo.rankings <- unlist(eloplusplus$rankings)
elo.rankings.df <- data.frame(
  team=factor(names(elo.rankings), levels=names(wranker.ranking.means)),
  ranking=as.numeric(elo.rankings)
)

rankings2013.df <- rbind(
  cbind(wranker.rankings.df, model="wranker"),
  cbind(elo.rankings.df, model="elo++")
)

png("./figs/rankings_2013.png", width=500, height=500)
ggplot(data=rankings2013.df, aes(x=team, y=ranking)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult=1) +
  stat_summary(fun.y = mean, geom = "point") +
  facet_wrap(~ model, ncol=1, scales="free_y") +
  labs(x="Team",
       y="Ranking",
       title="Rankings for 2012-2013 data") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()
