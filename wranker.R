# Implements the approach that won the Kaggle comp

source('modelHelperFunctions.R')

NUM.MH.JUMPS <- 10

getWrankerRankings <- function (
    X,
    seed.no=7,
    max.iter=200
  ) {
  set.seed(seed.no)

  Xhome <- subset(X, !is.na(home.team) & team == home.team)
  team.names <- unique(X$team)
  num.teams <- length(team.names)
  team.name.ordering <- sample(team.names)

  params <- list(
    rankings=list(),
    clusters=list(),
    cluster.means=list()
  )
  params$rankings[team.names] <- rnorm(num.teams)
  params$clusters[team.names] <- 1
  params$cluster.means[["1"]] <- 0
  params$inv.sigma2 <- 1
  params$inv.tau2 <- 10
  params$home.adv <- 0
  params$BETA <- 0.5
  params$ALPHA0 <- 1; params$BETA0 <- 1
  params$ALPHA1 <- 5; params$BETA1 <- 50
  params$GAMMA2 <- 0.5

  old.rankings <- params$rankings
  for (curr.iter in 1:max.iter) {
    params$rankings <- .resampleRankings(X, params, team.name.ordering)
    params <- .resampleClusters(params)
    params$cluster.means <- .resampleClusterMeans(params)
    params$inv.sigma2 <- .resampleSigma2Inv(params)
    params$inv.tau2 <- .resampleTau2Inv(params)
    params$home.adv <- .resampleHomeAdvantage(Xhome, params)

    print(
      paste(curr.iter, .normPercentChange(old.rankings, params$rankings), sep=":")
    )

    old.rankings <- params$rankings
  }

  params
}

#######################
# rankings resampling #
#######################
.resampleRankings <- function (
    X,
    params,
    team.name.ordering
  ) {
  date.range <- range(X$date)

  for (team.name in team.name.ordering) {
    Xplayed <- subset(X, team == team.name)

    for (iter in 1:NUM.MH.JUMPS) {
      params$rankings[[team.name]] <- .resampleRanking(
        team.name,
        params,
        Xplayed,
        date.range
      )
    }
  }

  params$rankings
}

.resampleRanking <- function (
    team.name,
    params,
    Xplayed,
    date.range
  ) {
  curr.ranking <- params$rankings[[team.name]]
  next.ranking <- .rankingProposalDistribution(curr.ranking)

  curr.log.density <- .rankingLogDensity(
    curr.ranking,
    team.name,
    params,
    Xplayed,
    date.range
  )
  next.log.density <- .rankingLogDensity(
    next.ranking,
    team.name,
    params,
    Xplayed,
    date.range
  )

  density.diff <- next.log.density - curr.log.density

  ifelse(
    runif(1) < exp(density.diff),
    next.ranking,
    curr.ranking
  )
}

.rankingProposalDistribution <- function (ranking) {
  ranking + rnorm(1, mean=0, sd=0.2)
}

.rankingLogDensity <- function (
    r,
    team.name,
    params,
    Xplayed,
    date.range
  ) {
  min.date <- date.range[1]; max.date <- date.range[2]
  inv.tau2 <- params$inv.tau2
  rankings <- params$rankings
  team.cluster <- params$clusters[[team.name]]
  team.cluster.mean <- params$cluster.means[[team.cluster]]

  game.weights <- getTimeDecayingWeights(
    Xplayed$date,
    min.date,
    max.date
  )

  ranking.diffs <- r - unlist(rankings[Xplayed$opponent])
  home.field <- ifelse(
    is.na(Xplayed$home.team),
    0,
    ifelse(Xplayed$home.team == team.name, 1, -1)
  ) * params$home.adv

  o.hats <- logit(ranking.diffs + home.field)
  likelihood.component <- -5 * sum(game.weights * (o.hats - Xplayed$won)^2)
  prior.component <- -0.5 * inv.tau2 * (r - team.cluster.mean)^2
  likelihood.component + prior.component
}

#######################
# cluster assignments #
#######################
.resampleClusters <- function (params) {
  for (team.name in names(params$rankings)) {
    params <- .resampleCluster(
      team.name,
      params
    )
  }
  params
}

.resampleCluster <- function (
    team.name,
    params
  ) {
  all.teams <- names(params$clusters)
  team.ranking <- params$rankings[[team.name]]

  cluster.assignments <- unlist(
    params$clusters[all.teams != team.name]
  )
  cluster.cnts <- table(cluster.assignments)
  cluster.ids <- names(cluster.cnts)

  cluster.likelihoods <- dnorm(
    team.ranking,
    unlist(params$cluster.means[cluster.ids]),
    params$inv.tau2^(-1/2),
    log=TRUE
  )
  new.cluster.likelihood <- dnorm(
    team.ranking,
    0,
    sqrt(params$inv.tau2^(-1) + params$inv.sigma2^(-1)),
    log=TRUE
  )

  all.priors <- log(c(cluster.cnts, params$BETA0))
  all.likelihoods <- c(cluster.likelihoods, new.cluster.likelihood)
  all.probs <- all.priors + all.likelihoods
  all.probs <- exp(all.probs - max(all.probs))

  team.cluster.ix <- which(rmultinom(1, 1, prob=all.probs) == 1)
  if (team.cluster.ix > length(cluster.ids)) {
    max.num.id <- max(as.numeric(cluster.ids))
    team.cluster.id <- as.character(max.num.id + 1)
    params$cluster.means[team.cluster.id] <-
      rnorm(1, 0, sqrt(params$inv.tau2^(-1) + params$inv.sigma2^(-1)))
  } else {
    team.cluster.id <- cluster.ids[team.cluster.ix]
  }
  params$clusters[team.name] <- team.cluster.id
  params
}

#################
# cluster means #
#################
.resampleClusterMeans <- function (params) {
  cluster.assignments <- unlist(params$clusters)
  cluster.ids <- unique(cluster.assignments)

  cluster.means <- sapply(cluster.ids, function (c.id) {
    cluster.teams <- names(cluster.assignments[cluster.assignments == c.id])
    n.cluster <- length(cluster.teams)
    cluster.avg <- mean(unlist(params$rankings[cluster.teams]))

    posterior.mean <- (n.cluster * params$inv.tau2 * cluster.avg) /
        (n.cluster * params$inv.tau2 + params$inv.sigma2)
    posterior.var <- 1 / (n.cluster * params$inv.tau2 + params$inv.sigma2)
    rnorm(1, mean=posterior.mean, sd=sqrt(posterior.var))
  })

  as.list(cluster.means)
}

###############################
# variances (sigma2 and tau2) #
###############################
.resampleSigma2Inv <- function (params) {
  cluster.means <- unlist(params$cluster.means)
  num.clusters <- length(cluster.means)
  posterior.shape <- params$ALPHA0 + num.clusters / 2
  posterior.scale <- params$BETA0 + sum(cluster.means^2) / 2
  1 / rgamma(1, posterior.shape, posterior.scale)
}

.resampleTau2Inv <- function (params) {
  team.names <- names(params$rankings)
  ranking.dev <- sapply(team.names, function (team.name) {
    r <- params$rankings[[team.name]]
    cluster.id <- params$clusters[[team.name]]
    r - params$cluster.means[[cluster.id]]
  })
  n <- length(team.names)

  posterior.shape <- params$ALPHA1 + n / 2
  posterior.scale <- params$BETA1 + sum(ranking.dev^2) / 2
  1 / rgamma(1, posterior.shape, posterior.scale)
}

###########################
# resample home field adv #
###########################
.resampleHomeAdvantage <- function (
    Xhome,
    params
  ) {
  min.date <- min(Xhome$date)
  max.date <- max(Xhome$date)
  curr.home.adv <- params$home.adv
  game.weights <- getTimeDecayingWeights(
    Xhome$date,
    min.date,
    max.date
  )
  curr.log.density <- .homeAdvLogDensity(
    params,
    Xhome,
    game.weights,
    curr.home.adv
  )

  for (iter in 1:NUM.MH.JUMPS) {
    next.home.adv <- .homeAdvProposalDistribution(curr.home.adv)
    next.log.density <- .homeAdvLogDensity(
      params,
      Xhome,
      game.weights,
      curr.home.adv
    )
    density.diff <- next.log.density - curr.log.density
    move.to.next <- runif(1) < exp(density.diff)
    if (move.to.next) {
      curr.home.adv <- next.home.adv
      curr.log.density <- next.log.density
    }
  }

  curr.home.adv
}

.homeAdvProposalDistribution <- function (home.adv) {
  home.adv + rnorm(1, mean=0, sd=0.02)
}

.homeAdvLogDensity <- function (
    params,
    Xhome,
    game.weights,
    home.adv
  ) {
  rankings <- params$rankings
  r.home <- unlist(rankings[Xhome$team])
  r.away <- unlist(rankings[Xhome$opponent])
  o.hats <- logit(r.home - r.away + home.adv)

  likelihood.component <- -5 * sum(game.weights * (o.hats - Xhome$won)^2)
  prior.component <- -1 * home.adv^2 / (2 * params$GAMMA2)
  likelihood.component + prior.component
}
