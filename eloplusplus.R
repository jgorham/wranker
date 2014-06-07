# Implements the approach that won the EloPlusPlus comp

source('modelHelperFunctions.R')

getEloPlusPlusRankings <- function (
    X,
    reg.constant,
    home.adv=NA,
    seed.no=7,
    max.iter=50
  ) {
  set.seed(seed.no)

  Xdedup <- subset(X, team < opponent)
  team.names <- unique(X$team)
  num.teams <- length(team.names)
  rankings <- list()
  rankings[team.names] <- 0
  min.date <- min(Xdedup$date)
  max.date <- max(Xdedup$date)
  fix.home.adv <- !is.na(home.adv)
  if (!fix.home.adv) {
    home.adv <- 0
  }

  for (curr.iter in 1:max.iter) {
    old.rankings <- rankings
    game.ordering <- sample(1:nrow(Xdedup))
    team.reg.targets <- .getAverageOpponentStrength(X, rankings)

    for (game.ix in game.ordering) {
      game <- Xdedup[game.ix, ]

      team.data <- list(
        name=game$team,
        reg.target=team.reg.targets[game$team],
        ranking=as.numeric(rankings[game$team]),
        num.opponents=sum(X$team == game$team)
      )
      opponent.data <- list(
        name=game$opponent,
        reg.target=team.reg.targets[game$opponent],
        ranking=as.numeric(rankings[game$opponent]),
        num.opponents=sum(X$team == game$opponent)
      )
      game.weight <- getTimeDecayingWeights(
        game$date,
        min.date,
        max.date
      )
      new.rankings <- .updateTeamEloPlusPlusRanking(
        curr.iter,
        game.weight,
        game,
        team.data,
        opponent.data,
        reg.constant,
        home.adv,
        max.iter
      )

      rankings[game$team] <- new.rankings$team
      rankings[game$opponent] <- new.rankings$opponent
    }

    if (!fix.home.adv) {
      home.adv <- .updateHomeFieldAdvantage(
        home.adv,
        curr.iter,
        Xdedup,
        rankings,
        max.iter
      )
    }

    print(
      paste(
        paste("[", curr.iter, "|ranking.delta]", sep=""),
        .normPercentChange(old.rankings, rankings),
        sep=":"
      )
    )
    print(
      paste(
        paste("[", curr.iter, "|home.adv]", sep=""),
        home.adv,
        sep=":"
      )
    )
  }

  list(
    rankings=rankings,
    home.adv=home.adv
  )
}

.updateHomeFieldAdvantage <- function (
    curr.home.adv,
    curr.iter,
    Xdedup,
    rankings,
    max.iter
  ) {
  Xhome <- subset(Xdedup, !is.na(home.team))
  min.date <- min(Xhome$date)
  max.date <- max(Xhome$date)

  eta <- getLearningRate(curr.iter, max.iter=max.iter)
  game.weights <- getTimeDecayingWeights(
    Xhome$date,
    min.date,
    max.date
  )
  ranking.diffs <- unlist(rankings[Xhome$team]) - unlist(rankings[Xhome$opponent])
  o.hats <- logit(ranking.diffs + curr.home.adv)

  grad.home.adv <- 2 * sum(game.weights * (o.hats - Xhome$won) * o.hats * (1 - o.hats))
  new.home.adv <- curr.home.adv - (eta * grad.home.adv) / sum(game.weights)
  new.home.adv
}

.getAverageOpponentStrength <- function(X, rankings) {
  team.names <- names(rankings)
  min.date <- min(X$date)
  max.date <- max(X$date)

  opponent.strength <- sapply(team.names, function (team.name) {
    games.played <- subset(X, team == team.name)
    weights <- getTimeDecayingWeights(
      games.played$date,
      min.date,
      max.date
    )
    norm.weights <- normalize(weights)
    sum(
      unlist(rankings[games.played$opponent]) *
      norm.weights
    )
  })

  names(opponent.strength) <- team.names
  opponent.strength
}

.updateTeamEloPlusPlusRanking <- function (
    curr.iter,
    game.weight,
    game,
    team.data,
    opponent.data,
    reg.constant,
    home.adv,
    max.iter
  ) {

  eta <- getLearningRate(curr.iter, max.iter=max.iter)

  ranking.diff <- team.data$ranking - opponent.data$ranking
  if (!is.na(game$home.team)) {
    team.was.home <- game$home.team == game$team
    ranking.diff <- ranking.diff + ifelse(team.was.home, 1, -1) * home.adv
  }
  o.hat <- logit(ranking.diff)

  game.component <- game.weight * (o.hat - game$won) * o.hat * (1 - o.hat)
  team.reg.component <- reg.constant *
    (team.data$ranking - team.data$reg.target) / team.data$num.opponents
  opponent.reg.component <- reg.constant *
    (opponent.data$ranking - opponent.data$reg.target) / opponent.data$num.opponents

  new.team.ranking <- team.data$ranking - eta * (
    team.reg.component + game.component
  )
  new.opponent.ranking <- opponent.data$ranking - eta * (
    opponent.reg.component - game.component
  )

  list(
    team=new.team.ranking,
    opponent=new.opponent.ranking
  )
}
