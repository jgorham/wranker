library(digest)

SEED.BASE <- '3f9sj2f'

seed.hash <- function (...) {
  dd <- digest(paste(SEED.BASE, ..., sep=":"))
  hh <- paste("0x", substr(dd, 10, 15), sep="")
  as.integer(hh)
}

predictProbs <- function (Xpred, rankings, home.adv.const) {
  pred.probs <- aaply(Xpred, 1, function (game) {
    team <- game$team; opponent <- game$opponent

    ranking.diff <- rankings[[team]] - rankings[[opponent]]
    if (!is.na(game$home.team)) {
      team.was.home <- game$home.team == team
      ranking.diff <- ranking.diff + ifelse(team.was.home, 1, -1) * home.adv.const
    }

    logit(ranking.diff)
  }, .expand=FALSE)

  pred.probs
}

computeMSE <- function (Xpred, rankings, home.adv.const) {
  pred.probs <- predictProbs(Xpred, rankings, home.adv.const)
  mean((pred.probs - Xpred$won)^2)
}

computePredictionError <- function (Xpred, rankings, home.adv.const) {
  pred.probs <- predictProbs(Xpred, rankings, home.adv.const)
  mean(round(pred.probs) != Xpred$won)
}

