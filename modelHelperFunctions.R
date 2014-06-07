# This is a set of helper functions for fitting the models

logit <- function (x) {1 / (1 + exp(-x))}

normalize <- function (x) {x / sum(x)}

getTimeDecayingWeights <- function (
    dates,
    min.date,
    max.date,
    EXPONENT=5,
    PADDING=7
  ) {
  days.since.min <- as.numeric(dates - min.date)
  time.range <- as.numeric(max.date - min.date)

  ((PADDING + days.since.min) / (PADDING + time.range))^EXPONENT
}

getLearningRate <- function (curr.iter, max.iter=100) {
  # fancier version of 1 / curr.iter
  MAGIC.EXPONENT <- 0.602
  ((1 + 0.1 * max.iter) / (curr.iter + 0.1 * max.iter))^MAGIC.EXPONENT
}

.normPercentChange <- function (v1, v2) {
  norm(unlist(v1) - unlist(v2), type="2") / norm(unlist(v1), type="2")
}

cluster.counts <- function (params) {
  table(unlist(params$clusters))
}
