source('loadData.R')
source('eloplusplus.R')
source('validateFunctions.R')

library(parallel)
library(Hmisc)
library(ggplot2)

NUM.WORKERS <- 10

crossValidateModel <- function (
    cv.params,
    starting.years,
    num.years=2
  ) {

  all.pred.errors <- mclapply(starting.years, function(starting.year) {
    pred.errors <- adply(cv.params, 1, function (cv.row) {
      bowl.year <- starting.year + num.years - 1
      years <- starting.year:bowl.year
      seed.no <- seed.hash(starting.year, cv.row$lambda, cv.row$home.adv)
      training.data <- getAllGames(years, exclude.last.bowl=TRUE)
      test.data <- getBowlGames(tail(years, 1), expand.rows=FALSE)

      results <- getEloPlusPlusRankings(training.data, cv.row$lambda,
                                        seed.no=seed.no, home.adv=cv.row$home.adv)
      error <- computePredictionError(test.data, results$rankings, results$home.adv)
      c(
        "reg.constant"=cv.row$lambda,
        "home.adv"=results$home.adv,
        "bowl.year"=bowl.year,
        "prediction.error"=error,
        "fit.home.adv"=is.na(cv.row$home.adv)
      )
    })

    print(paste("Finished year", starting.year))
    pred.errors
  }, mc.cores = NUM.WORKERS)

  Reduce(rbind, all.pred.errors)
}

REG.CONSTANT.CANDIDATES <- c(0, 0.2, 0.5, 1, 2, 4, 8)
HOME.ADV.CANDIDATES <- c(NA, seq(from=0, to=1, by=0.2))
STARTING.YEARS <- 1985:2004
CV.PARAMS <- expand.grid(lambda=REG.CONSTANT.CANDIDATES, home.adv=HOME.ADV.CANDIDATES)

cv.df <- crossValidateModel(
  CV.PARAMS,
  STARTING.YEARS
)

write.table(cv.df, file="./saved_data/cv.tsv", sep="\t", row.names=FALSE, quote=FALSE)
#cv.df <- read.csv("./saved_data/cv.tsv", sep="\t")
cv.df <- transform(cv.df, home.adv.model = ifelse(fit.home.adv, "fit", home.adv))

png("./figs/eloPlusPlus_cv.png", width=500, height=500)
ggplot(data=cv.df, aes(x=reg.constant, y=prediction.error)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", mult=1) +
  stat_summary(fun.y = mean, geom = "point") +
  facet_wrap(~ home.adv.model) +
  labs(x=expression(paste("Regularization Parameter (", lambda, ")")),
       y="Prediction Error",
       title="Cross Validation for Elo++") +
  theme_bw()
dev.off()
