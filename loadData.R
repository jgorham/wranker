library(plyr)

.loadCollegeFootballData <- function (data.fp) {
  df <- read.csv(data.fp, header=TRUE, as.is=TRUE)
  df
}

.preprocessDataFrameColumns <- function (
    df,
    no.home.teams=FALSE,
    expand.rows=TRUE
  ) {
  df$Visitor.Score <- as.numeric(df$Visitor.Score)
  df$Home.Score <- as.numeric(df$Home.Score)
  df$Date <- as.Date(df$Date, format="%m/%d/%Y")

  preprocessed.df <- adply(df, 1, function (game) {
    team.won <- as.numeric(game$Home.Score > game$Visitor.Score)
    home.team <- ifelse(no.home.teams, NA, game$Home.Team)

    home.row <- data.frame(
      date=game$Date,
      team=game$Home.Team,
      opponent=game$Visitor,
      home.team=home.team,
      won=team.won
    )

    if (!expand.rows) {
      return(home.row)
    }

    away.row <- data.frame(
      date=game$Date,
      team=game$Visitor,
      opponent=game$Home.Team,
      home.team=home.team,
      won=(1-team.won)
    )
    rbind(home.row, away.row)
  }, .expand=FALSE)

  preprocessed.df$team <- as.character(preprocessed.df$team)
  preprocessed.df$opponent <- as.character(preprocessed.df$opponent)

  preprocessed.df[,-1]
}

.getRegularSeasonDataFP <- function (year) {
  file.prefix <- ifelse(year <= 2008, "ncaa", "cfb")
  paste("./cfbdata/", file.prefix, year, "lines.csv", sep="")
}

.getRawRegularSeasonGames <- function (years) {
  season.dfs <- alply(years, 1, function (year) {
    data.fp <- .getRegularSeasonDataFP(year)
    .loadCollegeFootballData(data.fp)
  })

  Reduce(rbind, season.dfs)
}

getRegularSeasonGames <- function (years, expand.rows=TRUE) {
  raw.df <- .getRawRegularSeasonGames(years)
  stacked.df <- .preprocessDataFrameColumns(raw.df, expand.rows=expand.rows)
  stacked.df
}

.getBowlGamesDataFP <- function (year) {
  paste("./cfbdata/bowl", year, "lines.csv", sep="")
}

.getRawBowlGames <- function (years) {
  if (length(years) == 0 || is.na(years)) {
    return(data.frame())
  }
  bowl.dfs <- alply(years, 1, function (year) {
    data.fp <- .getBowlGamesDataFP(year)
    .loadCollegeFootballData(data.fp)
  })

  Reduce(rbind, bowl.dfs)
}

getBowlGames <- function (years, expand.rows=TRUE) {
  raw.df <- .getRawBowlGames(years)
  stacked.df <- .preprocessDataFrameColumns(
    raw.df,
    no.home.teams=TRUE,
    expand.rows=expand.rows
  )
  stacked.df
}

getAllGames <- function(years, exclude.last.bowl=FALSE, expand.rows=TRUE) {
  bowl.years <- ifelse(exclude.last.bowl, head(years, -1), years)
  raw.reg.df <- .getRawRegularSeasonGames(years)
  reg.df <- .preprocessDataFrameColumns(raw.reg.df, expand.rows=expand.rows)

  if (length(bowl.years) == 0 || is.na(bowl.years)) {
    bowl.df <- data.frame()
  } else {
    raw.bowl.df <- .getRawBowlGames(bowl.years)
    bowl.df <- .preprocessDataFrameColumns(
      raw.bowl.df,
      no.home.teams=TRUE,
      expand.rows=expand.rows
    )
  }

  stacked.df <- rbind(reg.df, bowl.df)
  stacked.df
}
