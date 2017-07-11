library(httr)
library(dplyr)
library(jsonlite)

team_search_url <- "http://nflarrest.com/api/v1/team/search/"
team_search_url <- modify_url(team_search_url, query=list(term="s"))
response <- GET(team_search_url)
response$status_code
the_content <- content(response, "text", encoding = "UTF-8")
dfTeam <- fromJSON(the_content, simplifyDataFrame =  TRUE)

GetTeamPlayer <- function(team){
  team_player_url <- "http://nflarrest.com/api/v1/team/topPlayers/"
  team_player_url <- paste0(team_player_url, team)
  response <- GET(team_player_url)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

players <- lapply(dfTeam$team_code, function(x){
  df <- GetTeamPlayer(x)
  players <- df$Name
})

names(players) <- NULL

players <- players %>%
  unlist() %>%
  unique()

GetPlayerArrest <- function(player){
  player_url <- 'http://NflArrest.com/api/v1/player/arrests/'
  player <- gsub(" ", "%20", player)
  player_url <- paste0(player_url, player)
  response <- GET(player_url)
  the_content <- content(response, "text", encoding = "UTF-8")
  df <- fromJSON(the_content, simplifyDataFrame =  TRUE)
  df
}

dfAllArrests <- lapply(players, function(x){
  df <- GetPlayerArrest(x)
  df
})

dfAllArrests <- dfAllArrests[!is.na(dfAllArrests)]
dfAllArrests <- do.call(rbind, dfAllArrests)
dfAllArrests <- dfAllArrests %>%
  mutate(Date = as.Date(Date)
         , Team_city = as.factor(Team_city)
         , Encounter = as.factor(Encounter)
         , Position_name = as.factor(Position_name)
         , ArrestSeasonState = as.factor (ArrestSeasonState)
         , Day_of_Week = as.factor(Day_of_Week))

save(file = "./all_arrests.rda", dfAllArrests)

dfAllArrests %>% write.csv("all_arrests.csv", row.names = FALSE)

dfMultiArrestPlayers <- dfAllArrests %>%
  group_by(Name) %>%
  summarise(NumArrests = n()) %>%
  mutate(MultiArrest = (NumArrests > 1))

dfFirstArrest <- dfAllArrests %>%
  select(-arrest_stats_id, Team_logo_id) %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  slice(1) %>%
  inner_join(dfMultiArrestPlayers, by = "Name")

dfFirstArrest %>% write.csv("first_arrest.csv", row.names = FALSE)

dfPropByPosition <- dfFirstArrest %>%
  group_by(Position) %>%
  summarise(MultiArrest = sum(MultiArrest)
            , NumRec = n()) %>%
  mutate(ProbSecondArrest = MultiArrest / NumRec)

dfPropByTeam <- dfFirstArrest %>%
  group_by(Team) %>%
  summarise(MultiArrest = sum(MultiArrest)
            , NumRec = n()) %>%
  mutate(ProbSecondArrest = MultiArrest / NumRec)

#==================================
library(rpart)

num_rec <- nrow(dfFirstArrest)
train <- base::sample(num_rec, size = 0.8 * num_rec)
test <- setdiff(seq.int(num_rec), train)
fit <- rpart(
  formula = MultiArrest ~ Team_city + Encounter + Position_name + ArrestSeasonState + Day_of_Week
  , data = dfFirstArrest[train, ]
)

summary(fit)

dfFirstArrest$Prediction[train] <- predict(fit)

library(randomForest)

fit_2 <- randomForest(
  formula = MultiArrest ~ Team_city + Encounter + Position_name + ArrestSeasonState + Day_of_Week
  , data = dfFirstArrest[train, ]
  , mtry = 3
)

dfFirstArrest$Prediction_2[train] <- predict(fit_2)

sum(dfFirstArrest$Prediction[train] > 0.5) / sum(dfFirstArrest$MultiArrest[train])
sum(dfFirstArrest$Prediction_2[train] > 0.5) / sum(dfFirstArrest$MultiArrest[train])

#================================================
devtools::install_github("ropengov/rsunlight")
library('rsunlight')

cw_timeseries(phrase='united states', start_date='2009-01-01', end_date='2009-04-30', granularity='month')
#================================================

library(httr)

url_root <- "https://musicbrainz.org/ws/2/"
url_artist <- paste0(url_root, "artist")
search_url <- modify_url(url_artist, query=list(query='"Yes"', fmt="json"))

response <- GET(search_url, user_agent("PirateGrunt/0.1 (FanninQED@Yahoo.com)"))
response <- GET(
  search_url
  , user_agent("PirateGrunt/0.1 (FanninQED@Yahoo.com)")
  , authenticate("MyName", "MyPassword"))
req <- response$request
req

response$status_code

the_content <- content(response, "text", encoding = "UTF-8")
dfArtists <- fromJSON(the_content)$artists %>%
  jsonlite::flatten()

yes_id <- dfArtists %>%
  filter(grepl("prog", disambiguation)) %>%
  select(id) %>%
  unlist() %>%
  unname()
