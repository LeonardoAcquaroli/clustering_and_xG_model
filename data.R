library(tibble)
library(gh)
library(gh)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(jsonlite)
library(progress)
library(doParallel)
library(stringi) # to unescape shortName column' values
library(htmltools) # to unescape playersFullName column' values
library(fuzzyjoin) # to approximately match strings
library(worldfootballR)
# process in parallel
#cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# EVENTS
# Get data from local
dir_path = "C:/Users/leoac/OneDrive - Università degli Studi di Milano/Data science/Football/A Dataset for football Cintia-Pappalardo/data/events"
file_list = list.files(dir_path, pattern = "\\.json$", full.names = TRUE)

# Initialize progress bar
pb = progress_bar$new(total = length(file_list), format = "[:bar] :percent ETA: :eta")

# Empty list of dfs
df_list = list()
# Loop through the files
for (file_path in file_list) {
  data = jsonlite::stream_in(file(file_path, "r"))
  df_list = append(df_list, list(data))
  
  # Increment progress bar
  pb$tick()
}
# Concatenate all the data frames into a single data frame
events = do.call(rbind, df_list)

# TAGS
tags = read.csv("https://raw.githubusercontent.com/LeonardoAcquaroli/playerank_R/main/data/tags2name.csv")
events$tags = lapply(events$tags, function(tags_df) {
                                             if ("id" %in% names(tags_df)){
                                               inner_join(x = as.data.frame(tags_df), y = tags[,c("Tag","Description")], by = c("id" = "Tag"))
                                              } else {NA}
                                             })
                                                                      
# TEAMS
teams = stream_in(file("https://raw.githubusercontent.com/LeonardoAcquaroli/playerank_R/main/data/teams.json", "r"))
# PLAYERS
players = stream_in(file("https://raw.githubusercontent.com/LeonardoAcquaroli/playerank_R/main/data/players.json", "r"))
players$shortName = stri_unescape_unicode(players$shortName)
players$lastName = stri_unescape_unicode(players$lastName)
players$firstName= stri_unescape_unicode(players$firstName)
players$foot = lapply(players$foot, function(foot) {ifelse(foot == "both" | foot == "","right",foot)}) # replace 5 "both" and some ""

# COMPETITIONS
competitions = stream_in(file("https://raw.githubusercontent.com/LeonardoAcquaroli/playerank_R/main/data/competitions.json", "r"))
# MATCHES
# matches df needs a for loop because it must load different files
matches_dir_path = "https://api.github.com/repos/LeonardoAcquaroli/playerank_R/contents/data/matches"
matches_files_list = gh("/repos/LeonardoAcquaroli/playerank_R/contents/data/matches")
# Initialize progress bar
pb_matches = progress_bar$new(total = length(matches_files_list), format = "[:bar] :percent ETA: :eta")
matches_dfs_list <- list()
for (file in matches_files_list) {
  matches_file_url <- file$download_url
  matches_data <- jsonlite::fromJSON(matches_file_url)
  # drop the groupName extra column contained in international matches
  if ("groupName" %in% names(matches_data)) {
    matches_data <- matches_data[, !names(matches_data) %in% "groupName"]
  }
  matches_dfs_list <- append(matches_dfs_list, list(matches_data))
  # Increment progress bar
  pb_matches$tick()
}
matches <-  bind_rows(matches_dfs_list)
head(matches[,c("winner", "wyId","label","competitionId")])
# -----------------------------------
# PRE-PROCESSING

## Keep events only from ITA (524), ENG (364), ESP (795)
# ITA_ENG_ESP_matchesID = matches$wyId[matches$competitionId %in% c(524,364,795)]
# events = events |> filter(matchId %in% ITA_ENG_ESP_matchesID)

## Drop goalkeeper
# gk_ids = players$wyId[players$role$name == "Goalkeeper"]
# events = events[!(events$playerId %in% gk_ids),]
# sum(is.na(events$playerId)) = 0 --> all the events are associated to a player unlike StatsBomb

## Drop penalties and calculate their xG

## Create features
# i first need to create the dummies and then add also the goal tag and the distance and angle

## Create start_x, start_y, end_x and end_y columns 
### 100x100 pitch
events$start_x = ifelse(sapply(events$positions, is.data.frame), sapply(events$positions, function(df) df[1, "x"]), NA)
events$start_y = ifelse(sapply(events$positions, is.data.frame), sapply(events$positions, function(df) df[1, "y"]), NA)
events$end_x = ifelse(sapply(events$positions, is.data.frame), sapply(events$positions, function(df) df[2, "x"]), NA)
events$end_y = ifelse(sapply(events$positions, is.data.frame), sapply(events$positions, function(df) df[2, "y"]), NA)

## DUMMY PRE-TIRO
### from smart pass
events$fromSmart_pass = c(ifelse(lag(events$subEventName) == "Smart pass", 1, 0))
### from cross
events$fromCross = c(ifelse(lag(events$subEventName) == "Cross", 1, 0))
### from save
events$fromSave = c(ifelse(lag(events$subEventName) == "Save attempt", 1, 0))
### from dangerous ball lost DBL
checkDescription <- function(x) {df =  do.call(rbind, x)
                                 present <- grepl("Dangerous ball lost", df$Description)
                                 ifelse(any(present), 1, 0)}

tmp_DBL = c(rep(0, 6), sapply(6:nrow(events), function(i) checkDescription(events$tags[(i-6):i])))
tmp_DBL = tmp_DBL[1:length(tmp_DBL)-1] # drop the last 0 cause the others are well ordered
events$fromDBL = tmp_DBL
# CREATE SHOTS DF then continue with the last two dummies
shots = events[events$subEventName == "Shot",]

### head/body vs foot lo faccio direttamente in shots
shots$head_OR_body = lapply(shots$tags, function(x) {ifelse(any(grepl("Head/body", x$Description)),
                                                            1, 0)}) 
shots$head_OR_body = unlist(shots$head_OR_body)
### strong foot lo faccio direttamente in shots
check_strongFoot = function(row) {if (row$head_OR_body == 1){return(0)} # It's a kick shot
                                  else {foot_string = (row$tags$Description[grepl("foot", row$tags$Description)])
                                        foot_string = tolower(strsplit(foot_string, split = " ")[[1]][[1]])
                                        return(foot_string)}
                                  }
shots$shotFoot = apply(shots, 1, function(row) check_strongFoot(row))
shots = shots |>
  inner_join(y = players, by = c("playerId" = "wyId")) |>
  select(c(names(shots), "foot"))

#shots = shots_tmp to restart shots
shots$strongFoot = ifelse(shots$shotFoot == shots$foot, 1, 0)
shots = shots[, !(names(shots) %in% c("shotFoot","foot"))]
             
### goal (1) or no goal (0)
shots$Goal = lapply(shots$tags, function(x) {ifelse(any((101 %in% x$id)),
                                                            1, 0)})
shots$Goal = unlist(shots$Goal)

# distance to goal
#shots$Distance = (100-shots$start_x)^2
distX = 100-shots$start_x
distY = abs(50-shots$start_y)
shots$distance_to_goal = sqrt(distX^2 + distY^2)

#angle
angle <- atan(7.32 * distX / (distX^2 + distY^2 - (7.32/2)^2))
shots$angle <- ifelse(angle > 0, angle*180/pi, (angle + pi)*180/pi)

# matrix
data_columns = c("fromSmart_pass","fromCross","fromSave","fromDBL","head_OR_body","Goal","strongFoot","distance_to_goal","angle") # "start_x","start_y"
shots_matrix = shots[,data_columns]
#shots_matrix = as.matrix(shots[,data_columns])

# End of data processing for Supervised
#---------------------------------

# DELETE CORNER KICKS
## Corner kicks can highly impact the mean position since they are taken also by central midfielders, lateral backs, etc...
## Lateral throw-ins are take into account since they are usually taken by the player in the nearest role
# events = events %>% filter(!(subEventName == "Corner"))


# CENTERS OF PERFORMANCE
# compute mean positions (centre of perfomance) for each playerId
cop = events %>%
  group_by(playerId) %>%
  filter(!(playerId == 0)) %>% # filter one playerId not recognized
  summarize(mean_x = mean(start_x), mean_y = mean(start_y), events_count = n()) %>% # aggregate
  filter(events_count > 10) |> # select only players with more than 10 events (2727 vs 2803)
  select(-events_count) |> # drop events_count
  inner_join(players, by = c("playerId" = "wyId")) %>% # join with player names
  select(c("playerId", "shortName", "mean_x", "mean_y")) # select id, name, cop + additional data to distinct players afterwaards 


# ---------------------------------
# FEATURES FROM FBref
# try to fetch all the data but gives error

#urls <- fb_match_urls(country = "GER", gender = "M", season_end_year = 2018)
urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2016, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")
WC18 <- fb_advanced_match_stats(match_url=urls, stat_type = "summary", team_or_player="player")

# GER and FRA gave problems
# <error/purrr_error_indexed>
#Error in `map()`:
#  ℹ In index: 307.
#Caused by error in `data.frame()`:
#  ! gli argomenti implicano un numero differente di righe: 1, 0

# ESP
# ITA
# ENG
# apply bind
players_features = do.call(rbind, list(ESP,ITA,ENG))

# keep only the features columns + position that we're going to remove after
players_features = players_features[,c(21:length(names(players_features)))] |> select(-Player_Num,-Nation,-Age)
# Create the positions list and then remove that column from players_features
positions_FBref = players_features[,c("Player", "Pos")]
# Select one position for each player
positions_FBref = positions_FBref |> group_by(Player, Pos) |> 
                                  arrange(Player) |>
                                  count() %>%
                                  top_n(1, n) %>%
                                  ungroup() |>
                                  group_by(Player) %>%
                                  slice(which.max(n)) %>%
                                  ungroup() |>
                                  select(-n)

# remove the Pos column
players_features = players_features |> 
                         filter(Pos != "GK") |>
                         select(-Pos)
# RESULT: players_features df contains all the summary stats for players of ENG, ITA, ESP of 17/18
# I need to group and aggregate the players_features df
players_features = players_features |> 
  group_by(Player) |>
  summarise(Min = sum(Min), # minutes
            Gls = sum(Gls)/Min*90, # goals
            Ast = sum(Ast)/Min*90, # assist
            PK = sum(PK)/Min*90, # goals from penalties
            PKatt = sum(PKatt)/Min*90, # penalties attempted
            Sh = sum(Sh)/Min*90, # shots
            SoT = sum(SoT)/Min*90, # shots on target
            CrdY = sum(CrdY)/Min*90, # yellow card
            CrdR = sum(CrdR)/Min*90, # red card
            Touches = sum(Touches)/Min*90, # touches
            Tkl = sum(Tkl)/Min*90, # tackles
            Int = sum(Int)/Min*90, # interceptions
            Blocks = sum(Blocks)/Min*90, # blocks
            xG_Expected = sum(xG_Expected)/Min*90, # xG by Opta does not contain penalty shotouts but contains in-game penalty kicks
            npxG_Expected = sum(npxG_Expected)/Min*90, # non penalty xG
            xAG_Expected = sum(xAG_Expected)/Min*90, # exp. assisted goals: xG after a pass
            SCA_SCA = sum(SCA_SCA)/Min*90, # shot-creating actions 
            GCA_SCA = sum(GCA_SCA)/Min*90, # shot-creating actions
            Cmp_Passes = sum(Cmp_Passes)/Min*90, # passes completed
            Att_Passes = sum(Att_Passes)/Min*90, # passes attempted
            Cmp_percent_Passes = mean(Cmp_percent_Passes),
            PrgP_Passes = sum(PrgP_Passes)/Min*90, # progressive passes
            Carries_Carries = sum(Carries_Carries)/Min*90, # carries
            PrgC_Carries = sum(PrgC_Carries)/Min*90, # progressive carries
            Att_Take_Ons = sum(Att_Take_Ons)/Min*90, # take-ons attempted
            Succ_Take_Ons = sum(Succ_Take_Ons)/Min*90 # successful take-ons
            )

# NOW FIX THE NAMES FROM FBref and Wyscout to match them

fullnames = read.csv("C:/Users/leoac/OneDrive - Università degli Studi di Milano/Unimi/Subjects/1st_year/03_quarter/Statistical learning/playerank_R/Code/cop_full_names.csv", sep = ";")
cop = cop |>
  mutate(playerFullName = fullnames$playerName)

copFeat = left_join(x = cop, y = players_features, by = c("playerFullName" = "Player"))
copFeat_na = copFeat[is.na(copFeat$Min),] # 248 NAs i.e. not joined
copFeat_na

copFeat_distjoin = stringdist_inner_join(copFeat_na, players_features, by = c("playerFullName" = "Player"), method = "jw", max_dist = 0.1)
print(copFeat_distjoin[,c("shortName","playerFullName","Player")], n = 1000) # here i matched 52 people but there are still 248-52=196 missing

cop$playerFullName[cop$playerFullName %in% copFeat_distjoin$playerFullName] = copFeat_distjoin$playerFullName

# substitute THE 52 DISTJOIN MATCHES to the cop df
for (i in 1:nrow(cop)){
  # check that has been matched by distjoin df
  if (cop$playerFullName[[i]] %in% copFeat_distjoin$playerFullName){
    new_name = copFeat_distjoin$Player[copFeat_distjoin$playerFullName == cop$playerFullName[[i]]]
    if (length(new_name) > 1){
      cop$playerFullName[[i]] = new_name[[1]]
      paste(cop$playerFullName[[i]], new_name[[1]])
    } else {
      cop$playerFullName[[i]] = new_name
      paste(cop$playerFullName[[i]], new_name)
      }
  }
}
# !!!! hurray! It worked !!!

# Now go with the ones matched through substring containing
copFeat_na$playerFullName[sapply(copFeat_na$playerFullName, function(x) any(grepl(x, players_features$Player)))]

matching_players <- sapply(copFeat_na$playerFullName, function(x) {
  matching_player <- grep(x, players_features$Player, value = TRUE)
  if (length(matching_player) > 1) {
    matching_player[[1]]
  } else if (length(matching_player) > 0) {
    matching_player
  } else {
    NA
  }
})
matching_players = data.frame(matches = matching_players)
matching_players = rownames_to_column(matching_players, var = "copFeatNA_name")
matching_players = matching_players[!is.na(matching_players$matches),]
rownames(matching_players) <- NULL # to reset the index

# prima metto i nomi nuovi in cop e poi rifaccio il join con Feat
merge_cop_matching_players = left_join(cop, matching_players, by = c("playerFullName" = "copFeatNA_name"))
merge_cop_matching_players[is.na(merge_cop_matching_players$matches),]

merge_cop_matching_players$matches = apply(merge_cop_matching_players,1, function(row) {
  if (is.na(row["matches"])){
    row["matches"] = row["playerFullName"]
  } else {
    row["matches"] = row["matches"]
  }
})

merge_cop_matching_players$playerFullName = merge_cop_matching_players$matches
merge_cop_matching_players = merge_cop_matching_players |> select(-matches)
cop = merge_cop_matching_players
# ------------------------------------------
# JOIN again cop and players_features
# rearrange the pipe in a clear way
copFeatures = inner_join(cop, players_features, by = c("playerFullName" = "Player"))
copFeatures = copFeatures[!is.na(copFeatures$Min),] # drop one all NA features row (G. N'Koudou)
copFeatures = copFeatures |> filter(Min > 450) |> # at least 5 games
  select(-Cmp_percent_Passes) 
copFeatures = copFeatures |>
  inner_join(players, by = c("playerId" = "wyId", "shortName" = "shortName")) |> select(names(copFeatures), "weight", "height", "foot")
copFeatures$Rfoot = sapply(copFeatures$foot, function(x) {if(x %in% c("right", "both", "")){1} else{0}})
copFeatures = copFeatures |> select(-foot)
colnames(copFeatures) = c("playerId","shortName","mean_x","mean_y","fullName","Min","Goals","Assists","Penalties","Penalties_att","Shots","Shots_oT","Ycard","Rcard","Touches","Tackle","Interceptions","Blocks","xG","npxG","xAG","SCA","GCA","Passes_cmp","Passes_att","Passes_prg","Carries","Carries_prg","TakeOns_att","TakeOns_succ","Weight","Height","Rfoot")

# The workspace is now saved