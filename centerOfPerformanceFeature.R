library(jsonlite)
library(dplyr)
library(stringr)

centerOfPerformanceFeature <- function(events_path, players_file, select = NULL) {
  
  # Helper function to filter events by select function
  filter_events <- function(events, select) {
    if (!is.null(select)) {
      return(filter(events, select))
    } else {
      return(events)
    }
  }
  
  # Read events data from JSON files
  read_events <- function(events_path, select) {
    events <- list()
    files <- list.files(events_path, full.names = TRUE)
    for (file in files) {
      data <- fromJSON(file)
      # if select !is.null (is not null)
      if (!is.null(select)) {
        data <- filter_events(data, select)
      }
      events <- c(events, data)
      cat(sprintf("[centerOfPerformanceFeature] added %s events from %s\n", length(data), file))
    }
    return(events)
  }
  
  # Read players data from JSON file
  read_players <- function(players_file) {
    players <- fromJSON(players_file)
    return(players)
  }
  
  # Compute average coordinates and count for each player and match
  compute_features <- function(events, players) {
    MIN_EVENTS <- 10
    players_positions <- list()
    for (evt in events) {
      if ("positions" %in% names(evt)) {
        player <- evt$playerId
        match <- evt$matchId
        position <- c(evt$positions[[1]]$x, evt$positions[[1]]$y)
        if (is.null(players_positions[[match]])) {
          players_positions[[match]] <- list()
        }
        if (is.null(players_positions[[match]][[player]])) {
          players_positions[[match]][[player]] <- list()
        }
        players_positions[[match]][[player]] <- c(players_positions[[match]][[player]], position)
      }
    }
    
    results <- list()
    for (match in names(players_positions)) {
      for (p in names(players_positions[[match]])) {
        positions <- players_positions[[match]][[p]]
        x <- mean(sapply(positions, function(pos) pos[1]))
        y <- mean(sapply(positions, function(pos) pos[2]))
        count <- length(positions)
        if (count > MIN_EVENTS) {
          documents <- list(
            list(feature = "avg_x", entity = as.integer(p), match = as.integer(match), value = as.integer(x)),
            list(feature = "avg_y", entity = as.integer(p), match = as.integer(match), value = as.integer(y)),
            list(feature = "n_events", entity = as.integer(p), match = as.integer(match), value = count)
          )
          results <- c(results, documents)
        }
      }
    }
    
    return(results)
  }
  
  # Main execution
  
  events <- read_events(events_path, select)
  players <- read_players(players_file)
  
  goalkeepers_ids <- players %>%
    filter(role$name == "Goalkeeper") %>%
    select(wyId) %>%
    pull() %>%
    setNames("GK")
  
  events <- events %>%
    filter(!(playerId %in% goalkeepers_ids))
  
  aggregated_features <- list()
  players_positions <- list()
}


##############
library(StatsBombR)
library(dplyr)
StatsBombData <- free_allevents()
comp = FreeCompetitions()
recent_comp = comp %>% filter(season_name %in% c("2017/2018","2018/2019","2019/2020","2020/2021","2021/2022","2022/2023"))
columns_needed = names(recent_comp)[c(3,4,5,6,8,11,12)]
recent_comp %>% select(columns_needed)
 