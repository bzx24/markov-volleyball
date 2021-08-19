library(datavolley)
library(dplyr)
library(hash)

#read in data
files <- list.files(path = "./dvw_files", pattern = "\\.dvw$")
x <- read_dv(paste0("./dvw_files/", files[1]))
data <- x$plays

for(i in 2:length(files)) {
  x <- read_dv(paste0("./dvw_files/", files[i]))
  hold <- x$plays
  data <- rbind(data,hold)
}
remove(hold, x, i, files)

#tidy data
markov_data <- data %>%
  filter(!is.na(skill)) %>%
  select(point_id, team, skill, skill_type, evaluation, start_zone, end_zone, end_subzone)

#create sequence of states
pass_states <- c("Pass Error", "Overpass", "P1A", "P1B", "P1C", "P1D", "P2A", "P2B", "P2C", "P2D", "P3A", "P3B", "P3C", "P3D", 
                 "P4A", "P4B", "P4C", "P4D", "P5A", "P5B", "P5C", "P5D", "P6A", "P6B", "P6C", "P6D", "P7A", "P7B", "P7C", "P7D", "P8A", 
                 "P8B", "P8C", "P8D", "P9A", "P9B", "P9C", "P9D")
sequence <- create_sequence(markov_data, pass_states)

#compute counts of states in sequence
states <- c("Serve", "Perfect Set", "Poor Set", "Rally Won", "Rally Lost", "Rally Continuation")
states <- append(states, pass_states)
vec <- compute_counts(sequence, states)
state_counts <- vec[1]
transition_counts <- vec[2]

#create transition matrix
transition_matrix <- matrix(, nrow = 44, ncol = 44)
colnames(transition_matrix) <- states
rownames(transition_matrix) <- states

#create markov chain object


#functions
create_sequence <- function(df, pass_states) {
  sequence <- c("Serve")
  
  #iterate through dataframe
  for (row in 2:nrow(df)) {
    skill <- df[row, "skill"]
    evaluation <- df[row, "evaluation"]
    state <- c()
    
    #only include first possession
    if (skill == "Dig" | skill == "Block" | skill == "Freeball" | skill == "Technical Timeout") next
    if (skill == "Set" & !(sequence[length(sequence)] %in% pass_states)) next
    if (skill == "Attack" & !(sequence[length(sequence)] %in% c("Perfect Set", "Poor Set"))) next
    
    #if skill is serve
    if (skill == "Serve") {
      if (evaluation == "Error") {
        state <- c(skill, "Rally Won")
      }
      else {
        state <- c(skill)
      }
      sequence <- append(sequence, state)
    }
    
    #if skill is reception
    else if (skill == "Reception") {
      if (evaluation == "Error") {
        state <- c("Pass Error", "Rally Lost")
      }
      else if (evaluation == "Poor, no attack") {
        state <- c("Overpass", "Rally Continuation")
      }
      else {
        if (markov_data[row + 1, "skill"] == "Set") {
          #pass location is actually recorded in set end zone
          pass <- paste("P", df[row + 1, "end_zone"], df[row + 1, "end_subzone"], sep = "")
          state <- c(pass)
        }
      }
      sequence <- append(sequence, state)
    }
    
    #if skill is set
    else if (skill == "Set") {
      if (evaluation == "Error") {
        next
      }
      else if (evaluation == "Perfect") {
        state <- c("Perfect Set")
      }
      else if (evaluation == "Poor") {
        state <- c("Poor Set")
      }
      sequence <- append(sequence, state)
    }
    
    #if skill is attack
    else if (skill == "Attack") {
      if (evaluation == "Error" | evaluation == "Blocked") {
        state <- c("Rally Lost")
      }
      else if (evaluation == "Winning attack") {
        state <- c("Rally Won")
      }
      else {
        state <- c("Rally Continuation")
      }
      sequence <- append(sequence, state)
    }
  }
  return(sequence)
}

compute_counts <- function(seq, states) {
  state_counts <- hash()
  trans_from <- hash()
  trans_to <- hash()
  
  #set value for each key to zero
  for (state in states) {
    state_counts[[state]] <- 0
    trans_to[[state]] <- 0
  }
  #build hash within hash
  for (state in states) {
    trans_from[[state]] <- trans_to
  }
  
  #compute counts
  n <- length(seq)
  for (idx in 1:n) {
    state <- seq[idx]
    #state counts
    state_counts[[state]] <- state_counts[[state]] + 1
    
    #transition counts
    if (idx < n - 1) {
      transition_counts[[state]]
    }
  }
  
  return(c(state_counts, trans_from))
}
  
  #compute state count
  for (state in seq) {
    state_count[[state]] <- state_count[[state]] + 1
  }
  
  #compute state transition count
}

compute_transition_probabilities <- function(hash) {
  
}