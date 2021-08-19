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
  filter(!is.na(skill) & !is.na(end_zone) & !is.na(end_subzone)) %>%
  select(point_id, team, skill, skill_type, evaluation, start_zone, end_zone, end_subzone)

#create sequence of states
pass_states <- c("Pass Error", "Overpass", "P1A", "P1B", "P1C", "P1D", "P2A", "P2B", "P2C", "P2D", "P3A", "P3B", "P3C", "P3D", 
                 "P4A", "P4B", "P4C", "P4D", "P5A", "P5B", "P5C", "P5D", "P6A", "P6B", "P6C", "P6D", "P7A", "P7B", "P7C", "P7D", "P8A", 
                 "P8B", "P8C", "P8D", "P9A", "P9B", "P9C", "P9D")
sequence <- create_sequence(markov_data, pass_states)

#create transition matrix
states <- c("Serve", "Pass Error", "Overpass", "P1A", "P1B", "P1C", "P1D", "P2A", "P2B", "P2C", "P2D", "P3A", "P3B", "P3C", "P3D", 
            "P4A", "P4B", "P4C", "P4D", "P5A", "P5B", "P5C", "P5D", "P6A", "P6B", "P6C", "P6D", "P7A", "P7B", "P7C", "P7D", "P8A", 
            "P8B", "P8C", "P8D", "P9A", "P9B", "P9C", "P9D", "Perfect Set", "Poor Set", "Rally Won", "Rally Lost", "Rally Continuation")
transition_matrix <- compute_transition_probabilities(sequence, states)

#transition martix no setting
states2 <- c("Serve", "Pass Error", "Overpass", "P1A", "P1B", "P1C", "P1D", "P2A", "P2B", "P2C", "P2D", "P3A", "P3B", "P3C", "P3D", 
             "P4A", "P4B", "P4C", "P4D", "P5A", "P5B", "P5C", "P5D", "P6A", "P6B", "P6C", "P6D", "P7A", "P7B", "P7C", "P7D", "P8A", 
             "P8B", "P8C", "P8D", "P9A", "P9B", "P9C", "P9D", "Rally Won", "Rally Lost", "Rally Continuation")
sequence2 <- sequence[! sequence %in% c("Perfect Set", "Poor Set")]
transition_matrix2 <- compute_transition_probabilities(sequence2, states2)

#create markov chain object


#functions

#creates sequence of states from df
#returns vector of states
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


#computes state and state transition counts and transition probability between states
#returns transition matrix between states
compute_transition_probabilities <- function(sequence, states) {
  #initialize count data structures
  state_counts <- hash()
  transition_counts <- matrix(0, nrow = length(states), ncol = length(states))
  colnames(transition_counts) <- states
  rownames(transition_counts) <- states
  
  #set value for each key to zero
  for (state in states) {
    state_counts[[state]] <- 0
  }
  
  #compute counts
  n <- length(sequence)
  for (idx in 1:n) {
    state <- sequence[idx]
    #state counts
    state_counts[[state]] <- state_counts[[state]] + 1
    
    #transition counts
    if (idx < n) {
      next_state <- sequence[idx + 1]
      transition_counts[state, next_state] <- transition_counts[state, next_state] + 1
    }
  }
  
  #getting rid of incomplete data
  for (state in states) {
    if (!(state %in% c("Rally Won", "Rally Lost", "Rally Continuation"))) {
      count <- transition_counts[state, "Serve"]
      state_counts[[state]] <- state_counts[[state]] - count
      transition_counts[state, "Serve"] <- 0
    }
  }
  
  #initialize matrix
  transition_matrix <- matrix(0, length(states), length(states))
  colnames(transition_matrix) <- states
  rownames(transition_matrix) <- states
  
  #calculate probability between each state
  for (state1 in states) {
    for (state2 in states) {
      if (state2 == "Serve") next
      else {
        transition_matrix[state1, state2] <- transition_counts[state1, state2]/state_counts[[state1]]
      }
    }
  }
  
  return(transition_matrix)
}
