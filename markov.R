library(datavolley)
library(dplyr)

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
  select(point_id, skill, skill_type, evaluation, start_zone, end_zone, end_subzone)

#create sequence of states
sequences <- character(0)
for (row in 1:nrow(markov_data)) {
  
  #only include first possession
  if (markov_data[row, "skill"] == "Dig" | markov_data[row, "skill"] == "Block" | markov_data[row, "skill"] == "Freeball" | markov_data[row, "skill"] == "Technical Timeout") next
  
  if (skill == "Set" & sequences[length(sequences)] != "Reception") next
  
  if (skill == "Attack" & sequences[length(sequences)] != "Set") next
  
  #append states to sequences
  if (skill == "Reception") {
    
  }
  
  sequences <- append(sequences, state)
  if (evaluation == "Winning attack") {
    sequences <- append(sequences, "Rally Won")
  } 
  
  else if (evaluation == "Error") {
    sequences <- append(sequences, "Rally Lost")
  } 
  
  else {
    sequences <- append(sequences, "Rally Continuation")
  }
}

#create markov chain object
states <- c("Serve", "1A", "1B", "1C", "1D", "2A", "2B", "2C", "2D", "3A", "3B", "3C", "3D", "4A", "4B", "4C", "4D")


