library(datavolley)

#read in data
files <- list.files(path = "./dvw_files", pattern = "\\.dvw$")
x <- read_dv(paste0("./dvw_files/", files[1]))
data <- x$plays
# data$date <- as_date(x$meta$match$date)

for(i in 2:length(files)) {
  x <- read_dv(paste0("./dvw_files/", files[i]))
  hold <- x$plays
  # hold$date <- as_date(x$meta$match$date)
  data <- rbind(data,hold)
}
remove(hold, x, i, files)

#hello