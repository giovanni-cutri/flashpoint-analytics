library(RSQLite)
library(dplyr)

dir.create("data")
url <- "http://infinity.unstable.life/Flashpoint/Data/flashpoint.sqlite"
destfile <- "data/flashpoint.sqlite"

# by default, there is a 60 seconds timeout limit when downloading a file
# this can be a problem when downloading large files, so we increase that limit
options(timeout = 300)

download.file(url, destfile)
  
con <- dbConnect(SQLite(), "data/flashpoint.sqlite")
games <- dbReadTable(con, "game")
dbDisconnect(con)

most_played <- games %>% filter(id %in% df$game_id)

most_played %>% filter(developer != "") %>% group_by(developer) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(publisher != "") %>% group_by(publisher) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(platform != "") %>% group_by(platform) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(source != "") %>% group_by(source) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(library != "") %>% group_by(library) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(tagsStr != "") %>% group_by(tagsStr) %>%
  summarise(total = n()) %>% arrange(desc(total))

most_played %>% filter(releaseDate != "") %>% arrange(releaseDate) %>%
  summarise(title = title, releaseDate = releaseDate)

tags <- list()
for (i in most_played$tagsStr) {
  j <- strsplit(i, "; ")
  for (tag in j) {
    tags <- append(tags, tag)
  }
}

tags
test <- do.call(rbind.data.frame, tags)
colnames(test) <- c("b")
test %>% group_by(b) %>%
  summarise(total = n()) %>% arrange(desc(total))


library(data.table)
setDT(tags)
mydata[, myorder := 1:.N, by = .(group, letter)]




games

devs <- games %>% filter(developer != "") %>% group_by(developer) %>% summarise(total = n()) %>% arrange(desc(total))

library(ggplot2)
  
pie(devs$total[1:5], devs$developer[1:5])
barplot(devs$total[1:5], names = devs$developer[1:5])


