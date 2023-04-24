library(RSQLite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

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

str(games)

games <- games %>% select(id, title, developer, publisher, platform,
                          releaseDate, language, library, tagsStr)

str(games)

head(games)

top_developers <- games %>% count(developer) %>% arrange(desc(n)) %>% head(10)
top_developers

top_developers <- games %>% filter(developer != "") %>% count(developer) %>%
  arrange(desc(n)) %>% head(10)
top_developers

top_publishers <- games %>% filter(publisher != "") %>% count(publisher) %>%
  arrange(desc(n)) %>% head(10)
top_publishers

top_developers %>% as.data.frame() %>%
  ggplot(aes(x = reorder(developer, n), y = n, fill = developer)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(breaks = seq(0, 2500, by = 500)) +
  ggtitle("Top ten developers distribution") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold"))
  
sizes <- round(top_developers$n / sum(top_developers$n) * 100, 1)
labels <- vector()

for(i in 1:(length(sizes))){
  labels[i] <- paste(top_developers$developer[i], "-", toString(sizes[i]), "%")
}
  
top_developers %>% as.data.frame() %>%
  ggplot(aes(x = "", y = sizes, fill = reorder(developer, -n))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  theme(axis.text = element_blank(), axis.title.x = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = labels) +
  ggtitle("Top ten developers distribution")


top_publishers %>% as.data.frame() %>%
  ggplot(aes(x = reorder(publisher, n), y = n, fill = publisher)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(breaks = seq(0, 8000, by = 1000)) +
  ggtitle("Top ten publishers distribution") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold"))

sizes <- round(top_publishers$n / sum(top_publishers$n) * 100, 1)
labels <- vector()

for(i in 1:(length(sizes))){
  labels[i] <- paste(top_publishers$publisher[i], "-", toString(sizes[i]), "%")
}

top_publishers %>% as.data.frame() %>%
  ggplot(aes(x = "", y = sizes, fill = reorder(publisher, -n))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  theme(axis.text = element_blank(), axis.title.x = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = labels) +
  ggtitle("Top ten publishers distribution")


dates <- games %>% filter(releaseDate != "") %>% select(title, releaseDate,
                                                        platform, library) %>%
  arrange(releaseDate)

head(dates)
tail(dates)

options(warn = -1)
dates$releaseDate <- dates$releaseDate %>% ymd(truncated = 2)
options(warn = 0)
dates <- dates %>% na.omit() %>% arrange(releaseDate)
head(dates)
tail(dates)

dates <- dates %>% slice(2:n())
head(dates, 20)

dates <- dates %>% slice(2:n()-2)
tail(dates, 20)

dates %>% filter(platform == "Flash") %>% head(20)


top_platforms <- dates %>% count(platform) %>% arrange(desc(n)) %>% head(5)
top_platforms


colors <- c("red", "orange", "blue", "green", "black")

top_platforms %>% as.data.frame() %>% arrange(platform) %>%
  ggplot(aes(x = reorder(platform, n), y = n, fill = platform)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(breaks = seq(0, 60000, by = 10000)) +
  ggtitle("Top five platforms distribution") +
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold")) +
  scale_fill_manual(values = colors)

sizes <- round(top_platforms$n / sum(top_platforms$n) * 100, 1)
labels <- vector()

for(i in 1:(length(sizes))){
  labels[i] <- paste(top_platforms$platform[i], "-", toString(sizes[i]), "%")
}

colors <- c("red", "orange", "green", "black", "blue")

top_platforms %>% as.data.frame() %>%
  ggplot(aes(x = "", y = sizes, fill = reorder(platform, -n))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y", start = 0) +
  theme(axis.text = element_blank(), axis.title.x = element_blank(),
        legend.title=element_blank()) +
  scale_fill_manual(labels = labels, values = colors) +
  ggtitle("Top five platforms distribution")
  


year_platform <- dates %>% filter(platform %in% top_platforms$platform)
years <- year_platform$releaseDate %>% summarise(year = releaseDate[1:4])
years




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


