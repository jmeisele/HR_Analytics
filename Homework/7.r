install.packages('gdata')
library(gdata)
install.packages('dplyr')
library(dplyr)
install.packages('ggmap')
library(ggmap)
library(ggplot2)
df <- read.csv("C:\\Users\\jmeis\\Desktop\\MedianZIP-3.csv")
str(df)

is.null(df$Zip)
is.null(df$Median)
is.null(df$Mean)
is.null(df$Pop)

install.packages('zipcode')
library(zipcode)

names(df)[1] <- 'zip'
sum(is.na(df$zip))
names(df)[2] <- 'median'
sum(is.na(df$median))
names(df)[3] <- 'mean'
sum(is.na(df$mean))
names(df)[4] <- 'population'
sum(is.na(df$population))

data(zipcode)
zipdf <- zipcode

zipdf$zip <- as.numeric(zipdf$zip)

total <- merge(df,zipdf,by="zip")

us <- map_data("state")

set1 <- subset(total, total$state != 'HI')
lower48 <- subset(set1, total$state != 'AK')
lower48$state <- as.factor(lower48$state)
lower48$median <- as.numeric(lower48$median)
lower48$population <- as.numeric(lower48$population)
total$median <- as.numeric(total$median)


grouped <- lower48 %>% group_by(state) %>% summarise(population = median(population), median = mean(median))


map.simple <- ggplot(total, aes(map_id = state))
map.simple <- map.simple + geom_map(map = us, fill = 'white', color = 'black')

dummyDF <- data.frame(state.name, stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple + geom_map(map = us, fill = 'white', color = 'black')
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple <- map.simple + coord_map() + ggtitle("Basic map of continental USA")
map.simple
map.popColor <- ggplot(total, aes(map_id = state))
map.popColor <- map.popColor + geom_map(map = us, aes(fill = total$median))
map.popColor
