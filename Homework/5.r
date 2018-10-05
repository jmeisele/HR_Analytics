df1 <- airquality
na.fill(df$Ozone, fill = mean(df$Ozone))


for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
is.na(df)
library(ggplot2)

ozone <- hist(df1$Ozone)
solar <- hist(df$Solar.R)
wind <- hist(df$Wind)
temp <- hist(df$Temp)

ozone_box <- boxplot(df$Ozone)
wind_box <- boxplot(df$Wind)

ggplot(df1, aes(x = Ozone)) + geom_histogram()
ggplot(df, aes(x = Wind)) + geom_histogram()
ggplot(df, aes(x = Solar.R)) + geom_histogram()
ggplot(df, aes(x = Temp)) + geom_histogram()

ggplot(df, aes(x=Day, y=Ozone, group = Day)) + geom_boxplot()

ggplot(df, aes(Day, Ozone ))

ggplot(df, aes(Day, Ozone )) +
  geom_tile(aes(fill = Ozone), color = "white") + scale_fill_gradient(low = "white", high = "steelblue")
+ ylab("Ozone")
+ xlab("Day")

ggplot(df1, aes(x=Wind, y=Temp)) +
  geom_point(aes(size=Ozone, color = Solar.R))
