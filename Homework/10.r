str(df)
df <- data.frame(mlr01)
names(df)[1] <- "fawns"
names(df)[2] <- "adult population"
names(df)[3] <- "precipitaion"
names(df)[4] <- "winter rating"
plot(x = df$`adult population`, y = df$fawns)
plot(x = df$precipitaion, y = df$fawns)
plot(x = df$`winter rating`, y = df$fawns)

library(ggplot2)
ggplot(df, aes(x=df$`adult population`, y=df$fawns)) + geom_point() + xlab("Adult Population") + ylab("Fawns")
ggplot(df, aes(x=df$precipitaion, y=df$fawns)) + geom_point() + xlab("Precipitation") + ylab("Fawns")
ggplot(df, aes(x=df$'winter rating', y=df$fawns)) + geom_point() + xlab("Winter Rating") + ylab("Fawns")

model1 <- lm(formula = df$fawns ~ df$`winter rating`, data = df)
summary(model1)

model2 <- lm(formula = df$fawns ~ df$`winter rating`+ df$`adult population`, data = df)
summary(model2)

model3 <- lm(formula = df$fawns ~ df$`adult population` + df$precipitaion + df$`winter rating`, data = df)
summary(model3)
