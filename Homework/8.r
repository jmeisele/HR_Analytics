install.packages("arules")
library("arules")

install.packages("arulesViz")
library(arulesViz)

###################
# explore the groceries dataset
###################

data(Groceries)

g <- Groceries
inspect(g[1:5])
length(g)  # number of baskets
str(g) # structure of g


data <- g@data
str(data)
means <- rowMeans(data)
str(means) # % of how many times item appears in basket
str(g@itemInfo)
g@itemInfo[,1]
labels <- g@itemInfo[,1]
levels <- g@itemInfo$level2

df <- data.frame(means, labels, levels)
df1 <- df[order(-df$means),]

#################
# now to use aRules
#################

itemFrequencyPlot(Groceries, support = 0.1)

rules <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.5))

rules1 <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))
inspect(rules1)


#generate lots of rules
ruleset <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.35))
par(mfrow = c(1,1))
plot(ruleset)

#look at rules with lift above xxx
goodrules <- ruleset[quality(ruleset)$lift > 3.5]
plot(goodrules)
inspect(goodrules)


#and suppot above xxx
betterrules <- goodrules[quality(goodrules)$support > 0.01]
plot(betterrules)


#look for beer rules
beerRules <- aprior(Groceries, parameter = list(support = 0.001, confidence = 0.25))
