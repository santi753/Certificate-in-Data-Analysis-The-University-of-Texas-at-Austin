################
# Load packages
################

install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")
install.packages("RColorBrewer")
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(RColorBrewer)

##############
# Import data 
##############

setwd('C:/Users/santi/OneDrive/Documentos/CURSO DATA ANALYTICS/Cours III/Course 3 Task 4/data/ElectronidexTransactions2017')

##-- Load Dataset --##

ET <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep = ",", rm.duplicates = T)

################
# Evaluate data
################

summary(ET)
inspect(head(ET, n = 10))
ET[,1:5]%>%itemFrequency
length(ET) # Number of transactions.
size(ET) # Number of items per transaction
LIST(head(ET, n = 10)) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(ET)# To see the item labels
itemInfo(ET)

######################
#  Data Visualizations
######################

itemFrequencyPlot(ET,topN=10,type="absolute", main = "Most frequent items", ylab="Item frequency", col=brewer.pal(8,'Pastel2'))
itemFrequencyPlot(ET,topN=20, support=0.05)
image(ET[1:10,])
image(ET[1:10,], aspect=0.5)
image(sample(ET, 10), aspect=0.5)


###################
# Association Rules 
###################

#------Rules--------#

RulesName <- apriori(ET, parameter = list(supp = 0.001, conf = 0.8))
RulesName <- RulesName[!is.redundant(RulesName)]
is.redundant(RulesName)
summary(RulesName)
inspect(head(sort(RulesName, by = "confidence"), n=30))
inspectDT(head(sort(RulesName, by = "confidence"), n=30))
inspect(head(sort(RulesName, by = "lift"), n=30))
inspectDT(head(sort(RulesName, by = "lift"), n=30))
ItemRules <- subset(RulesName, items %in% "iMac")
ItemRules

#------Rules with iMac--------#

RulesName1 <-apriori(data=ET, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="iMac"),
               control = list(verbose=F))
RulesName1 <-sort(RulesName1, decreasing=TRUE,by="confidence")
inspect(RulesName1[1:20])

#-------Rules 2-------#

RulesName2 <- apriori(ET, parameter = list(supp = 0.01, conf = 0.4))
inspect(head(sort(RulesName2, by = "lift"), n=20))


######################
# Rules Visualizations
######################

plot(RulesName[1:5], method="graph", control=list(type="items"))
plot(RulesName, control=list(jitter=2, col = "green"), shading = "lift")
plot(RulesName[1:20], method = "grouped")
topRules <- RulesName[1:5]
plot(topRules)
plot(topRules, measure="confidence", method="graph", control=list(type="items"), shading = "lift")
plot(topRules, method = "grouped")
plot(topRules, method = "matrix")
plot(topRules, method = "paracoord")

rules.sub <- subset(RulesName, subset = lift > 1.5)
plot(rules.sub, method = "graph", engine = "html")

