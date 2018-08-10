library(sqldf)
library(ISLR)
library(tree)
library(randomForest)

FIFA = read.csv('C:/Users/Omar/Desktop/WorldCupProject/FIFARosters.csv',header = TRUE, na.strings = ".")
SUMMARY = sqldf("SELECT Year, Team, Avg(Age) AS AVGAGE, median(Age) AS MEDAGE, MAX(Age)-MIN(Age) as AGERANGE, 
      AVG(HeightM) AS AVGHEIGHT, median(HeightM) AS MEDHEIGHT, Count(AGE) AS AGECOUNT, Count(HeightM) AS HEIGHTCOUNT  
      FROM FIFA
      GROUP BY YEAR,TEAM")
write.csv(SUMMARY,file= "FIFASUMMARY.csv")

ELO = read.csv('C:/Users/Omar/Desktop/WorldCupProject/GDPPOP_to_ELO.csv',header = TRUE, na.strings = ".")

GDPELO = lm(Elo~GDP,data = ELO)
GDPELO

POPELO = glm(Elo~Population,data = ELO)
POPELO

GDPPERELO = glm(Elo~GDPPER,data = ELO)
GDPPERELO

ROSTER = read.csv('C:/Users/Omar/Desktop/WorldCupProject/RosterPerformance.csv',header = TRUE, na.strings = ".")

cor(ROSTER$Perf, ROSTER$AVGAGE)
cor(ROSTER$Perf, ROSTER$MEDAGE)
cor(ROSTER$Perf, ROSTER$AGERANGE)
cor(ROSTER$Perf, ROSTER$AVGHEIGHT)
cor(ROSTER$Perf, ROSTER$MEDHEIGHT)
cor(ROSTER$Perf, ROSTER$PerfPer)

MAGE = lm(formula = PerfPer~AVGAGE, data = ROSTER)
plot(ROSTER$Perf,ROSTER$AVGAGE)
summary(MAGE)

MAGE = lm(formula = PerfPer~MEDAGE, data = ROSTER)
plot(ROSTER$Perf,ROSTER$MEDAGE)
summary(MAGE)

ARAGE = lm(formula = PerfPer~AGERANGE, data = ROSTER)
summary(ARAGE)

AGHE = lm(formula = PerfPer~AGEHEIGHT, data = ROSTER)
summary(AGHE)

TROSTER = read.csv('C:/Users/Omar/Desktop/WorldCupProject/TreeRoster.csv',header = TRUE, na.strings = ".")

dt = sort(sample(nrow(TROSTER)*.7, nrow(TROSTER),replace = TRUE))
train<-TROSTER[dt,]
test<-TROSTER[-dt,]

AGETREE = tree(PerfPer~., data = train)
summary(AGETREE)

set.seed(1)
AGETREEPRED = predict(AGETREE,newdata = test)
summary(AGETREEPRED)


mse=mean((yhat.bag-AGETREE)^2)
r2=1-mse/var(AGETREE);r2
mean((TREEREG$y-TREEREG$predicted)^2)


FAV = read.csv('C:/Users/Omar/Desktop/WorldCupProject/FavSport.csv',header = TRUE, na.strings = ".")
FAVList = sqldf('SELECT DISTINCT Country, Count(FAVSPORT)
                FROM FAV
                GROUP BY Country')
FAVLIST
