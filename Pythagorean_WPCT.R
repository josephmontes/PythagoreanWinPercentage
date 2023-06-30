#Projection System

library(Lahman)
library(tidyverse)

head(Batting)
head(Teams)

base<- Teams %>% 
  mutate(SLG= ((H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4))/(AB)) %>% 
  mutate(OBP= (H+HBP+BB)/(AB+HBP+BB+SF)) %>%
  filter(yearID == 2021) %>% 
  mutate(xRC=SLG*OBP*AB, xRA=ERA*(IPouts/27)) %>% 
  mutate(xWin = ((xRC)^2)/(((xRC)^2)+((xRA)^2))) %>% 
  mutate(WL= W/(W+L), xW=WL-xWin)

---------------------------------------------------------------------------
---------------------------------------------------------------------------

#Load the packages
library("retrosheet")
library(tidyverse)
library(Lahman)

#Exercise 1: Calculate the average score difference for a team in their wins and losses

#Store the entire 2021 season's game info from Retrosheets into a variable
rs2021 <- getRetrosheet("game", 2021)

#Isolate the games of one team
NYA <- subset(rs2021, HmTm == "NYA" | VisTm == "NYA")

#Create a column that calculates the score difference in each of their games
NYA$ScoreDiff <- with(NYA, ifelse(HmTm == "NYA",
                                  HmRuns - VisRuns,
                                  VisRuns - HmRuns))

#Use the score difference column to create a column that shows if the team won
NYA$Win <- NYA$ScoreDiff > 0

#Use the aggregate function to calculate the mean score difference in wins & in losses
aggregate(abs(NYA$ScoreDiff), list(W=NYA$Win), summary)

#Exercise 2: Over-performing your Pythagorean Win% by winning close games

#Identify the teamID of the winner in all games from a season
results<- rs2021[,c("VisTm", "HmTm", "VisRuns", "HmRuns")]
results$winner <- ifelse(results$HmRuns > results$VisRuns, as.character(results$HmTm),
                         as.character(results$VisTm))

#Calculate the score difference in all the games from that season
results$diff <- abs(results$VisRuns - results$HmRuns)

#Isolate one run games
onerungames <- subset(results, diff == 1)

#Create a dataframe that sums the amount of one run games each team won that season
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

#Use the Lahman Database to calculate the Win% and Run Differential of teams from that season
myteams <- subset(Teams, yearID == 2021)
myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, W/(W+L))

#Show the correlation between large run diffentials and Win%
plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab= "winning percentage")

#Use lm() function to determine unknown constants for simple linear model of Win%: a + b x RD + e
linfit <- lm(Wpct ~ RD, data=myteams)
abline(a=coef(linfit) [1], b=coef(linfit)[2], lwd=2)

#Calculate Linear Win% and residuals
myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

#Calculate Pythogorean Win% and residuals
myteams$pytWpct <- with(myteams, R^2 / (R^2 +RA^2))
myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct

#Merge this dataset with the previously created onerunwins dataframe
teams2021 <- subset(myteams, yearID == 2021)
teams2021 <- merge(teams2021, onerunwins)

#Plot the relationship between a team's Pythagorean residual and one run wins
plot(teams2021$onerunW, teams2021$pytResiduals, xlab="one run wins",
     ylab="Pythagorean residuals")

#Identify the two extremes of the Pythagorean residuals + the team you analyzed in Exercise 1
most_extreme <- which(teams2021$pytResiduals == max(teams2021$pytResiduals))
least_extreme <- which(teams2021$pytResiduals == min(teams2021$pytResiduals))
Yankees <- which(teams2021$teamID == "NYA")

#Label the points
points(teams2021$onerunW[most_extreme], teams2021$pytResiduals[most_extreme], col="red", pch=16, cex=1.5)
points(teams2021$onerunW[least_extreme], teams2021$pytResiduals[least_extreme], col="blue", pch=16, cex=1.5)
points(teams2021$onerunW[Yankees], teams2021$pytResiduals[Yankees], col="yellow", pch=16, cex=1.5)
text(teams2021$onerunW[most_extreme], teams2021$pytResiduals[most_extreme],
     teams2021$teamID[most_extreme], pos=2)
text(teams2021$onerunW[least_extreme], teams2021$pytResiduals[least_extreme],
     teams2021$teamID[least_extreme], pos=2)
text(teams2021$onerunW[Yankees], teams2021$pytResiduals[Yankees],
     teams2021$teamID[Yankees], pos = 2)

#This code was recommended by the book  to more easily identify and label the points , but it was not running and not returning an error code for me
identify(teams2021$onerunW, teams2021$pytResiduals,
         labels = teams2021$teamID) 