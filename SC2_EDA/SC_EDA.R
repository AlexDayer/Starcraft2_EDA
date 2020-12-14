### Research Questions ### 

#What variables are correlated with skill level in Starcraft 2? 

### Load Packages ### 

library(Rmisc)
library(tidyverse)
library(skimr)
library(dplyr)
library(ggplot2)
library(visdat)
library(corrplot)



### Load Data ###

dataSC_raw = read_csv('starcraft.csv')

### Check Data ###

skim(dataSC_raw)

### Clean Data ###

dataSC_clean = dataSC_raw %>% 
  filter(!is.na(Age),
         !is.na(HoursPerWeek),
         !is.na(TotalHours), TotalHours < 500000)

### Double Check Data ### 
skim (dataSC_clean)

### Correlations ###

corrMatrix = dataSC_clean %>%
  cor(method = 'spearman')

jpeg("CorrelationsPlot.jpg", width =350, height = 350)
corrPlot<-corrplot(corrMatrix, method='circle', order = "hclust")
dev.off()

### Create Averages and Arrange by Skill Level ###

Averages = dataSC_clean %>% 
  group_by(LeagueIndex) %>% 
  arrange(LeagueIndex) %>% 
  summarize(avgActionLatency = mean(ActionLatency), 
    sdActionLatency = sd(ActionLatency), 
    avgTotalSpent = mean(TotalHours), 
    sdTotalHours = sd(TotalHours), 
    avgAPM = mean(APM), 
    sdAPM = sd(APM),
    avgGapBetweenPacs = mean(GapBetweenPACs),
    sdGapBetweenPacs = sd(GapBetweenPACs)) %>% 
  ungroup()

### Convert Number of PACs from Timestamps to Seconds and Minutes / Arrange by Skill Level ### 

NumPacData = dataSC_clean %>% 
  mutate(PacsPerSec = NumberOfPACs*88.5)

NumPacPerMin = NumPacData %>%
  mutate(PacsPerMin = PacsPerSec*60)

NumPACMinSkill = NumPacPerMin %>% 
  group_by(LeagueIndex) %>% 
  arrange(LeagueIndex) %>% 
  summarize(
    avgNumPACsPerMin = mean(PacsPerMin), 
    sdNumPACsPerMin = sd(PacsPerMin)) %>% 
  ungroup()

### Convert SelectByHotKeys per Timestamp to Seconds and Minutes / Arrange by Skill Level ### 

HotKeyData = dataSC_clean %>% 
  mutate(HotKeyPerSec = SelectByHotkeys*88.5)

HotKeyPerMin = HotKeyData %>%
  mutate(HotKeyPerMin = HotKeyPerSec*60)

HotKeybySkill = HotKeyPerMin %>% 
  group_by(LeagueIndex) %>% 
  arrange(LeagueIndex) %>% 
  summarize(
    avgHotKeySelectsPerMin = mean(HotKeyPerMin), 
    sdHotKeySelectsPerMin = sd(HotKeyPerMin)) %>% 
  ungroup()

### Join Dataframes of Averages Together ### 

AveragesBySkillHot = inner_join(Averages, HotKeybySkill)
AveragesBySkill = inner_join(AveragesBySkillHot, NumPACMinSkill)

### Plot Average Action Latency by Skill ###

pAL = ggplot(AveragesBySkill, aes(LeagueIndex, avgActionLatency)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Action Latency by Skill") +
  xlab("League Index") + ylab("ms to First Action within PAC")

### Plot Total Hours Spent by Skill ###
pTS = ggplot(AveragesBySkill, aes(LeagueIndex, avgTotalSpent)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Total Hours Spent") +
  xlab("League Index") + ylab("Total Hours Spent")

### PLot Average APM by Skill ### 
pAPM = ggplot(AveragesBySkill, aes(LeagueIndex, avgAPM)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Actions Per Minute") +
  xlab("League Index") + ylab("Actions Per Minute")

### PLot Average Gap Between PACS by Skill ### 
pGBP = ggplot(AveragesBySkill, aes(LeagueIndex, avgGapBetweenPacs)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Average Gap Between PACs (ms)") +
  xlab("League Index") + ylab("Gap Between PACs (ms)")

### PLot Number PACs a Minute by Skill ### 
pNPM = ggplot(AveragesBySkill, aes(LeagueIndex, avgNumPACsPerMin)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Number PACs per Minute") +
  xlab("League Index") + ylab("PACs per Minute")

### PLot Hot Keys a Minute by Skill ### 
pHPM = ggplot(AveragesBySkill, aes(LeagueIndex, avgHotKeySelectsPerMin)) +
  geom_bar(stat='identity', color = 'black', fill = 'black') + ggtitle("Hotkey Selects per Minute") +
  xlab("League Index") + ylab("Hotkey Selects per Minute") 

#### Plot All ###

jpeg("bargraphs.jpg", width =350, height = 350)
multiplot(pAL, pTS, pAPM, pGBP, pNPM, pHPM, cols = 2)
dev.off()
