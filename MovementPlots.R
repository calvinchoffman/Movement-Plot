#Import Libraries
library(tidyverse)
library(ggplot2)

#Read in Desired CSV File
data <- read_csv("Clean_Danny_Report.csv")

#Filter Pitcher by TM file name 
Danny<- data %>% filter(Pitcher=="Avitia, Daniel")

#Create PLot
TotalPitches<- ggplot(Danny) +
  aes(x = `HorzBreak`, y = InducedVertBreak, color = TaggedPitchType) +
  geom_point() +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
  scale_x_continuous(breaks = seq(-30, 30, 5)) +
  scale_y_continuous(breaks = seq(-30, 30, 5))

#Create Averages Plot
Averages <- Danny %>% group_by(TaggedPitchType) %>% summarise(HB=mean(HorzBreak), VB=mean(InducedVertBreak), MPH=mean(RelSpeed)) 
AVG <- ggplot(Averages) +
aes(x = HB, y = VB, color = TaggedPitchType) +
   geom_point(size=2) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
  scale_x_continuous(breaks = seq(-30, 30, 5)) +
  scale_y_continuous(breaks = seq(-30, 30, 5))+
  geom_text(aes(label = MPH), vjust = -2)

#Save Plots
ggsave("Jun10AVGsDanny.png", AVG)
ggsave("Jun10TotalDanny.png", TotalPitches)