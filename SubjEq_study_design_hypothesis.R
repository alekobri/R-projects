setwd("C:\\Users\\sunbu\\Documents\\Humboldt\\1610_WiSe\\Cognition & Behaviour\\Presentation\\Sub_Eq\\")

day01 <- read.csv2(file="Training_1234-16.11.25.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day02 <- read.csv2(file="Training_1234-16.11.26.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day03 <- read.csv2(file="Training_1234-16.11.27.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day04 <- read.csv2(file="Training_1234-16.11.28.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)

day05 <- read.csv2(file="SubjEq-16.11.29.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day06 <- read.csv2(file="SubjEq-16.11.30.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day07 <- read.csv2(file="SubjEq-16.12.01.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day08 <- read.csv2(file="SubjEq-16.12.02.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day09 <- read.csv2(file="SubjEq-16.12.03.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day10 <- read.csv2(file="SubjEq-16.12.04.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day11 <- read.csv2(file="SubjEq-16.12.05.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day12 <- read.csv2(file="SubjEq-16.12.06.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)

day13 <- read.csv2(file="Training_5678-16.12.07.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)

day14 <- read.csv2(file="SubjEq-16.12.08.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day15 <- read.csv2(file="SubjEq-16.12.09.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day16 <- read.csv2(file="SubjEq-16.12.10.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day17 <- read.csv2(file="SubjEq-16.12.11.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day18 <- read.csv2(file="SubjEq-16.12.12.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day19 <- read.csv2(file="SubjEq-16.12.13.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)
day20 <- read.csv2(file="SubjEq-16.12.14.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)

day01$day <- 1
day02$day <- 2
day03$day <- 3
day04$day <- 4
day05$day <- 5
day06$day <- 6
day07$day <- 7
day08$day <- 8
day09$day <- 9
day10$day <- 10
day11$day <- 11
day12$day <- 12
day13$day <- 13
day14$day <- 14
day15$day <- 15
day16$day <- 16
day17$day <- 17
day18$day <- 18
day19$day <- 19
day20$day <- 20

library("plyr", lib.loc="D:/Program Files (x86)/R-3.3.2/library")
library("dplyr", lib.loc="D:/Program Files (x86)/R-3.3.2/library")
alldays <- bind_rows(day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day14, day15, day16, day17, day18, day19, day20, .id = NULL)

Daily_mice <- read.csv("~/Humboldt/1610_WiSe/Cognition & Behaviour/Presentation/Sub_Eq/Daily_schedule_mice_2.csv")

TheTable <- merge(alldays,Daily_mice)

# How efficient are the mice?
library(stringr)

efficiency <- alldays%>%filter(str_detect(unitLabel, "Cond"))%>%
  subset(day > 6 & day != 13 & day != 20)%>%
  group_by(IdLabel, day)%>%
  summarise(max_value=sum(reinforce1value, na.rm=TRUE))

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel, ".l"))[2]/
  table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel,".l"))[2] 

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel, ".m"))[2]/
  table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel,".m"))[2] 

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel, ".r"))[2]/
 table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==0)$unitLabel,".r"))[2]

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel, ".l"))[2]/
  table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel,".l"))[2] 

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel, ".m"))[2]/
  table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel,".m"))[2] 

table(str_detect(subset(TheTable,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel, ".r"))[2]/
  table(str_detect(subset(Daily_mice,day > 6 & day != 13 & day != 20 & rel ==1)$unitLabel,".r"))[2]

###Make Table with last 100 per day
LastRecords<-TheTable[order(TheTable$DateTime),]%>%subset(day > 6 & day != 13 & day != 20) %>%
  plyr::ddply(.,.(day,IdLabel),tail,n=100)

##Count visits at inactive feeders
Inactive.visits<-subset(LastRecords,rel==0)%>%group_by(day,IdLabel)%>%
  dplyr::summarise(Count.total=n(),left=sum(str_detect(unitLabel,".l")),
                   middle=sum(str_detect(unitLabel,".m")),right=sum(str_detect(unitLabel,".r")))
#Remove 0s
Right<-Inactive.visits$right[Inactive.visits$right!=0]
Mid<-Inactive.visits$middle[Inactive.visits$middle!=0]
Left<-Inactive.visits$left[Inactive.visits$left!=0]
t.test(Right,Left)
t.test(Right,Mid)
t.test(Left,Mid)

##Count visits at active feeders
Active.visits<-subset(LastRecords,rel==1)%>%group_by(day,IdLabel)%>%
  dplyr::summarise(Count.total=n(),left=sum(str_detect(unitLabel,".l")),
                   middle=sum(str_detect(unitLabel,".m")),right=sum(str_detect(unitLabel,".r")))
#Remove 0s
Right<-Active.visits$right[Active.visits$right!=0]
Mid<-Active.visits$middle[Active.visits$middle!=0]
Left<-Active.visits$left[Active.visits$left!=0]
t.test(Right,Left)
t.test(Right,Mid)
t.test(Left,Mid)
