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
day21 <- read.csv2(file="SubjEq-16.12.15.csv", header=TRUE, dec=".", sep=";", fileEncoding="UTF-16LE", as.is = T, row.names=NULL)

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
day21$day <- 21

library("plyr", lib.loc="D:/Program Files (x86)/R-3.3.2/library")
library("dplyr", lib.loc="D:/Program Files (x86)/R-3.3.2/library")
alldays <- bind_rows(day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day14, day15, day16, day17, day18, day19, day20, day21, .id = NULL)

Daily_mice <- read.csv("~/Humboldt/1610_WiSe/Cognition & Behaviour/Presentation/Sub_Eq/Daily_schedule_mice_2.csv")

TheTable_2 <- merge(alldays,Daily_mice)

###Make Table with last 100 per day
LastRecords_2<-TheTable_2[order(TheTable_2$DateTime),]%>%subset(day > 19 & rel==1) %>%
  plyr::ddply(.,.(day,IdLabel),tail,n=100)

###Just last 100 *active* feeders
library("plyr")
LastRecords_2<-TheTable_2[order(TheTable_2$DateTime),]%>%subset(day > 19 & rel==1) %>%
  plyr::ddply(.,.(day,IdLabel),tail,n=100)
detach("package:plyr", unload=TRUE)

###Determine leftness

active_feeders<-subset(Daily_mice,day > 19 & rel==1)%>%
  group_by(IdLabel, day)%>%summarise(unit1=head(unitLabel,1),unit2=tail(unitLabel,1))

table_m2<-rbind(subset(active_feeders,str_detect(unit1,"l")&str_detect(unit2,"m"))[,1:2]%>%mutate(m=0),
               subset(active_feeders,str_detect(unit2,"r")&str_detect(unit1,"m"))[,1:2]%>%mutate(m=1))

LastRecords_2<-merge(LastRecords_2,table_m2,all.x = T)
LastRecords_2<-mutate(LastRecords_2,leftness=ifelse(str_detect(unitLabel,"l"),1,
                                                ifelse(str_detect(unitLabel,"r"),0,m)))

###Tests####
side_preference2<-LastRecords_2%>%group_by(m,IdLabel)%>%summarise(side_preference=mean(leftness))
t.test(side_preference2$side_preference,mu = 0.5)
#l & r
t.test(subset(side_preference2,is.na(m))$side_preference,mu=0.5)
#l & m
t.test(subset(side_preference2,m==0)$side_preference,mu=0.5)
#r & m
t.test(subset(side_preference2,m==1)$side_preference,mu=0.5)
#0-hypothesis: Mice avoid the r-feeder
t.test(subset(side_preference2,m==1| is.na(m))$side_preference,mu=0.5)

#Mean of leftness-preference by label
sapply(split(side_preference$side_preference,side_preference$IdLabel),mean)

#Plotting
plot<-as.data.frame(cbind(c("ID66", "ID67", "ID68", "ID69", "ID70", "ID71", "ID72", "ID73")))
colnames(plot)<-c("IdLabel")

library("ggplot2", lib.loc="D:/Program Files (x86)/R-3.3.2/library")

ggplot(plot, aes(IdLabel, mean))+stat_summary(fun.y = "mean", geom = "bar")+ylab("Average preference for left option")

side_preference2<-mutate(side_preference2,condition=as.factor(ifelse(is.na( m) , "lr",ifelse(m==0,"lm","mr"))))
stripchart(data = side_preference2,side_preference~condition,vertical = T)

ggplot(side_preference2, aes(condition, side_preference, col = IdLabel, geom="point")) +
  geom_point()+xlab("Condition")+ylab("Ratio of visits on left option") +
  stat_summary(fun.y = "median", fun.ymin = "median", fun.ymax= "median", size= 0.3, geom = "crossbar", color="black") +
  stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.5, geom = "crossbar", color = "gray")
