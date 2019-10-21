#reading in Moth abundance and Coweeta data
library(dplyr)
library(stringr)
library(tidyr)

moth <- read.table('c:/git/moth-caterpillar-comparison/data/moth-abundance.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
  filter(site=='Blue Heron')
coweeta<- read.table('c:/git/moth-caterpillar-comparison/data/coweeta_cats.txt',header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
  filter(Year>2009)
#Moth abundance data seems to be clean and complete, has NA's for the morpho species but don't think I need to worry about that.
#Coweeta data is filtered for 2010 and beyond and each observation has the appropriate amount of leaves (n>40 leaves). 
#Still need to look into filtering Coweeta data for duplicates of 0 in both records (Same year/yearday/sample, etc, but 0 in both records)
#Look at two ways to organize the moth data, one where the years are all aggregated and the julian day records are matched together
#The other way is to organize by the lunar phases, so to create some function that takes the julian day past new moon and subtract from the first day of that year.

moth_by_day<-moth%>%
  group_by(julian.day)%>%
  summarize(nCount=sum(daily.moth.species))

  
moth_aggregate<-moth%>%
  filter(year==2010)%>%
  group_by(julian.day)%>%
  summarize(nCount=sum(daily.moth.species))%>%
  mutate(JulianWeek=7*floor((julian.day)/7)+4)%>%
  replace_na(list(nCount=0))%>%
  mutate(avgN=nCount/sum(nCount))

#  select(c(JulianWeek, nCount))%>%
# group_by(JulianWeek)%>%
#summarize(nCount=sum(nCount))
  
plot(x=moth_aggregate$JulianWeek, y=moth_aggregate$avgN)

plot(moth_by_day)
lines()


#CreateCounter <- function(curr.count) {
#  list(
#    increment = function(amount) {
#      curr.count <<- curr.count + amount
#    },
#    value = function() {
#      return(curr.count)
#    }
#  )
#}

moth_lunar<-moth%>%
  select(c(year,month,day,days.past.new.moon))%>%
  group_by(month, days.past.new.moon)%>%
  mutate(Lunar.Cycle=seq(days.past.new.moon))
  
for(i in 0:29){
  
}

  #moth_lunar$Lunar.Cycle[1:14,]<-1
plot(moth_lunar$days.past.2009,moth_lunar$daily.moth.species)

