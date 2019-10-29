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

#Organizing data by julian day
moth_by_day<-moth%>%
  group_by(julian.day)%>%
  summarize(nCount=sum(photos))

  
moth_aggregate<-moth%>%
  filter(year==2010)%>%
  group_by(julian.day)%>%
  summarize(nCount=sum(photos))%>%
  mutate(JulianWeek=7*floor((julian.day)/7)+4)%>%
  replace_na(list(nCount=0))%>%
  mutate(avgN=nCount/sum(nCount))

#  select(c(JulianWeek, nCount))%>%
# group_by(JulianWeek)%>%
#summarize(nCount=sum(nCount))
  
plot(x=moth_aggregate$JulianWeek, y=moth_aggregate$avgN)
plot(moth_by_day)


#Organizing data based on lunar phases

dfs <- list()
for(i in 2010:2019){
  moth_lunar<-moth%>%
    filter(year==i)%>%
    mutate(New.moon=(days.past.new.moon==0))%>%
    mutate(New.moon=replace(New.moon,New.moon==FALSE,0))%>%
    group_by(year,Lunar.Cycle=cumsum(New.moon)+1)%>%
    mutate(Lunar.Days=row_number())%>%
    select(c(days.past.new.moon,New.moon,Lunar.Cycle,Lunar.Days))
  dfs[[i]]<-moth_lunar
}
final_lunar<-bind_rows(dfs)
moth_lunar<-bind_cols(moth,final_lunar)


moth_set<-moth_lunar%>%
  group_by(year,Lunar.Cycle)%>%
  mutate(Lunar.Total=sum(photos),Phase.days=n(),Frac=photos/((Lunar.Total/Phase.days)),
         Lunar.avg=Lunar.Total/Phase.days)%>%
  replace_na(list(Frac=0))


postNmoon<-moth_set%>%
  group_by(year,Lunar.Cycle)%>%
  filter(Lunar.Days<=14)%>%
  mutate(RawCount=sum(photos))%>%
  mutate(nonzero=photos>0)%>%
  mutate(n=replace(nonzero,nonzero==FALSE,0))%>%
  group_by(year,Lunar.Cycle)%>%
  mutate(nonzerodays=sum(n))%>%
  select(-c(nonzero,n))

preNmoon<-moth_set%>%
  group_by(year,Lunar.Cycle)%>%
  filter(Lunar.Days>14)%>%
  mutate(RawCount=sum(photos))%>%
  mutate(nonzero=photos>0)%>%
  mutate(n=replace(nonzero,nonzero==FALSE,0))%>%
  group_by(year,Lunar.Cycle)%>%
  mutate(nonzerodays=sum(n))%>%
  select(-c(nonzero,n))

  
  #mutate(Lunar.Phase1=Lunar.Days<=14, Lunar.Phase2=Lunar.Days>14)%>%
 # mutate(Lunar.Phase1=replace(Lunar.Phase1,Lunar.Phase1==TRUE,1))%>%
  #group_by(Lunar.Cycle, Lunar.Phase1)%>%
  #mutate(id=seq_along())
  
  group_by(year,PostNewMoon=cumsum(Lunar.Phase1)+1)%>%
  mutate(Lunar.Days=row_number())
  
  


#Plot Frac. of avg for lunar days across lunar cycles
par(mfrow=c(3,3))
rainbowcols = rainbow(13)

lunar_ratio<-for(y in 2010:2018){
  moth_plot<-moth_set%>%
    filter(year==y,Lunar.Cycle==2)
  #Quadratic model
  quadmod<-moth_set%>%
    filter(year==y)
  Lunar2=quadmod$Lunar.Days^2
  quad<-lm(quadmod$Frac~quadmod$Lunar.Days+Lunar2)
  square<-summary(quad)$r.squared
  plot(main=y,x=moth_plot$Lunar.Days,y=moth_plot$Frac,type="l",
       col=rainbowcols[1],xlab="Lunar Days", ylab="Frac of Surveys")
    lines(predict(quad),)
    legend("topleft",bty="n",legend=paste("R^2=",square))
  for(i in 2:14){
    moth_plot<-moth_set%>%
      filter(year==y,Lunar.Cycle==i)
    points(x=moth_plot$Lunar.Days,y=moth_plot$Frac,type="l", col = rainbowcols[i])
  }
}




  
day1 = moth %>%
  filter(julian.day == 1) %>%
  select(year, days.past.new.moon)  %>%
  mutate(shift = days.past.new.moon - days.past.new.moon[1])
  
moth$julian.day.shifted = NA

for (y in 2010:2018) {
  moth$julian.day.shifted[moth$year == y] = moth$julian.day[moth$year == y] - day1$shift[day1$year == y]
}


# mutate(Counter=X==1000)%>%
 # mutate(Counter=replace(Counter,Counter==FALSE,1))%>%
 
 # mutate(Lunar.Cycle=rep(1:n,each=n))

#  moth_lunar$Lunar.Cycle<-sequence(rle(moth_lunar$New.moon)$lengths)
  
  

 #moth_lunar$Lunar.Cycle[1:14,]<-1
  

#moth_lunar<-moth%>%
#  filter(year==2010)%>%
#  {ifelse(moth_lunar$days.past.new.moon==0,1,0)}%>%
  
#  group_by(year,Lunar.Cycle=cumsum(New.moon==1L)+1)%>%
#  mutate(Lunar.Days=row_number())


