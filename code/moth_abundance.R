#reading in Moth abundance and Coweeta data
library(dplyr)
library(stringr)
library(tidyr)
library(pracma)
library(gridExtra)
library(compare)
library(mixtools)
library(data.table)
source('../caterpillars-analysis-public/code/analysis_functions.r')



fitG = function(x, y, mu, sig, scale, ...){
  
  f = function(p){
    
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
  
}

locmax=function(df, dipFromPeak=0.1){
  photoDiff = diff(df$avg)
  diffRelativeToMax = photoDiff/max(df$avg,na.rm=TRUE)
  firstIndexRaw = min(which(diffRelativeToMax< -dipFromPeak))
  
  runs = rle(sign(diffRelativeToMax))
  runIDs = rep(1:length(runs$lengths),runs$lengths)
  runSum = sapply(1:length(runs$lengths), function(x)
    sum(diffRelativeToMax[runIDs==x]))
  runIndex = min(which(runSum< -dipFromPeak))
  runJDindex = min(which(runIDs==(runIndex)))
  
  return(df$day[min(firstIndexRaw,runJDindex)])
}


moth <- read.table('data/moth-abundance.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
  filter(site=='Blue Heron')
coweeta<- read.table('data/coweeta_cats.txt',header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE)%>%
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


#Organizing data based on lunar phases

dfs <- list()
for(i in 2010:2019){
  moth_lunar<-moth%>%
    filter(year==i)%>%
    mutate(New.moon=(days.past.new.moon==0))%>%
    mutate(New.moon=replace(New.moon,New.moon==FALSE,0))%>%
    group_by(year,Lunar.Cycle=cumsum(New.moon)+1)%>%
    mutate(Lunar.Days=row_number())%>%
    dplyr::select(c(days.past.new.moon,New.moon,Lunar.Cycle,Lunar.Days, year))
  dfs[[i]]<-moth_lunar
}
final_lunar<-bind_rows(dfs)
moth_lunar<-bind_cols(moth,final_lunar)


moth_set<-moth_lunar%>%
  group_by(year,Lunar.Cycle)%>%
  mutate(Lunar.Total=sum(photos),Phase.days=n(),Frac=photos/((Lunar.Total/Phase.days)),
         Lunar.avg=Lunar.Total/Phase.days)%>%
  replace_na(list(Frac=0))

#Don't think I need nonzerodays, because if Pick surveyed it every day, then all the zeros that show up can be considered true zeros, and part of the data. So I'm going to take it out and see what changes
postNmoon<-moth_set%>%
  group_by(year,Lunar.Cycle)%>%
  filter(Lunar.Days<=14)%>%
  mutate(RawCount=sum(photos))%>%
  mutate(nLunarDays=n())%>%
  mutate(phototaken=photos/nLunarDays)%>%
  mutate(Phase="PostNewMoon")
 

preNmoon<-moth_set%>%
  group_by(year,Lunar.Cycle)%>%
  filter(Lunar.Days>14)%>%
  mutate(RawCount=sum(photos))%>%
  mutate(nLunarDays=n())%>%
  mutate(phototaken=photos/nLunarDays)%>%
  mutate(Phase="PreNewMoon")


lunar_phase_bind<-bind_rows(postNmoon,preNmoon)




plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}



Gauss<-lunar_phase_bind%>%
  group_by(year, Lunar.Cycle , Phase , nLunarDays)%>%
  mutate(day = median(julian.day))%>%
  group_by(year, day)%>%
  mutate(avg = RawCount/nLunarDays)%>%
  group_by(year, day, Phase, avg)%>%
  summarize()%>%
  mutate_cond(is.na(avg), avg = 0)

GMM<-lunar_phase_bind%>%
  group_by(year, Lunar.Cycle , Phase , nLunarDays)%>%
  mutate(day = median(julian.day))%>%
  group_by(year, day)%>%
  summarise(moths=sum(photos))

df<-as.data.frame(lapply(GMM, rep, GMM$moths))
hist(mixturefilt$day, breaks=fit$day)
hist(fit$day,breaks=fit$day, freq=FALSE)

par(mfrow=c(1,2))


pdf("Moth_GMM_Plot")
List<-list()
par(mfrow=c(3,3))
for(i in 2010:2018){
  mixturefilt<-df%>%
    filter(year==i)
days<-mixturefilt$day
  fit<-Gauss%>%
    filter(year==i)%>%
    mutate(prepost=ifelse(Phase=="PreNewMoon", 3,4))

  set.seed(1)
  mixmdl<-normalmixEM(days, k=2)
  #summary(mixmdl)
  #mixmdl$loglik
  #plot(mixmdl, which=2)
  List[[i]]<-mixmdl$loglik
  
}

df<-do.call(rbind.data.frame, List[2010:2018])


loglik<-rbindlist(dfs)

dev.off()

hist(mixturefilt$day, breaks=fit$day)
  
days<-mixturefilt$day
  args<-list(...)
  hist()
mixmdl$lambda


mixturefilt<-df%>%
  filter(year==2010)

set.seed(1)
days<-mixturefilt$day
mixmdl<-normalmixEM(days, k=2)
summary(mixmdl)
plot(mixmdl, which=2)







par(mfrow=c(3,3))
cont<-NULL
for(i in 2010:2018){
  fit<-Gauss%>%
    filter(year==i)%>%
    mutate(prepost=ifelse(Phase=="PreNewMoon", 3,4))
  
  gfit1 = fitG(x = fit$day,y = fit$avg,mu = weighted.mean(fit$day,fit$avg),sig=10000,scale=100,control=list(maxit=10000),method="L-BFGS-B",lower=c(0,0,0,0,0,0))
  p=gfit1$par
  r2=cor(fit$day,p[3]*dnorm(fit$day,p[1],p[2]))^2
  totalAvg=sum(fit$avg)
  
  gaussplot<-plot(x = fit$day, y = fit$avg, xlab = "Julian Day", ylab = "Mean Density", col=fit$prepost, pch=16,main=i)
  lines(0:365,p[3]*dnorm(0:365,p[1],p[2]),col='blue')
  
  altpheno<-lunar_phase_bind%>%
    filter(year==i)
  
  moth_sum<- cumsum(altpheno$photos)
  ten<- min(which(moth_sum>(0.1*sum(altpheno$photos))))
  fifty<- min(which(moth_sum>(0.5*sum(altpheno$photos))))
  halfcycle<- min(which(fit$avg>0.5*max(fit$avg))) #For true max or local maximum?
  #halfcycle<-min(which(moth_sum>0.5*max(altpheno$photos)))
  abline(v = ten, col="red", lwd=3, lty=2)
  abline(v = fifty, col="blue", lwd=3, lty=2)
  abline(v = fit[halfcycle,2], col="green", lwd=4, lty=2)
  half<- fit[halfcycle,2]
  
  max1<- locmax(fit,dipFromPeak = 0.2)
  peakfit<- Gauss%>%
    filter(year==i,day>200)%>%
    mutate(prepost=ifelse(Phase=="PreNewMoon", 3,4))
  max2<- locmax(peakfit,dipFromPeak=0.2)
  abline(v=max1,col="black",lwd=3,lty=2)
  abline(v=max2,col="yellow",lwd=3,lty=2)    
  foo<- bind_cols(list(max1, max2, ten, fifty,half$day))
  foo$Year=i
  cont[[i]]=foo
}    

#Create df of phenometrics to use for correlation matrix
moth_pheno<-bind_rows(cont)
names(moth_pheno)<-c("Moth_Peak_1", "Moth_Peak_2", "Moth_10%", "Moth_50", "Moth_Half_Peak","Year")

write.table(moth_pheno, "moth_pheno.txt", sep='\t',row.names=F)
title("Mean Density of Moths over Lunar Phases",outer=TRUE,line=-1)
legend(280,300,legend=c("Pre New Moon","Post New Moon","10%","50%", "Half of Max", "Peak 1", "Peak 2"),pch=c(1,1,NA,NA,NA,NA,NA),lty=c(NA,NA,2,2,2,2,2),col=c(3,4,2,4,3, 1, 7),title="Legend", xpd=NA,cex=.9)



#Plot Frac. of avg for lunar days across lunar cycles 
#ISSUE: Has kinks in the fit, not sure why they pop up but there should be a smooth curve for it
#Possible issue with lines(predict(quad),)
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
title("Fraction of Surveys with Moths across Lunar Cycles",outer=TRUE,line=-1)




#Shifting each year's observations to account for lunar cycle discrepancy
day1 = moth %>%
  filter(julian.day == 1) %>%
  select(year, days.past.new.moon)  %>%
  mutate(shift = days.past.new.moon - days.past.new.moon[1])

moth$julian.day.shifted = NA

for (y in 2010:2018) {
  moth$julian.day.shifted[moth$year == y] = moth$julian.day[moth$year == y] - day1$shift[day1$year == y]
}



#mutate(Lunar.Phase1=Lunar.Days<=14, Lunar.Phase2=Lunar.Days>14)%>%
# mutate(Lunar.Phase1=replace(Lunar.Phase1,Lunar.Phase1==TRUE,1))%>%
#group_by(Lunar.Cycle, Lunar.Phase1)%>%
#mutate(id=seq_along())

#  group_by(year,PostNewMoon=cumsum(Lunar.Phase1)+1)%>%
# mutate(Lunar.Days=row_number())

#Phenometrics 
#Extract date where x-th percentile of moths were observed. 
#So we need to know the total number of moths in a year observed
#Then, we can just use an if statement, or a case-when statement that looks at when the percentage
#(sum of moths at that date/sum of total moths>10% or 50%, whatever)
#Also, to look at half of the maximum of the half-cycle value, 
#so basically first you have to know what the peak is for that cycle, then do an if statement for the date with # of moths that first exceeds that). 

#for(i in 2010:2018){
#  altpheno<-lunar_phase_bind%>%
#    filter(year==i)
#  moth_sum<-cumsum(altpheno$photos)
#  ten<-min(which(moth_sum>(0.1*sum(altpheno$photos))))
#  fifty<-min(which(moth_sum>(0.5*sum(altpheno$photos))))
#  halfcycle<-min(which(altpheno$photos>0.5*max(altpheno$photos)))
  
#}

#Finding first local maximum
#we use the raw dataset(which would be lunar_phase_bind I think) and photos=freq
#Dip angle needs to be tested, starting with 0.1 first
#  locmax=function(df, dipFromPeak=0.1){
#    photoDiff=diff(df$avg)
#    diffRelativeToMax=photoDiff/max(df$avg,na.rm=TRUE)
#    firstIndexRaw=min(which(diffRelativeToMax< -dipFromPeak))
    
#    runs=rle(sign(diffRelativeToMax))
#    runIDs=rep(1:length(runs$lengths),runs$lengths)
#    runSum=sapply(1:length(runs$lengths), function(x)
#    sum(diffRelativeToMax[runIDs==x]))
#    runIndex=min(which(runSum< -dipFromPeak))
#    runJDindex=min(which(runIDs==(runIndex)))
    
#    return(df$day[min(firstIndexRaw,runJDindex)])
#  }
  
  
#  for(i in 2010:2018){
#    filt<-Gauss%>%
#      filter(year==2011)
#    locmax(filt,dipFromPeak = 0.2)
    
#  }
  
  
#  filt<-lunar_phase_bind%>%
#    filter(year==2011)
#plot(x=filt$julian.day,y=filt$photos)

#  locmax(lunar_phase_bind,dip=0.1)  
  



