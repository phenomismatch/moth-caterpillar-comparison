
source('code/formatting_coweeta.r')
source('C:/git/caterpillars-analysis-public/code/analysis_functions.r')
source('C:/git/moth-caterpillar-comparison/code/Coweeta_threshold_testing.r')


RawCow <- read.csv("C:/git/moth-caterpillar-comparison/data/coweeta_phenosummary.csv", header=TRUE)

PhenSum<-RawCow%>%
  filter(Year>2009, Name != "Coweeta - RK")%>%
  pivot_wider(names_from=Name,
              values_from=pctPeakDate:massRollingPeakDateWindow)%>%
  select()

site_filter<-function(field_year,field_plot,threshold_value){
  filter<-cow_filter(field_year,field_plot,threshold_value)
  fil<-cow_fil(field_year,field_plot,filter)
  cow_surv<-coweeta_surveys(fil)
  cow_arth<-coweeta_arths(fil,cow_surv)
  merged<-merge_fun(cow_surv,cow_arth)
  date<-date_change(merged)
}



par(mfrow=c(3,3))
for (i in 2010:2018){
  Tree<-site_filter(i, "BB", 50)%>%
  group_by(PlantSpecies,julianweek)%>%
   summarize(count=n())%>%
  spread(PlantSpecies, count)
  plot(x=Tree$julianweek, y=Tree$`American chestnut`,xlab="Julian Week", ylab="nSurveys", type="b",ylim=c(0,100), bty='L',main=i)
  points(x=Tree$julianweek, y=Tree$`Red maple`, col="red", type ="b")
  points(x=Tree$julianweek, y=Tree$`Red oak`, col="blue", type="b")
  points(x=Tree$julianweek, y=Tree$`Striped maple`, col="green", type= "b")
  #legend(140,100,legend=c("American Chestnut", "Red Maple", "Red Oak", "Striped Maple"), col=c("black", "red", "blue", "green"), lty=1, cex=1, pt.cex=1) 
}

#par(mfrow=c(3,2))
#for (i in 2010:2015){
#Tree<-site_filter(i, "BS", 50)%>%
#  group_by(PlantSpecies,julianweek)%>%
#  summarize(count=n())  
#plot(x=Tree$julianweek,y=Tree$count, xlab="Julian Week", ylab="nSurveys", type="b")
#}

#Plotting meanDensityByWeek actually
par(mfrow=c(3,3))

cow_plots<-for (i in 2010:2018){
  tryCatch({
    meanDensityByWeek(surveyData = site_filter(i,"BS",50),plot = TRUE,new=TRUE, xlab="Julian Week", ylab="Mean Density",plotVar = 'meanDensity', main=i,sub="BS")
    meanDensityByWeek(surveyData = site_filter(i,"BB",50),plot = TRUE,new=TRUE,xlab="Julian Week", ylab="Mean Density",plotVar = 'meanDensity',main=i,sub="BB")
    
    },error=function(e){}
  )
}
title("Mean Density of Caterpillars by Week For Thresholds 50",outer=TRUE,line=-1)
#legend("topleft",legend=c("Threshold of 100","Threshold of 140"),lty=c(1,2),col=c(1,4),title="Legend", xpd=NA,cex=0.5,pt.cex=2)

#Plot fracSurveys
par(mfrow=c(3,3))
cow_plots<-for (i in 2010:2018){
  tryCatch({
    meanDensityByWeek(surveyData = site_filter(i,"BS",50),plot = TRUE,new=TRUE, xlab="Julian Week", ylab="Frac. Surveys",plotVar = 'fracSurveys', main=i,sub="BS")
    meanDensityByWeek(surveyData = site_filter(i,"BB",50),plot = TRUE,new=TRUE,xlab="Julian Week", ylab="Frac.Survey",plotVar = 'fracSurveys',main=i,sub="BB")
    
  },error=function(e){}
  )
}


test<-meanDensityByWeek(surveyData = site_filter(2010,"BS",50),plot = TRUE,new=TRUE, xlab="Julian Week", ylab="Mean Density", ylim=c(0,40),main=2010)

#Proportion of Surveys with caterpillars for each julian day for a given julian week
#Set Threshold for all years to 100, then find proportion of surveys-nSurveys for each day/Total nSurveys in that given julian week
par(mfrow=c(3,3))
list<-c("BB","BS")
#Plot%in% c("BB","BS")
for(j in list){
  for (i in 2010:2018){
    cow_thresh <- cowplusnotes %>%
      filter(Year == i, Plot== j, TreeSpecies %in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
      select(Year, Yearday, Plot, Point, TreeSpecies, Sample, NumCaterpillars)%>%
      distinct()%>%
      mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
      group_by(Year,JulianWeek)%>%
      summarize(nJulianWeekSurvey = n_distinct(TreeSpecies, Point, Sample)) %>% 
      filter(nJulianWeekSurvey>50)%>%
      mutate(NSurveywithCat=replace(NumCaterpillars,NumCaterpillars>1,1))%>%
      replace_na(list(NSurveywithCat=0))%>%
      group_by(Year,Yearday,JulianWeek)%>%
      mutate(NSurv=sum(NSurveywithCat))%>%
      mutate(PropSurv=NSurv/nJulianWeekSurvey)%>%
      group_by(Year,Yearday,JulianWeek,nJulianWeekSurvey,PropSurv)%>%
      summarize()
      
    
    plot(x=cow_thresh$Yearday,y=cow_thresh$PropSurv, main=i,sub=j, xlab="Yearday", ylab="Proportion of Surveys", type="l")
    
  }
  
}
title("Proportion of surveys with Caterpillars for a given julian day",outer=TRUE,line=-1)


#Just proportion of surveys for each day
cow_thresh<-cowplusnotes%>%
  filter(Year==i, Plot==j, TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  select(Year,Yearday,Plot,Point,TreeSpecies,Sample,NumCaterpillars)%>%
  distinct()%>%
  group_by(Year,Yearday)%>%
  tally()%>%
  rename(nSurveys=n)%>%
  mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
  #aggregate(cow_thresh$nSurveys,by=list(Year=cow_thresh$Year,cow_thresh$JulianWeek=JWeek),FUN=sum)
  group_by(Year,JulianWeek)%>%
  mutate(nJulianWeekSurvey=sum(nSurveys))%>%
  filter(nJulianWeekSurvey>50)%>%
  group_by(Year,Yearday)%>%
  mutate(PropSurv=nSurveys/nJulianWeekSurvey)
# group_by(Year)%>%
#  add_count()%>%
#rename(nWeeks=n)
plot(x=cow_thresh$Yearday,y=cow_thresh$PropSurv, main=i,sub=j, xlab="Yearday", ylab="Proportion of Surveys", type="l")



#Start plotting Phenology and alternative phenometrics 
cow_thresh<-cowplusnotes%>%
  filter(Year>2009, Plot%in% c("BB","BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  select(Year,Yearday,Plot,Point,TreeSpecies,Sample, NumCaterpillars)%>%
  distinct()%>%
  group_by(Year,Yearday)%>%
  tally()%>%
  rename(nSurveys=n)%>%
  mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
  #aggregate(cow_thresh$nSurveys,by=list(Year=cow_thresh$Year,cow_thresh$JulianWeek=JWeek),FUN=sum)
  group_by(Year,JulianWeek)%>%
  mutate(nJulianWeekSurvey=sum(nSurveys))%>%
  filter(nJulianWeekSurvey>50)

cow_pheno<-left_join(cowplusnotes,cow_thresh,by=c("Year","Yearday"))


#Plotting coweeta data as mean caterpillars for each julian week. 
list<-c("BB","BS")
par(mfrow=c(3,3))

for(j in list){
  for(i in 2010:2018){
    cow_phen<-cow_pheno%>%
      filter(Plot==j,Year==i)%>%
      replace_na(list(JulianWeek=0))%>%
      filter(JulianWeek!=0)%>%
      group_by(Year,Yearday)%>%
      summarize(catcount=sum(NumCaterpillars))%>%
      mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
      group_by(Year,JulianWeek)%>%
      mutate(nDay=n())%>%
      mutate(catweekcount=sum(catcount))%>%
      mutate(avg=catweekcount/nDay)%>%
      group_by(Year,JulianWeek, avg)%>%
      summarize()%>%
      mutate_cond(is.na(avg), avg = 0)
    
    fit<-cow_phen%>%
      filter(Year==i)
    
    gfit1=fitG(x=fit$JulianWeek,y=fit$avg,mu=weighted.mean(fit$JulianWeek,fit$avg),sig=10,scale=150,control=list(maxit=10000),method="L-BFGS-B",lower=c(0,0,0,0,0,0))
    p=gfit1$par
    r2=cor(fit$JulianWeek,p[3]*dnorm(fit$JulianWeek,p[1],p[2]))^2
    totalAvg=sum(fit$avg)
    
    plot(x=fit$JulianWeek,y=fit$avg,main=i, sub=j,type="l",xlab="JulianWeek", ylab="Average Caterpillars")
    #lines(0:365,p[3]*dnorm(0:365,p[1],p[2]),col='blue')
    altpheno<-cow_pheno%>%
      filter(Year==i)
    catsum<-cumsum(altpheno$NumCaterpillars)
    # ten<-min(which(catsum>(0.1*sum(altpheno$photos))))
    #  fifty<-min(which(catsum>(0.5*sum(altpheno$photos))))
    #  halfcycle<-min(which(fit$avg>0.5*max(fit$avg)))
    #  abline(v = fit[ten,2], col="red", lwd=3, lty=2)
    #  abline(v = fit[fifty,2], col="blue", lwd=3, lty=2)
    #  abline(v = fit[halfcycle,2], col="green", lwd=4, lty=2)
    
    
    
  }
  
}
title("Average Caterpillars for each Julian Week surveyed",outer=TRUE,line=-1)

#Quadratic fit?
quadmod<-moth_set%>%
  filter(year==y)
Lunar2=quadmod$Lunar.Days^2
quad<-lm(quadmod$Frac~quadmod$Lunar.Days+Lunar2)
square<-summary(quad)$r.squared
plot(main=y,x=moth_plot$Lunar.Days,y=moth_plot$Frac,type="l",
     col=rainbowcols[1],xlab="Lunar Days", ylab="Frac of Surveys")
lines(predict(quad),)
legend("topleft",bty="n",legend=paste("R^2=",square))



plot(x=cow_pheno$Yearday,y=cow_pheno$NumCaterpillars)


fitG = function(x, y, mu, sig, scale, ...){
  
  f = function(p){
    
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
  
}



# Write files
#write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)


# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)
