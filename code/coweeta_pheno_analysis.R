source('code/analysis_functions.r')
source('code/formatting_coweeta.r')
source('code/Coweeta_threshold_testing.r')
source('code/moth_abundance.R')
library(corrplot)
library(gridExtra)

cow_pheno_sum <- read.csv("data/coweeta_phenosummary.csv", header=TRUE)


#Convert cow_pheno_sum using pivot_wider to get phenometrics for both sites as columns
Phen_BB<-cow_pheno_sum%>%
  filter(Year>2009, Name == "Coweeta - BB")%>%
  pivot_wider(names_from=Name,
              values_from=medianGreenup:massRollingPeakDateWindow)%>%
  mutate_if(is.integer,replace_na,0)%>% 
  setnames(old=c( "pctPeakDate_Coweeta - BB", "massPeakDate_Coweeta - BB", "pctRollingPeakDateWindow_Coweeta - BB", "massRollingPeakDateWindow_Coweeta - BB"), new=c( "pct_peak_BB", "mass_peak_BB","pctRolling_BB", "massRolling_BB"))%>%
  dplyr::select(c(Year, pct_peak_BB, mass_peak_BB,pctRolling_BB,massRolling_BB))
  
Phen_BS<-cow_pheno_sum%>%
  filter(Year>2009, Name == "Coweeta - BS")%>%
  pivot_wider(names_from=Name,
              values_from=medianGreenup:massRollingPeakDateWindow)%>%
  mutate_if(is.integer,replace_na,0)%>%
  setnames(old=c( "pctPeakDate_Coweeta - BS", "massPeakDate_Coweeta - BS","pctRollingPeakDateWindow_Coweeta - BS", "massRollingPeakDateWindow_Coweeta - BS"), new=c( "pct_peak_BS", "mass_peak_BS","pctRolling_BS", "massRolling_BS"))%>%
  dplyr::select(c(Year, pct_peak_BS, mass_peak_BS, pctRolling_BS,massRolling_BS))

BB_BS<-merge(Phen_BS, Phen_BB, by="Year")
Phen_Final<-merge(BB_BS,moth_pheno,by="Year")
 
#Correlation Matrix
phen_mat<-round((cor(Phen_Final[2:14], use = 'pairwise.complete.obs' )), 2)

#Quick visualization of correlation matrix
corrplot(phen_mat,type="upper",tl.col="black", tl.srt=45)


par(mfrow=c(3,3))
coweeta<-for ( i in 2010:2018){
  year_filt<-cow_pheno%>%
    filter(Year==i, Plot=="BB")%>%
    group_by(Yearday)%>% 
    summarize(raw=sum(NumCaterpillars))
  plot(x=year_filt$Yearday,y=year_filt$raw, xlab="Yearday", ylab="raw", main=c(i,"BB"))
  foo<-Phen_Final%>%
    filter(Year==i)
  
  abline(v = foo$mass_peak_BB, col="red", lwd=5, lty=2)
  abline(v = foo$massRolling_BB, col="blue", lwd=4, lty=2)
  abline(v = foo$pctRolling_BB, col="green", lwd=3, lty=2)
  abline(v = foo$pct_peak_BB, col="yellow", lwd=2, lty=2)
  
  # abline(v = foo$massRolling_BS, col="yellow", lwd=3, lty=2)
}
legend(100,30,legend=c("Mass_Peak","Mass_Rolling", "Pct_Rolling"),lty=c(4,3,2),col=c(2,4,3),title="Legend", xpd=NA,cex=.9)

par(mfrow=c(3,3))
coweeta<-for ( i in 2010:2018){
  year_filt<-cow_pheno%>%
    filter(Year==i, Plot=="BS")%>%
    group_by(Yearday)%>%
    summarize(raw=sum(NumCaterpillars))
  
  
  plot(x=year_filt$Yearday,y=year_filt$raw, xlab="Yearday", ylab="raw", main=c(i,j))
  foo<-Phen_Final%>%
    filter(Year==i)
  abline(v = foo$mass_peak_BS, col="red", lwd=5, lty=2)
  abline(v = foo$massRolling_BS, col="blue", lwd=4, lty=2)
  abline(v = foo$pctRolling_BS, col="green", lwd=3, lty=2)
  abline(v = foo$pct_peak_BS, col="yellow", lwd=2, lty=2)
}
legend(100,30,legend=c("Mass_Peak","Mass_Rolling", "Pct_Rolling"),lty=c(5,4,3),col=c(2,4,3),title="Legend", xpd=NA,cex=.9)





#Plots of Mean Density 
par(mfrow=c(3,3))
cow_plots<-for (i in 2010:2018){
  cow_filt<-final_cow_set%>%
    filter(Year==i,Name=="Coweeta_BB")
  foo<-Phen_Final%>%
    filter(Year==i)
  
  
  table<-meanDensityByWeek(surveyData = cow_filt, plot=TRUE,plotVar = 'meanDensity',xlab="Julian Week", ylab="Mean Density", main=c(i,"BB"))
  abline(v = foo$pct_peak_BB, col="yellow", lwd=5, lty=2)
  abline(v = foo$mass_peak_BB, col="red", lwd=4, lty=2)
  abline(v = foo$massRolling_BB, col="blue", lwd=3, lty=2)
  abline(v = foo$pctRolling_BB, col="green", lwd=2, lty=2)
  # abline(v=foo$Moth_50,col="yellow",lwd=5,lty=2)
  
}
#legend(x = 20, y=50, legend = c("Mean BioMass Peak Date", "BioMass Rolling Window"),lty=c(2,2), col=c(2,4))
legend("bottomright",legend=c("Mass_Peak","Mass_Rolling", "Pct_Rolling", "Pct_Peak"),lty=c(4,3,2, 5),col=c(2,4,3,7),title="Legend", xpd=NA,cex=.9)


par(mfrow=c(3,3))
cow_plots<-for (i in 2010:2018){
  cow_filt<-final_cow_set%>%
    filter(Year==i,Name=="Coweeta_BS")
  foo<-Phen_Final%>%
    filter(Year==i)
  
  table<-meanDensityByWeek(surveyData = cow_filt, plot=TRUE,plotVar = 'meanDensity',xlab="Julian Week", ylab="Mean Density", main=c(i,"BS"))
  abline(v = foo$pct_peak_BS, col="yellow", lwd=5, lty=2) 
  abline(v = foo$mass_peak_BS, col="red", lwd=4, lty=2)
  abline(v = foo$massRolling_BS, col="blue", lwd=3, lty=2)
  abline(v = foo$pctRolling_BS, col="green", lwd=2, lty=2)
}
legend("bottomright",legend=c("Mass_Peak","Mass_Rolling", "Pct_Rolling", "Pct_Peak"),lty=c(4,3,2, 5),col=c(2,4,3, 7),title="Legend", xpd=NA,cex=.9)



#Example Plot of Mean Density of Caterpillars for BB 2017
par(mfrow=c(3,3))
cow_plots<-for (i in 2010:2018){
  cow_filt<-final_cow_set%>%
    filter(Year==i,Name=="Coweeta_BB")
  foo<-Phen_Final%>%
    filter(Year==i)
  
table<-meanDensityByWeek(surveyData = cow_filt, plot=TRUE,plotVar = 'totalCount',xlab="Julian Week", ylab="Weekly Counts", main=c(i))
abline(v = foo$mass_peak_BB, col="red", lwd=4, lty=2)
abline(v = foo$massRolling_BB, col="blue", lwd=3, lty=2)

}
legend(x = 165, y=1.6, legend = c("Mean BioMass Peak Date", "BioMass Rolling Window"),lty=c(2,2), col=c(2,4))

cow_plots<-for (i in 2010:2018){
  cow_filt<-final_cow_set%>%
    filter(Year==i,Name=="Coweeta_BS")
  foo<-Phen_Final%>%
    filter(Year==i)
  
  table<-meanDensityByWeek(surveyData = cow_filt, plot=TRUE,plotVar = 'totalCount',xlab="Julian Week", ylab="Weekly Counts", main=c(i))
  abline(v = foo$mass_peak_BS, col="red", lwd=4, lty=2)
  abline(v = foo$massRolling_BS, col="blue", lwd=3, lty=2)
}


#Plots of Selected Phenometrics based on correlation matrix
par(mfrow=c(2,2))

plot(x=Phen_Final$`Moth_10%`, y=Phen_Final$massRolling_BS, main="mean Biomass (rolling) BS vs. Moth 10%", xlab="Moth 10% Date (Julian Day)", ylab="Mean Biomass Rolling Window BS")
plotfit<-lm(Phen_Final$massRolling_BS~Phen_Final$`Moth_10%`)
square<-summary(plotfit)$r.squared
abline(-143.93, 3.94)
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$`Moth_50`, y=Phen_Final$massRolling_BS,main="mean Biomass (rolling) BS vs. Moth 50%  ", xlab="Moth 50% Date (Julian Day)", ylab="Mean Biomass Rolling Window BS")
plotfit<-lm(Phen_Final$massRolling_BS~Phen_Final$`Moth_50`)
square<-summary(plotfit)$r.squared
abline(-635.273,3.645)
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$`Moth_10%`, y=Phen_Final$pct_peak_BS,main="pct Peak Date BS vs. Moth 10%", xlab="Moth 10% Date (Julian Day)", ylab="pct Peak Date BS")
plotfit<-lm(pct_peak_BS~`Moth_10%`, data=Phen_Final)
square<-summary(plotfit)$r.squared
abline(191.26,-0.63)
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Half_Peak, y=Phen_Final$pct_peak_BS. , main = "pct Peak Date BS vs. Moth Half Peak", xlab="Moth Half Peak Date (Julian Day)", ylab="pct Peak Date BS")
plotfit<-lm(pct_peak_BS~Moth_Half_Peak, data=Phen_Final)
square<-summary(plotfit)$r.squared
abline(178.057, -0.2519)
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$`Moth_10%`, y=Phen_Final$massRolling_BS,main = "pctRollingBS vs. Moth 10%", xlab="Moth 10% Date", ylab="pct Peak Date Rolling BS")
lm(Phen_Final$massRolling_BS~Phen_Final$`Moth_10%`)
square<-summary(plotfit)$r.squared
abline(-143.93, 3.94)
legend("topleft",bty="n",legend=paste("R^2=",square))


#par(mfrow=c(3,3))
#for (i in 2010:2018){
#  Tree<-site_filter(i, "BB", 50)%>%
#  group_by(PlantSpecies,julianweek)%>%
#   summarize(count=n())%>%
#  spread(PlantSpecies, count)
#  plot(x=Tree$julianweek, y=Tree$`American chestnut`,xlab="Julian Week", ylab="nSurveys", type="b",ylim=c(0,100), bty='L',main=i)
#  points(x=Tree$julianweek, y=Tree$`Red maple`, col="red", type ="b")
#  points(x=Tree$julianweek, y=Tree$`Red oak`, col="blue", type="b")
#  points(x=Tree$julianweek, y=Tree$`Striped maple`, col="green", type= "b")
#legend(140,100,legend=c("American Chestnut", "Red Maple", "Red Oak", "Striped Maple"), col=c("black", "red", "blue", "green"), lty=1, cex=1, pt.cex=1) 
#}

#par(mfrow=c(3,2))
#for (i in 2010:2015){
#Tree<-site_filter(i, "BS", 50)%>%
#  group_by(PlantSpecies,julianweek)%>%
#  summarize(count=n())  
#plot(x=Tree$julianweek,y=Tree$count, xlab="Julian Week", ylab="nSurveys", type="b")
#}



#Plotting meanDensityByWeek, don't really need this one anymore
par(mfrow=c(3,3))

cow_plots<-for (i in 2010:2018){
  tryCatch({
    meanDensityByWeek(surveyData = final_cow_set,plot = TRUE,new=TRUE, xlab="Julian Week", ylab="Mean Density",plotVar = 'meanDensity', main=i,sub="BS")
    },error=function(e){}
  )
}
title("Mean Density of Caterpillars by Week For Thresholds 50",outer=TRUE,line=-1)
#legend("topleft",legend=c("Threshold of 100","Threshold of 140"),lty=c(1,2),col=c(1,4),title="Legend", xpd=NA,cex=0.5,pt.cex=2)


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


#Create plots of Raw Count of Caterpillar data with the phenometrics imposed on top of them




# Write files
#write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)


# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)
