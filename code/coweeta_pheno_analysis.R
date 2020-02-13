source('code/formatting_coweeta.r')
source('code/Coweeta_threshold_testing.r')

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(raster)
library(rgdal)
library(dggridR)
library(sf)
library(corrplot)
library(gridExtra)

cow_dat<-read.table("data/Coweeta_Filtered.txt",header=TRUE)
moth_pheno<-read.table("data/moth_pheno.txt",header=TRUE)
cow_pheno_sum<-read.table("data/Coweeta_Phenometrics.txt", header=TRUE)

# Visualize basic summaries of survey effort
cow_survs = final_cow_set %>% distinct(LocalDate, Year, PlantFK, PlantSpecies, julianday, julianweek, Plot)
survs_by_plotyear = data.frame(table(cow_survs[, c('Year', 'Plot')]))
plot(survs_by_plotyear$Year[survs_by_plotyear$Plot == 'BB'], survs_by_plotyear$Freq[survs_by_plotyear$Plot == 'BB'], type = 'l', lwd = 3, col = 'darkred')
points(survs_by_plotyear$Year[survs_by_plotyear$Plot == 'BS'], survs_by_plotyear$Freq[survs_by_plotyear$Plot == 'BS'], type = 'l', lwd = 3, col = 'darkblue')


#Convert cow_pheno_sum using pivot_wider to get phenometrics for both sites as columns
Phen_BB<-cow_pheno_sum%>%
  filter(Year>2009, Name == "Coweeta_BB")%>%
  pivot_wider(names_from=Name,
              values_from=medianGreenup:massRollingPeakDateWindow)%>%
  mutate_if(is.integer,replace_na,0)%>% 
  dplyr::select(c(Year,pctPeakDate_Coweeta_BB,massPeakDate_Coweeta_BB,pctRollingPeakDateWindow_Coweeta_BB, massRollingPeakDateWindow_Coweeta_BB))%>%
  setnames(old=c( "pctPeakDate_Coweeta_BB", "massPeakDate_Coweeta_BB", "pctRollingPeakDateWindow_Coweeta_BB", "massRollingPeakDateWindow_Coweeta_BB"), new=c( "pct_peak_BB", "mass_peak_BB","pctRolling_BB", "massRolling_BB"))

Phen_BS<-cow_pheno_sum%>%
  filter(Year>2009, Name == "Coweeta_BS")%>%
  pivot_wider(names_from=Name,
              values_from=medianGreenup:massRollingPeakDateWindow)%>%
  mutate_if(is.integer,replace_na,0)%>%
  dplyr::select(c(Year,pctPeakDate_Coweeta_BS,massPeakDate_Coweeta_BS,pctRollingPeakDateWindow_Coweeta_BS, massRollingPeakDateWindow_Coweeta_BS))%>%
  setnames(old=c( "pctPeakDate_Coweeta_BS", "massPeakDate_Coweeta_BS", "pctRollingPeakDateWindow_Coweeta_BS", "massRollingPeakDateWindow_Coweeta_BS"), new=c( "pct_peak_BS", "mass_peak_BS","pctRolling_BS", "massRolling_BS"))



BB_BS<-merge(Phen_BS, Phen_BB, by="Year")
Phen_Final<-merge(BB_BS,moth_pheno,by="Year")
 
#Correlation Matrix
phen_mat<-round((cor(Phen_Final[2:14], use = 'pairwise.complete.obs' )), 2)

#Quick visualization of correlation matrix
pdf("Correlation_Matrix")
corrplot(phen_mat,type="upper",tl.col="black", tl.srt=45)
dev.off()

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


## NESTED FOR LOOP ACROSS SITE, PLOTVAR (meanBiomass, fracSurveys), AND YEAR


#Plots
pdf("Coweeta_Mean_Density_Plots")
par(mfrow=c(3,3))
for (var in c("meanBiomass", "fracSurveys")){
  for(j in c("BB", "BS")){
    for ( i in 2010:2018){
      cow_filt<-cow_dat%>%
        filter(Year==i, Plot==j)%>%
        mutate(surveys=n())
      foo<-Phen_Final%>%
        filter(Year==i)%>%
        dplyr::select(contains(j))
        
      table<-meanDensityByWeek(surveyData=cow_filt, plot=TRUE, plotVar=var, xlab="Julian Week", ylab= var, main = paste(i,j,"# Surveys =", cow_filt$surveys[1]) )
      abline(v = foo[1], col="yellow", lwd=5, lty=2)
      abline(v = foo[2], col="red", lwd=4, lty=2)
      abline(v = foo[3], col="blue", lwd=3, lty=2)
      abline(v = foo[4], col="green", lwd=2, lty=2)    
  
    }
    
  }
}
legend("bottomright",legend=c("Pct_Peak","Mass_Peak", "Pct_Rolling", "Mass_Rolling"),lty=c(4,3,2, 5),col=c(7,2,4,3),title="Legend", xpd=NA,cex=.9)

dev.off()


#Overlaid plots, still need to do it for meanBiomass, so far just for fracsurveys
par(mfrow=c(3,3))
for (var in c( "fracSurveys")){
    for ( i in 2010:2018){
      cow_filtBB<-cow_dat%>%
        filter(Year==i, Plot=="BB")%>%
        mutate(surveys=n())
      cow_filtBS<-cow_dat%>%
        filter(Year==i, Plot=="BS")%>%
        mutate(surveys=n())
    
      fooBB<-Phen_Final%>%
        filter(Year==i)%>%
        dplyr::select(contains("BB"))
      fooBS<-Phen_Final%>%
        filter(Year==i)%>%
        dplyr::select(contains("BS"))
      
      
      plot1<-meanDensityByWeek(surveyData=cow_filtBB, plot=FALSE, plotVar=var, xlab="Julian Week", ylab= var, main = paste(i,"BB","# Surveys =", cow_filtBB$surveys[1]))
      plot2<-meanDensityByWeek(surveyData=cow_filtBS, plot=FALSE, plotVar=var, xlab="Julian Week", ylab= var, main = paste(i,"BS","# Surveys =", cow_filtBS$surveys[1]))
      
      plot(x=plot1$julianweek, y=plot1$fracSurveys, ylab=var, type="b", main = paste(i,"BB=", cow_filtBB$surveys[1], "BS=", cow_filtBS$surveys[1])) 
      lines(x=plot2$julianweek, y=plot2$fracSurveys, type="b", col="blue")
      
    }
  
}
legend("bottomright",legend=c("BB", "BS"),lty=c(1),col=c(1,4),title="Legend", xpd=NA,cex=.9)


par(mfrow=c(3,3))
for (var in c( "meanBiomass")){
  for ( i in 2010:2018){
    cow_filtBB<-cow_dat%>%
      filter(Year==i, Plot=="BB")%>%
      mutate(surveys=n())
    cow_filtBS<-cow_dat%>%
      filter(Year==i, Plot=="BS")%>%
      mutate(surveys=n())
    
    fooBB<-Phen_Final%>%
      filter(Year==i)%>%
      dplyr::select(contains("BB"))
    fooBS<-Phen_Final%>%
      filter(Year==i)%>%
      dplyr::select(contains("BS"))
    
    
    plot1<-meanDensityByWeek(surveyData=cow_filtBB, plot=FALSE, plotVar=var, xlab="Julian Week", ylab= var, main = paste(i,"BB","# Surveys =", cow_filtBB$surveys[1]))
    plot2<-meanDensityByWeek(surveyData=cow_filtBS, plot=FALSE, plotVar=var, xlab="Julian Week", ylab= var, main = paste(i,"BS","# Surveys =", cow_filtBS$surveys[1]))
    
    plot(x=plot1$julianweek, y=plot1$meanBiomass, ylab=var, type="b", main = paste(i,"BB=", cow_filtBB$surveys[1], "BS=", cow_filtBS$surveys[1])) 
    lines(x=plot2$julianweek, y=plot2$meanBiomass, type="b", col="blue")
    
  }
  
}
legend("bottomright",legend=c("BB", "BS"),lty=c(1),col=c(1,4),title="Legend", xpd=NA,cex=.9)

#Plot of Coweeta data survey data across all years for tree species 
trees<-cow_dat%>%
  group_by(PlantSpecies, Plot)%>%
  summarise(surveys=n())
  
barplot(height=trees$surveys,main="Tree Species Survey Efforts",xlab="Tree Species", ylab="Surveys",col=c("lightblue","darkgreen"),legend=c("BB", "BS"), las=1, names.arg=trees$PlantSpecies)

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

pdf("Coweeta_Bivariate_Analysis_Plots")

plot(x=Phen_Final$Moth_Half_Peak, y=Phen_Final$mass_peak_BS, main="Mean Biomass Peak BS vs. Moth Half Peak Date", xlab="Moth Half Peak (Julian Day)", ylab="Mean Biomass Peak BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$mass_peak_BS~Phen_Final$Moth_Half_Peak)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Half_Peak"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Half_Peak, y=Phen_Final$massRolling_BS, main="Mean Biomass rolling window BS vs. Moth Half Peak Date", xlab="Moth Half Peak (Julian Day)", ylab="Mean Biomass Rolling Window BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$massRolling_BS~Phen_Final$Moth_Half_Peak)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Half_Peak"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Half_Peak, y=Phen_Final$mass_peak_BB, main="Mean Biomass Peak BB vs. Moth Half Peak Date", xlab="Moth Half Peak (Julian Day)", ylab="Mean Biomass Peak BB (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$mass_peak_BB~Phen_Final$Moth_Half_Peak)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Half_Peak"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Half_Peak, y=Phen_Final$massRolling_BB,main="Mean Biomass (rolling) BB vs. Moth Half Peak Date", xlab="Moth Half Peak (Julian Day)", ylab="Mean Biomass Rolling Window BB (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$massRolling_BB~Phen_Final$Moth_Half_Peak)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Half_Peak"])
legend("topleft",bty="n",legend=paste("R^2=",square))


plot(x=Phen_Final$Moth_Peak_1, y=Phen_Final$mass_peak_BS, main="Mean Biomass Peak BS vs. First Moth Peak Date", xlab="Moth Peak (Julian Day)", ylab="Mean Biomass Peak BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$mass_peak_BS~Phen_Final$Moth_Peak_1)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Peak_1"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Peak_1, y=Phen_Final$massRolling_BS, main="Mean Biomass rolling window BS vs. First Moth Peak Date", xlab="Moth Peak (Julian Day)", ylab="Mean Biomass Rolling Window BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$massRolling_BS~Phen_Final$Moth_Peak_1)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Peak_1"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Peak_1, y=Phen_Final$mass_peak_BB, main="Mean Biomass Peak BB vs. First Moth Peak Date", xlab="Moth Peak (Julian Day)", ylab="Mean Biomass Peak BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$mass_peak_BB~Phen_Final$Moth_Peak_1)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Peak_1"])
legend("topleft",bty="n",legend=paste("R^2=",square))

plot(x=Phen_Final$Moth_Peak_1, y=Phen_Final$massRolling_BB,main="Mean Biomass (rolling) BB vs. First Moth Peak Date", xlab="Moth Peak (Julian Day)", ylab="Mean Biomass Rolling Window BS (Julian Day)", pch=16)
plotfit<-lm(Phen_Final$massRolling_BB~Phen_Final$Moth_Peak_1)
square<-summary(plotfit)$r.squared
abline(coef(plotfit)["(Intercept)"], coef(plotfit)["Phen_Final$Moth_Peak_1"])
legend("topleft",bty="n",legend=paste("R^2=",square))

dev.off()

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



