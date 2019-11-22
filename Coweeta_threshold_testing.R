

source('data/formatting_coweeta.r')
source('C:/git/caterpillars-analysis-public/code/analysis_functions.r')
#Frequency plots and threshold values 


freq_plot<-function(field_year, field_plot){
  group_cow_set<-cowplusnotes%>%
    filter(cowplusnotes$Year==field_year, cowplusnotes$Plot==field_plot)%>%
    select(Plot,Yearday,Point,TreeSpecies,Sample)%>%
    distinct()%>%
    group_by(Point,Plot,TreeSpecies,Sample,Yearday)%>%
    summarise(n())%>%
    mutate(freq=(lead(Yearday)-Yearday), gridLetter = substr(Point, 1, 1),
           gridY = as.numeric(substr(Point, 2, nchar(Point))),
           gridX = which(LETTERS == gridLetter))
  par(mfrow = c(6, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  pdf(paste0("coweeta_plots_",field_year,"_",field_plot,".pdf"))
  for (j in unique(group_cow_set$Yearday)) {
    tmp = filter(group_cow_set, Yearday == j)
    plot(group_cow_set$gridX, group_cow_set$gridY, pch = 16, xlab = "", ylab = "",main="Plot Samples")
  }
dev.off()
  return(group_cow_set)
  
}
freq_plot(2010,"BB")

treesByYear = cowplusnotes %>%
  filter(Plot != "RK") %>%
  select(Year, Plot,Yearday,Point,TreeSpecies,Sample)%>%
  distinct()%>%
  count(Year, TreeSpecies) %>%
  arrange(Year, desc(n)) %>%
  spread(key = TreeSpecies,value = n, fill = 0)


#pdf("coweeta_tree_surveys_by_year.pdf", height = 11, width = 8)
par(mfrow = c(length(unique(treesByYear$Year)), 1))
for (y in unique(treesByYear$Year)) {
  bplot = barplot(as.matrix(treesByYear[treesByYear$Year == y, 4:ncol(treesByYear)]), col = 'darkorchid', xaxt = "n", xlab = "", ylab = "")
}
text(bplot, rep(-1, ncol(treesByYear)-4), xpd = TRUE, srt = 45)
dev.off()





#BBfreq <- NA

#for(i in 2015:2018){
#  g<-freq_plot(i,"BB")
#  print(g)
#}

#Function to filter coweeta data for field year/plot, then get frequency for each day that was sampled
samp_days<-function(field_year,field_plot){
  coweeta_data<-cowplusnotes%>%
    filter(cowplusnotes$Year==field_year,cowplusnotes$Plot==field_plot)%>%
    select(Plot, Yearday, Point,TreeSpecies, Sample)%>%
    distinct()%>%
    group_by(Plot,Point, Yearday)%>%
    summarize(obsv=n())%>%
    arrange(Yearday)%>%
    mutate(gridLetter=substr(Point,1,1),
           gridY=as.numeric(substr(Point,2,nchar(Point))),
           gridX=which(LETTERS == gridLetter))
  par(mfrow = c(6, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  plots<-list()
  for (i in 1:length(unique(coweeta_data$Yearday))){
    tmp = filter(coweeta_data, Yearday == unique(coweeta_data$Yearday)[i])
    
    #plots[[i]]<-ggplot(tmp,aes_string(x=gridX,y=gridY))+geom_point(aes_string(size=n))+xlim(0,26)+ylim(0,max(coweeta_data$gridY))
    plots[[i]]<-ggplot()+theme_bw(base_size=12*.3)+
      geom_point(aes_string(x=tmp$gridX,y=tmp$gridY,size=tmp$obsv))+
      xlim(0,26)+ylim(0,max(coweeta_data$gridY))+theme(legend.text=element_text(size=4), legend.key.size=unit(.5,"cm"))
    
  }
  pdf(paste0("coweeta_plots_",field_year,"_",field_plot,".pdf"))
  grid.arrange(grobs=plots)
  dev.off()
  return(coweeta_data)
  
}

BBsamp10<-samp_days(2010,"BB")
BBsamp11<-samp_days(2011,"BB")
BBsamp12<-samp_days(2012,"BB")
BSsamp12<-samp_days(2012,"BS")

#Histogram of frequency of surveys for every survey period/day
cow_samples<-cowplusnotes%>%
  filter(Year>2009, Plot%in% c("BS", "BB"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  select(Year,Yearday,Plot, Point, TreeSpecies, Sample)%>%
  distinct()%>%
  count(Year, Yearday)%>%
  rename(nSurveys=n)
hist(cow_samples$nSurveys, 20)

# cow_samp_hist<-ggplot(cow_samples,aes(x=Yearday,y=nSurveys))+geom_histogram(stat="identity")
# cow_samp_hist


threshold<-function(threshold_value){
  cow_thresh<-cowplusnotes%>%
    filter(Year>2009, Plot%in% c("BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
    select(Year,Yearday,Plot,Point,TreeSpecies,Sample)%>%
    distinct()%>%
    group_by(Year,Yearday)%>%
    tally()%>%
    rename(nSurveys=n)%>%
    mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
    #aggregate(cow_thresh$nSurveys,by=list(Year=cow_thresh$Year,cow_thresh$JulianWeek=JWeek),FUN=sum)
    group_by(Year,JulianWeek)%>%
    summarize(nJulianWeekSurvey=sum(nSurveys))%>%
    filter(nJulianWeekSurvey>threshold_value)%>%
    #count()
    group_by(Year)%>%
    add_count()%>%
    rename(nWeeks=n)
}


#So I forgot to filter out by sites, so actually, when you use a threshold of 100, BB sites are missing values in 2011 as there just weren't enough surveys then


thresh_100<-threshold(100)


#No point in doing 50, so we can start at 100


#Set Threshold for all years to 100, then find proportion of surveys-nSurveys for each day/Total nSurveys in that given julian week



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

cow_phen<-cow_pheno%>%
  filter(Plot==j)%>%
  replace_na(list(JulianWeek=0))%>%
  filter(cow_pheno$JulianWeek!=0)%>%
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

#Plotting coweeta data as average value of caterpillars seen for each julian week. 
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
    
 # fit<-cow_phen%>%
 #   filter(Year==i)
  
#  gfit1=fitG(x=fit$JulianWeek,y=fit$avg,mu=weighted.mean(fit$JulianWeek,fit$avg),sig=10,scale=150,control=list(maxit=10000),method="L-BFGS-B",lower=c(0,0,0,0,0,0))
#  p=gfit1$par
#  r2=cor(fit$JulianWeek,p[3]*dnorm(fit$JulianWeek,p[1],p[2]))^2
#  totalAvg=sum(fit$avg)
  
  plot(x=fit$JulianWeek,y=fit$avg,main=i, sub=j,type="l")
  #lines(0:365,p[3]*dnorm(0:365,p[1],p[2]),col='blue')
#  altpheno<-cow_pheno%>%
#    filter(Year==i)
#  catsum<-cumsum(altpheno$NumCaterpillars)
 # ten<-min(which(catsum>(0.1*sum(altpheno$photos))))
#  fifty<-min(which(catsum>(0.5*sum(altpheno$photos))))
#  halfcycle<-min(which(fit$avg>0.5*max(fit$avg)))
#  abline(v = fit[ten,2], col="red", lwd=3, lty=2)
#  abline(v = fit[fifty,2], col="blue", lwd=3, lty=2)
#  abline(v = fit[halfcycle,2], col="green", lwd=4, lty=2)
  
  
  
}
  
  }

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
  


# WeekSurveys<-sum(cowplusnotes$nSurveys)



#ggplot(cow_thresh,aes(x=Yearday,y=nSurveys))+geom_histogram(stat="identity")

#coweeta_data<-cowplusnotes%>%
#  filter(cowplusnotes$Year==2010,cowplusnotes$Plot=="BB")%>%
#  select(Plot, Yearday, Point, Sample,TreeSpecies)%>%
#  distinct()%>%
#  group_by(Plot,Point, Yearday)%>%
#  summarize(n=n())%>%





#coweeta_data<-cowplusnotes%>%
#  filter(cowplusnotes$Year==2010,cowplusnotes$Plot=="BB")%>%
#  select(Plot, Yearday, Point, Sample,TreeSpecies)%>%
#  distinct()%>%
#  group_by(Plot,Point, Yearday)%>%
#  summarize(n=n())%>%
#  mutate(gridLetter=substr(Point,1,1),
#         gridY=as.numeric(substr(Point,2,nchar(Point))),
#         gridX=which(LETTERS==gridLetter))


#par(mfrow = c(6, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))

#for (j in unique(coweeta_data$Yearday)){
#  tmp=filter(coweeta_data,Yearday==j)
#  plot<-ggplot(coweeta_data,aes(x=gridX,y=gridY))+geom_point(aes(size=n))
#}


#aggregate(BBday10$n,by=list(Sampled=BBday10$Yearday),FUN=sum)



sampled_days_BB<-cowplusnotes%>%
  filter(cowplusnotes$Plot=="BB",cowplusnotes$TreeSpecies!="8",cowplusnotes$TreeSpecies!="9")%>%
  group_by(TreeSpecies)%>%
  summarize(n=n())

sum(sampled_days_BB$n)
sum(sampled_days_BS$n)
sampled_days_BS<-cowplusnotes%>%
  filter(cowplusnotes$Plot=="BS",cowplusnotes$TreeSpecies!="8",cowplusnotes$TreeSpecies!="9")%>%
  group_by(TreeSpecies)%>%
  summarize(n=n())

#barplot(sampled_days$n, main=sampled_days$n,xlab="",width=1,names.arg=sampled_days$TreeSpecies, ylab="",)



site_plot_BB<-ggplot(data=sampled_days_BB,aes(x=sampled_days_BB$TreeSpecies,y=sampled_days_BB$n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text( color="black", size=8, angle=45,vjust=1,hjust=1))+
  xlab("Tree Species")+
  ylab("Number of Samples")
pdf(paste("BB_Tree_Samples.pdf"))
site_plot_BB
dev.off()

site_plot_BS<-ggplot(data=sampled_days_BS,aes(x=sampled_days_BS$TreeSpecies,y=sampled_days_BS$n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text( color="black", size=8, angle=45,vjust=1,hjust=1))+
  xlab("Tree Species")+
  ylab("Number of Samples")
pdf(paste0("BS_Tree_Samples.pdf"))
site_plot_BS
dev.off()




#Also we might want to create a plot of the amount of tree species surveyed, as in how many were surveyed, and whether it's the same amount each time.


BBfreq10<-freq_plot(2010, "BB")
BSfreq10<-freq_plot(2010, "BS")
BBfreq11<-freq_plot(2011, "BB")


BBday10<-samp_days(2010,"BB")
Bsday10<-samp_days(2010,"BS")





widecowplusnotes_2010<- grouped_cow_2010%>%
  spread(Yearday,'n()',fill=NA,convert=TRUE)%>%
  arrange(`136`)

write.csv(widecowplusnotes_2010,'widecowplusnotes_2010.csv')



widecowpointtally<-widecowplusnotes%>%
  group_by(Point)%>%
  tally()


# Plots examining # visits by grid point by yearday
BBfreq10 = filter(cow_freq_2010, Plot=="BB")


par(mfrow = c(5, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
for (j in unique(BBfreq10$Yearday)) {
  tmp = filter(BBfreq10, Yearday == j)
  plot(BBfreq10$gridX, BBfreq10$gridY, pch = 16, xlab = "", ylab = "")
}

