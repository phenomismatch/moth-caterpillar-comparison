


source('../caterpillars-analysis-public/code/analysis_functions.r')
source('code/formatting_coweeta.r')



#Function to filter by year and julian week, then gives the number of weeks that fulfill the designated threshold value and plot
threshold<-function(threshold_value, plot){
  cow_thresh<-cowplusnotes%>%
    filter(Year>2009, Plot%in% c(plot), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
    dplyr::select(Year,Yearday,Plot,Point,TreeSpecies,Sample)%>%
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

thresh_100<-threshold(100, "BB")
thresh_50<-threshold(50,"BB")

#At this point, I decide to use 50 as the threshold value for both sites, so we can now filter the coweeta dataset and convert into catcount format



#Plot of sample frequency
freq_plot<-function(field_year, field_plot){
  group_cow_set<-cowplusnotes%>%
    filter(cowplusnotes$Year==field_year, cowplusnotes$Plot==field_plot)%>%
    dplyr::select(Plot,Yearday,Point,TreeSpecies,Sample)%>%
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
  dplyr::select(Year, Plot,Yearday,Point,TreeSpecies,Sample)%>%
  distinct()%>%
  count(Year, TreeSpecies) %>%
  arrange(Year, desc(n)) %>%
  spread(key = TreeSpecies,value = n, fill = 0)


#pdf("coweeta_tree_surveys_by_year.pdf", height = 11, width = 8)
#par(mfrow = c(length(unique(treesByYear$Year)), 1))
#for (y in unique(treesByYear$Year)) {
#  bplot = barplot(as.matrix(treesByYear[treesByYear$Year == y, 4:ncol(treesByYear)]), col = 'darkorchid', xaxt = "n", xlab = "", ylab = "")
#}
#text(bplot, rep(-1, ncol(treesByYear)-4), xpd = TRUE, srt = 45)
#dev.off()





#BBfreq <- NA

#for(i in 2015:2018){
#  g<-freq_plot(i,"BB")
#  print(g)
#}

#Function to filter coweeta data for field year/plot, then get frequency for each day that was sampled
samp_days<-function(field_year,field_plot){
  coweeta_data<-cowplusnotes%>%
    filter(cowplusnotes$Year==field_year,cowplusnotes$Plot==field_plot)%>%
    dplyr::select(Plot, Yearday, Point,TreeSpecies, Sample)%>%
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

#Histogram of frequency of survey efforts
cow_samples<-cowplusnotes%>%
  filter(Year>2009, Plot%in% c("BS", "BB"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  dplyr::select(Year,Yearday,Plot, Point, TreeSpecies, Sample)%>%
  distinct()%>%
  count(Year, Yearday)%>%
  rename(nSurveys=n)
hist(cow_samples$nSurveys, 20)

# cow_samp_hist<-ggplot(cow_samples,aes(x=Yearday,y=nSurveys))+geom_histogram(stat="identity")
# cow_samp_hist




 # fit<-cow_phen%>%
 #   filter(Year==i)
  
#  gfit1=fitG(x=fit$JulianWeek,y=fit$avg,mu=weighted.mean(fit$JulianWeek,fit$avg),sig=10,scale=150,control=list(maxit=10000),method="L-BFGS-B",lower=c(0,0,0,0,0,0))
#  p=gfit1$par
#  r2=cor(fit$JulianWeek,p[3]*dnorm(fit$JulianWeek,p[1],p[2]))^2
#  totalAvg=sum(fit$avg)
  
#  plot(x=fit$JulianWeek,y=fit$avg,main=i, sub=j,type="l")
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

sampled_days_BS<-cowplusnotes%>%
  filter(cowplusnotes$Plot=="BS",cowplusnotes$TreeSpecies!="8",cowplusnotes$TreeSpecies!="9")%>%
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

