# Convert UGA Coweeta Caterpillars dataset into format of Caterpillars Count!
# --original file provided by Bob Cooper, Coweeta UGA Caterpillar data.xlsx
 
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
# Read in data, clean up leading/trailing spaces, weird symbols
cats = read.table('data/Coweeta_cats.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE) %>%
  filter(Plot != "") %>%
  mutate(Point = trimws(Point)) %>%
  mutate(Point = gsub("\v", "", Point)) %>%
  mutate(CaterpillarFamily = trimws(CaterpillarFamily)) %>%
  mutate(CaterpillarFamily = gsub("\v", "", CaterpillarFamily),Comments = gsub("\v", "", Comments))

catcomments = count(cats, Comments)
#write.table(catcomments, 'z:/lab/databases/coweetacaterpillars/coweeta_comments.txt', sep = '\t', row.names = F)

comments = read.table('data/coweeta_comments.txt', sep = '\t', header = T, quote = '\"', fill = TRUE, stringsAsFactors = FALSE)

# Go through comments and pull out pieces that need to go in the following fields:
#   arthropodNotes, NumberOfLeaves, Group, Hairy, Rolled, Tented, BeetleLarva, Sawfly

# NOTE: This can be done somewhat faster with regex, but there are really lots of exceptions and edge cases
# so it's probably faster to do it manually.

# E.g., Notes = gsub("([A-Z]{2,3} *[,/] *)*[A-Z]{2,3}", "", Comments), #matching sets of initials

catplus = left_join(cats, comments, by = 'Comments')




# Three sites: BB (1298 unique branches), BS (1677 unique branches), RK (575 unique branches)
numBBbranches = 1298
numBSbranches = 1677
numRKbranches = 575

# Figure out what is the largest existing survey branch ID in the existing database.
highestExistingSiteID = 1299
highestExistingBranchID = 2172
highestExistingSurveyID = 32015
highestExistingArthID = 43884


circles = c(rep(rep(1:5, each = 5), length.out = numBBbranches), 
            rep(rep(1:5, each = 5), length.out = numBSbranches), 
            rep(rep(1:5, each = 5), length.out = numRKbranches))
orient = c(rep(c('A', 'B', 'C', 'D', 'E'), length.out = numBBbranches),
           rep(c('A', 'B', 'C', 'D', 'E'), length.out = numBSbranches),
           rep(c('A', 'B', 'C', 'D', 'E'), length.out = numRKbranches))

branches = count(catplus, Plot, Point, TreeSpecies, Sample) %>%
  mutate(ID = (highestExistingBranchID + 1):(highestExistingBranchID + nrow(distinct(cats, Plot, Point, TreeSpecies, Sample))),
         Branch = paste(Plot, Point, TreeSpecies, Sample, sep = "_"),
         SiteFK = c(rep(highestExistingSiteID+1, numBBbranches), 
                    rep(highestExistingSiteID+2, numBSbranches), 
                    rep(highestExistingSiteID+3, numRKbranches)),
         Circle = circles,
         Orientation = orient,
         Code = 1:length(circles),
         Species = paste(substring(TreeSpecies, 1, 1), 
                         gsub("-", " ", tolower(substring(TreeSpecies, 2))), sep = ''),
         Species = gsub("_", " ", Species)) %>%
  select(Branch, ID, SiteFK, Circle, Orientation, Code, Species)


# surveys

multSurveyRecs = catplus %>%
  select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Comments) %>%
  distinct() %>%
  count(Year, Plot, Yearday, Point, TreeSpecies, Sample) %>%
  filter(n > 1) %>%
  left_join(catplus[, c('Year', 'Plot', 'Yearday', 'Point', 'TreeSpecies', 'Sample', 'Comments')])

# go through the multSurveyRecs manually to identify whether multiple records exist because of 
# differences in Comment text, and if so, identify what should be the appropriate surveyNote
# for that survey event. In this file below, 0 means surveyNote should be "", 1 means it
# should be whatever is in the Comment field, 2 means it's unclear whether there is a true
# second survey event of the same branch on the same yearday-year, 9 the appropriate surveyNote
# is provided in the 'x' field.

dups = read.table('data/coweeta_dup_surveys.txt', 
                  sep = '\t', header = T, quote = '\"', fill = TRUE, stringsAsFactors = FALSE)

dups019 = filter(dups, n %in% c(0, 1, 9)) %>%
  mutate(Notes = ifelse(n==1, Comments, ""),
         Notes = ifelse(n==9, x, Notes)) %>%
  select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Notes)

# surveys table for Coweeta data; 62097 recs

cowplusnotes = left_join(catplus, dups019, by = c('Year', 'Plot', 'Yearday', 'Point', 'TreeSpecies', 'Sample')) %>%
  mutate(Notes = ifelse(is.na(Notes), Comments, Notes))

cowplusnotes$surveyed<-ifelse(cowplusnotes$NumCaterpillars>=0,"1",NA)





#Not sure if this is the right way to create the function, if these two need to be separate functions or if I could combine them?
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
  #pdf(paste0("coweeta_plots_",field_year,"_",field_plot,".pdf"))
  for (j in unique(group_cow_set$Yearday)) {
    tmp = filter(group_cow_set, Yearday == j)
   plot(group_cow_set$gridX, group_cow_set$gridY, pch = 16, xlab = "", ylab = "",main="Plot Samples")
  }
  #dev.off()
  return(group_cow_set)
  
}
#

treesByYear = cowplusnotes %>%
  filter(Plot != "RK") %>%
  select(Year, Plot,Yearday,Point,TreeSpecies,Sample)%>%
  distinct()%>%
  count(Year, TreeSpecies) %>%
  arrange(Year, desc(n)) %>%
  spread(key = TreeSpecies,value = n, fill = 0)


pdf("coweeta_tree_surveys_by_year.pdf", height = 11, width = 8)
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
  filter(Year>2009, Plot%in% c("BB","BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
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
  


  thresh_160<-threshold(50)
  thresh_170<-threshold(170)
  thresh_180<-threshold(180)
  thresh_100<-threshold(100)
  thresh_50<-threshold(50)
  #No point in doing 50, so we can start at 100, maybe 180 is the upper bound? So let's go by 40 (100, 140, 18).
  
  
#Function to filter out for threshold value, plot, and year
cow_filter<-function(field_year,field_plot,threshold_value){
cow_WeeklySurv<-cowplusnotes%>%
  filter(Year==field_year, Plot==field_plot, TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  select(Year,Yearday,Plot,Point,TreeSpecies,Sample)%>%
  distinct()%>%
  group_by(Year,Yearday)%>%
  tally()%>%
  rename(nSurveys=n)%>%
  mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
  group_by(JulianWeek)%>%
  mutate(WeeklySurv=sum(nSurveys))%>%
  filter(WeeklySurv>threshold_value)
}

# Filters -----------------------------------------------------------------

BB_100_2010_filter<-cow_filter(field_year = 2010,field_plot = "BB",threshold_value = 100)
BS_100_2010_filter<-cow_filter(field_year = 2010,field_plot = "BS",threshold_value = 100)
BB_140_2010_filter<-cow_filter(field_year = 2010,field_plot = "BB",threshold_value = 140)
BS_140_2010_filter<-cow_filter(field_year = 2010,field_plot = "BS",threshold_value = 140)
BB_180_2010_filter<-cow_filter(field_year = 2010,field_plot = "BB",threshold_value = 180)
BS_180_2010_filter<-cow_filter(field_year = 2010,field_plot = "BS",threshold_value = 180)

BB_100_2011_filter<-cow_filter(field_year = 2011,field_plot = "BB",threshold_value = 100)
BS_100_2011_filter<-cow_filter(field_year = 2011,field_plot = "BS",threshold_value = 100)
BB_140_2011_filter<-cow_filter(field_year = 2011,field_plot = "BB",threshold_value = 140)
BS_140_2011_filter<-cow_filter(field_year = 2011,field_plot = "BS",threshold_value = 140)
BB_180_2011_filter<-cow_filter(field_year = 2011,field_plot = "BB",threshold_value = 180)
BS_180_2011_filter<-cow_filter(field_year = 2011,field_plot = "BS",threshold_value = 180)

BB_100_2012_filter<-cow_filter(field_year = 2012,field_plot = "BB",threshold_value = 100)
BS_100_2012_filter<-cow_filter(field_year = 2012,field_plot = "BS",threshold_value = 100)
BB_140_2012_filter<-cow_filter(field_year = 2012,field_plot = "BB",threshold_value = 140)
BS_140_2012_filter<-cow_filter(field_year = 2012,field_plot = "BS",threshold_value = 140)
BB_180_2012_filter<-cow_filter(field_year = 2012,field_plot = "BB",threshold_value = 180)
BS_180_2012_filter<-cow_filter(field_year = 2012,field_plot = "BS",threshold_value = 180)

BB_100_2013_filter<-cow_filter(field_year = 2013,field_plot = "BB",threshold_value = 100)
BS_100_2013_filter<-cow_filter(field_year = 2013,field_plot = "BS",threshold_value = 100)
BB_140_2013_filter<-cow_filter(field_year = 2013,field_plot = "BB",threshold_value = 140)
BS_140_2013_filter<-cow_filter(field_year = 2013,field_plot = "BS",threshold_value = 140)
BB_180_2013_filter<-cow_filter(field_year = 2013,field_plot = "BB",threshold_value = 180)
BS_180_2013_filter<-cow_filter(field_year = 2013,field_plot = "BS",threshold_value = 180)

BB_100_2014_filter<-cow_filter(field_year = 2014,field_plot = "BB",threshold_value = 100)
BS_100_2014_filter<-cow_filter(field_year = 2014,field_plot = "BS",threshold_value = 100)
BB_140_2014_filter<-cow_filter(field_year = 2014,field_plot = "BB",threshold_value = 140)
BS_140_2014_filter<-cow_filter(field_year = 2014,field_plot = "BS",threshold_value = 140)
BB_180_2014_filter<-cow_filter(field_year = 2014,field_plot = "BB",threshold_value = 180)
BS_180_2014_filter<-cow_filter(field_year = 2014,field_plot = "BS",threshold_value = 180)

BB_100_2015_filter<-cow_filter(field_year = 2015,field_plot = "BB",threshold_value = 100)
BS_100_2015_filter<-cow_filter(field_year = 2015,field_plot = "BS",threshold_value = 100)
BB_140_2015_filter<-cow_filter(field_year = 2015,field_plot = "BB",threshold_value = 140)
BS_140_2015_filter<-cow_filter(field_year = 2015,field_plot = "BS",threshold_value = 140)
BB_180_2015_filter<-cow_filter(field_year = 2015,field_plot = "BB",threshold_value = 180)
BS_180_2015_filter<-cow_filter(field_year = 2015,field_plot = "BS",threshold_value = 180)

BB_100_2016_filter<-cow_filter(field_year = 2016,field_plot = "BB",threshold_value = 100)
BS_100_2016_filter<-cow_filter(field_year = 2016,field_plot = "BS",threshold_value = 100)
BB_140_2016_filter<-cow_filter(field_year = 2016,field_plot = "BB",threshold_value = 140)
BS_140_2016_filter<-cow_filter(field_year = 2016,field_plot = "BS",threshold_value = 140)
BB_180_2016_filter<-cow_filter(field_year = 2016,field_plot = "BB",threshold_value = 180)
BS_180_2016_filter<-cow_filter(field_year = 2016,field_plot = "BS",threshold_value = 180)

BB_100_2017_filter<-cow_filter(field_year = 2017,field_plot = "BB",threshold_value = 100)
BS_100_2017_filter<-cow_filter(field_year = 2017,field_plot = "BS",threshold_value = 100)
BB_140_2017_filter<-cow_filter(field_year = 2017,field_plot = "BB",threshold_value = 140)
BS_140_2017_filter<-cow_filter(field_year = 2017,field_plot = "BS",threshold_value = 140)
BB_180_2017_filter<-cow_filter(field_year = 2017,field_plot = "BB",threshold_value = 180)
BS_180_2017_filter<-cow_filter(field_year = 2017,field_plot = "BS",threshold_value = 180)

BB_100_2018_filter<-cow_filter(field_year = 2018,field_plot = "BB",threshold_value = 100)
BS_100_2018_filter<-cow_filter(field_year = 2018,field_plot = "BS",threshold_value = 100)
BB_140_2018_filter<-cow_filter(field_year = 2018,field_plot = "BB",threshold_value = 140)
BS_140_2018_filter<-cow_filter(field_year = 2018,field_plot = "BS",threshold_value = 140)
BB_180_2018_filter<-cow_filter(field_year = 2018,field_plot = "BB",threshold_value = 180)
BS_180_2018_filter<-cow_filter(field_year = 2018,field_plot = "BS",threshold_value = 180)

#Merge ---------


#can also make this a function-can/should i link those together?
cow_join<-function(field_year,field_plot, site_thresh_year_filter){
  cowplusnotes%>%
  filter(Year==field_year, Plot==field_plot, TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  left_join(site_thresh_year_filter,by=c('Year', 'Yearday'))%>%
  filter(!is.na(WeeklySurv))%>%
  subset(select=-c(surveyed,nSurveys,JulianWeek, WeeklySurv))
}

BB_100_2010_merged<-cow_join(2010,"BB",BB_100_2010_filter)
BS_100_2010_merged<-cow_join(2010,"BS",BS_100_2010_filter)
BB_140_2010_merged<-cow_join(2010,"BB",BB_140_2010_filter)
BS_140_2010_merged<-cow_join(2010,"BS",BS_140_2010_filter)
BB_180_2010_merged<-cow_join(2010,"BB",BB_180_2010_filter)
BS_180_2010_merged<-cow_join(2010,"BS",BS_180_2010_filter)

BB_100_2011_merged<-cow_join(2011,"BB",BB_100_2011_filter)
BS_100_2011_merged<-cow_join(2011,"BS",BS_100_2011_filter)
BB_140_2011_merged<-cow_join(2011,"BB",BB_140_2011_filter)
BS_140_2011_merged<-cow_join(2011,"BS",BS_140_2011_filter)
BB_180_2011_merged<-cow_join(2011,"BB",BB_180_2011_filter)
BS_180_2011_merged<-cow_join(2011,"BS",BS_180_2011_filter)


BB_100_2012_merged<-cow_join(2012,"BB",BB_100_2012_filter)
BS_100_2012_merged<-cow_join(2012,"BS",BS_100_2012_filter)
BB_140_2012_merged<-cow_join(2012,"BB",BB_140_2012_filter)
BS_140_2012_merged<-cow_join(2012,"BS",BS_140_2012_filter)
BB_180_2012_merged<-cow_join(2012,"BB",BB_180_2012_filter)
BS_180_2012_merged<-cow_join(2012,"BS",BS_180_2012_filter)



BB_100_2013_merged<-cow_join(2013,"BB",BB_100_2013_filter)
BS_100_2013_merged<-cow_join(2013,"BS",BS_100_2013_filter)
BB_140_2013_merged<-cow_join(2013,"BB",BB_140_2013_filter)
BS_140_2013_merged<-cow_join(2013,"BS",BS_140_2013_filter)
BB_180_2013_merged<-cow_join(2013,"BB",BB_180_2013_filter)
BS_180_2013_merged<-cow_join(2013,"BS",BS_180_2013_filter)


BB_100_2014_merged<-cow_join(2014,"BB",BB_100_2014_filter)
BS_100_2014_merged<-cow_join(2014,"BS",BS_100_2014_filter)
BB_140_2014_merged<-cow_join(2014,"BB",BB_140_2014_filter)
BS_140_2014_merged<-cow_join(2014,"BS",BS_140_2014_filter)
BB_180_2014_merged<-cow_join(2014,"BB",BB_180_2014_filter)
BS_180_2014_merged<-cow_join(2014,"BS",BS_180_2014_filter)


BB_100_2015_merged<-cow_join(2015,"BB",BB_100_2015_filter)
BS_100_2015_merged<-cow_join(2015,"BS",BS_100_2015_filter)
BB_140_2015_merged<-cow_join(2015,"BB",BB_140_2015_filter)
BS_140_2015_merged<-cow_join(2015,"BS",BS_140_2015_filter)
BB_180_2015_merged<-cow_join(2015,"BB",BB_180_2015_filter)
BS_180_2015_merged<-cow_join(2015,"BS",BS_180_2015_filter)


BB_100_2016_merged<-cow_join(2016,"BB",BB_100_2016_filter)
BS_100_2016_merged<-cow_join(2016,"BS",BS_100_2016_filter)
BB_140_2016_merged<-cow_join(2016,"BB",BB_140_2016_filter)
BS_140_2016_merged<-cow_join(2016,"BS",BS_140_2016_filter)
BB_180_2016_merged<-cow_join(2016,"BB",BB_180_2016_filter)
BS_180_2016_merged<-cow_join(2016,"BS",BS_180_2016_filter)


BB_100_2017_merged<-cow_join(2017,"BB",BB_100_2017_filter)
BS_100_2017_merged<-cow_join(2017,"BS",BS_100_2017_filter)
BB_140_2017_merged<-cow_join(2017,"BB",BB_140_2017_filter)
BS_140_2017_merged<-cow_join(2017,"BS",BS_140_2017_filter)
BB_180_2017_merged<-cow_join(2017,"BB",BB_180_2017_filter)
BS_180_2017_merged<-cow_join(2017,"BS",BS_180_2017_filter)


BB_100_2018_merged<-cow_join(2018,"BB",BB_100_2018_filter)
BS_100_2018_merged<-cow_join(2018,"BS",BS_100_2018_filter)
BB_140_2018_merged<-cow_join(2018,"BB",BB_140_2018_filter)
BS_140_2018_merged<-cow_join(2018,"BS",BS_140_2018_filter)
BB_180_2018_merged<-cow_join(2018,"BB",BB_180_2018_filter)
BS_180_2018_merged<-cow_join(2018,"BS",BS_180_2018_filter)


#-----
  names(BB_100_2010)[names(BB_100_2010) == 'Yearday'] <- 'julianday'




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


par(mfrow = c(6, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))

for (j in unique(coweeta_data$Yearday)){
  tmp=filter(coweeta_data,Yearday==j)
  plot<-ggplot(coweeta_data,aes(x=gridX,y=gridY))+geom_point(aes(size=n))
}


aggregate(BBday10$n,by=list(Sampled=BBday10$Yearday),FUN=sum)



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

#Cow_survs------

coweeta_surveys<-function(merged){
  cowsurvs = merged %>%
  select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Notes) %>%
  distinct() %>%
  left_join(comments[, c('Comments', 'NumberOfLeaves')], by = c('Notes' = 'Comments')) %>%
  mutate(Branch = paste(Plot, Point, TreeSpecies, Sample, sep = "_"),
         ID = highestExistingSurveyID + 1:n(),
         SubmissionTimestamp = rep(0, n()),
         UserFKOfObserver = rep(1830, n()), # dummy account created for "Robert Cooper Lab", UserFK 1830
         LocalDate = as.Date(Yearday, as.Date(paste(Year, "-01-01", sep = ""))),
         LocalTime = rep("00:00:00", n()),
         ObservationMethod = rep("Visual", n()),
         WetLeaves = rep("", n()),
         PlantSpecies = paste(substring(TreeSpecies, 1, 1), 
                              gsub("-", " ", tolower(substring(TreeSpecies, 2))), sep = ''),
         PlantSpecies = gsub("_", " ", PlantSpecies),
         NumberOfLeaves = ifelse(is.na(NumberOfLeaves), 50, NumberOfLeaves),
         AverageLeafLength = rep(-128, n()),
         HerbivoryScore = rep(-128, n()),
         SubmittedThroughApp = rep(0, n()),
         MinimumTemperature = rep(9999, n()),
         MaximumTemperature = rep(9999, n()),
         NeedToSendToSciStarter = rep(0, n()),
         CORRESPONDING_OLD_DATABASE_SURVEY_ID = rep(0,n())) %>%
  left_join(branches[, c('Branch', 'ID')], by = 'Branch') %>%
  rename(PlantFK = ID.y, ID = ID.x) %>%
  select(ID, SubmissionTimestamp, UserFKOfObserver, PlantFK, LocalDate, LocalTime, ObservationMethod, Notes, 
         WetLeaves, PlantSpecies, NumberOfLeaves, AverageLeafLength, HerbivoryScore, SubmittedThroughApp,
         MinimumTemperature, MaximumTemperature, NeedToSendToSciStarter, CORRESPONDING_OLD_DATABASE_SURVEY_ID, Branch)
}

BS_100_2010_cowsurvs<-coweeta_surveys(BS_100_2010_merged) 
BS_140_2010_cowsurvs<-coweeta_surveys(BS_140_2010_merged)
BS_180_2010_cowsurvs<-coweeta_surveys(BS_180_2010_merged)
BB_100_2010_cowsurvs<-coweeta_surveys(BB_100_2010_merged) 
BB_140_2010_cowsurvs<-coweeta_surveys(BB_140_2010_merged)
BB_180_2010_cowsurvs<-coweeta_surveys(BB_180_2010_merged)


BS_100_2011_cowsurvs<-coweeta_surveys(BS_100_2011_merged) 
BS_140_2011_cowsurvs<-coweeta_surveys(BS_140_2011_merged)
BS_180_2011_cowsurvs<-coweeta_surveys(BS_180_2011_merged)
BB_100_2011_cowsurvs<-coweeta_surveys(BB_100_2011_merged) 
BB_140_2011_cowsurvs<-coweeta_surveys(BB_140_2011_merged)
BB_180_2011_cowsurvs<-coweeta_surveys(BB_180_2011_merged)



BS_100_2012_cowsurvs<-coweeta_surveys(BS_100_2012_merged) 
BS_140_2012_cowsurvs<-coweeta_surveys(BS_140_2012_merged)
BS_180_2012_cowsurvs<-coweeta_surveys(BS_180_2012_merged)
BB_100_2012_cowsurvs<-coweeta_surveys(BB_100_2012_merged) 
BB_140_2012_cowsurvs<-coweeta_surveys(BB_140_2012_merged)
BB_180_2012_cowsurvs<-coweeta_surveys(BB_180_2012_merged)


BS_100_2013_cowsurvs<-coweeta_surveys(BS_100_2013_merged) 
BS_140_2013_cowsurvs<-coweeta_surveys(BS_140_2013_merged)
BS_180_2013_cowsurvs<-coweeta_surveys(BS_180_2013_merged)
BB_100_2013_cowsurvs<-coweeta_surveys(BB_100_2013_merged) 
BB_140_2013_cowsurvs<-coweeta_surveys(BB_140_2013_merged)
BB_180_2013_cowsurvs<-coweeta_surveys(BB_180_2013_merged)


BS_100_2014_cowsurvs<-coweeta_surveys(BS_100_2014_merged) 
BS_140_2014_cowsurvs<-coweeta_surveys(BS_140_2014_merged)
BS_180_2014_cowsurvs<-coweeta_surveys(BS_180_2014_merged)
BB_100_2014_cowsurvs<-coweeta_surveys(BB_100_2014_merged) 
BB_140_2014_cowsurvs<-coweeta_surveys(BB_140_2014_merged)
BB_180_2014_cowsurvs<-coweeta_surveys(BB_180_2014_merged)


BS_100_2015_cowsurvs<-coweeta_surveys(BS_100_2015_merged) 
BS_140_2015_cowsurvs<-coweeta_surveys(BS_140_2015_merged)
BS_180_2015_cowsurvs<-coweeta_surveys(BS_180_2015_merged)
BB_100_2015_cowsurvs<-coweeta_surveys(BB_100_2015_merged) 
BB_140_2015_cowsurvs<-coweeta_surveys(BB_140_2015_merged)
BB_180_2015_cowsurvs<-coweeta_surveys(BB_180_2015_merged)


BS_100_2016_cowsurvs<-coweeta_surveys(BS_100_2016_merged) 
BS_140_2016_cowsurvs<-coweeta_surveys(BS_140_2016_merged)
BS_180_2016_cowsurvs<-coweeta_surveys(BS_180_2016_merged)
BB_100_2016_cowsurvs<-coweeta_surveys(BB_100_2016_merged) 
BB_140_2016_cowsurvs<-coweeta_surveys(BB_140_2016_merged)
BB_180_2016_cowsurvs<-coweeta_surveys(BB_180_2016_merged)


BS_100_2017_cowsurvs<-coweeta_surveys(BS_100_2017_merged) 
BS_140_2017_cowsurvs<-coweeta_surveys(BS_140_2017_merged)
BS_180_2017_cowsurvs<-coweeta_surveys(BS_180_2017_merged)
BB_100_2017_cowsurvs<-coweeta_surveys(BB_100_2017_merged) 
BB_140_2017_cowsurvs<-coweeta_surveys(BB_140_2017_merged)
BB_180_2017_cowsurvs<-coweeta_surveys(BB_180_2017_merged)


BS_100_2018_cowsurvs<-coweeta_surveys(BS_100_2018_merged) 
BS_140_2018_cowsurvs<-coweeta_surveys(BS_140_2018_merged)
BS_180_2018_cowsurvs<-coweeta_surveys(BS_180_2018_merged)
BB_100_2018_cowsurvs<-coweeta_surveys(BB_100_2018_merged) 
BB_140_2018_cowsurvs<-coweeta_surveys(BB_140_2018_merged)
BB_180_2018_cowsurvs<-coweeta_surveys(BB_180_2018_merged)

# 69 records with 2 or more "survey events" per branch/date
multSurveyRecs2 = cowsurvs %>%
  count(ID, PlantFK, LocalDate, Notes) %>%
  filter(n > 1)
  

# arthropods table------
coweeta_arths<-function(merged,coweeta_surv){
  cowarths = merged%>%
  mutate(Branch = paste(Plot, Point, TreeSpecies, Sample, sep = "_"),
         LocalDate = as.Date(Yearday, as.Date(paste(Year, "-01-01", sep = "")))) %>%
  left_join(coweeta_surv[, c('Branch', 'ID', 'LocalDate', 'Notes')], by = c('Branch', 'LocalDate', 'Notes')) %>%
  rename(SurveyFK = ID) %>%
  filter(NumCaterpillars > 0) %>%
  mutate(ID = highestExistingArthID + 1:n(),
         Group = ifelse(Group == "", "caterpillar", as.character(Group)),
         Length = Length_mm,
         Quantity = NumCaterpillars,
         PhotoURL = rep("", n()),
         Notes = ifelse(CaterpillarFamily == "", arthropodNotes, 
                        ifelse(arthropodNotes == "", CaterpillarFamily, paste(arthropodNotes, CaterpillarFamily, sep = ";"))),
         Hairy = ifelse(is.na(Hairy), 0, Hairy),
         Rolled = ifelse(is.na(Rolled), 0, Rolled),
         Tented = ifelse(is.na(Tented), 0, Tented),
         Sawfly = ifelse(!is.na(Sawfly) | CaterpillarFamily == "Sawfly", 1, 0),
         BeetleLarva = ifelse(is.na(BeetleLarva), 0, BeetleLarva),
         NeedToSendToINaturalist = rep(0, n()),
         Group = ifelse(BeetleLarva == 1, "beetle", Group),
         Group = ifelse(CaterpillarFamily == "Sawfly", "bee", Group)) %>%
  select(ID, SurveyFK, Group, Length, Quantity, PhotoURL, Notes, Hairy, Rolled, Tented, Sawfly, BeetleLarva, NeedToSendToINaturalist)
}


BS_100_2010_cowarths<-coweeta_arths(BS_100_2010_merged,BS_100_2010_cowsurvs)
BS_140_2010_cowarths<-coweeta_arths(BS_140_2010_merged,BS_140_2010_cowsurvs)
BS_180_2010_cowarths<-coweeta_arths(BS_180_2010_merged,BS_180_2010_cowsurvs)
BB_100_2010_cowarths<-coweeta_arths(BB_100_2010_merged,BB_100_2010_cowsurvs)
BB_140_2010_cowarths<-coweeta_arths(BB_140_2010_merged,BB_140_2010_cowsurvs)
BB_180_2010_cowarths<-coweeta_arths(BB_180_2010_merged,BB_180_2010_cowsurvs)

BS_100_2011_cowarths<-coweeta_arths(BS_100_2011_merged,BS_100_2011_cowsurvs)
BS_140_2011_cowarths<-coweeta_arths(BS_140_2011_merged,BS_140_2011_cowsurvs)
BS_180_2011_cowarths<-coweeta_arths(BS_180_2011_merged,BS_180_2011_cowsurvs)
BB_100_2011_cowarths<-coweeta_arths(BB_100_2011_merged,BB_100_2011_cowsurvs)
BB_140_2011_cowarths<-coweeta_arths(BB_140_2011_merged,BB_140_2011_cowsurvs)
BB_180_2011_cowarths<-coweeta_arths(BB_180_2011_merged,BB_180_2011_cowsurvs)


BS_100_2012_cowarths<-coweeta_arths(BS_100_2012_merged,BS_100_2012_cowsurvs)
BS_140_2012_cowarths<-coweeta_arths(BS_140_2012_merged,BS_140_2012_cowsurvs)
BS_180_2012_cowarths<-coweeta_arths(BS_180_2012_merged,BS_180_2012_cowsurvs)
BB_100_2012_cowarths<-coweeta_arths(BB_100_2012_merged,BB_100_2012_cowsurvs)
BB_140_2012_cowarths<-coweeta_arths(BB_140_2012_merged,BB_140_2012_cowsurvs)
BB_180_2012_cowarths<-coweeta_arths(BB_180_2012_merged,BB_180_2012_cowsurvs)


BS_100_2013_cowarths<-coweeta_arths(BS_100_2013_merged,BS_100_2013_cowsurvs)
BS_140_2013_cowarths<-coweeta_arths(BS_140_2013_merged,BS_140_2013_cowsurvs)
BS_180_2013_cowarths<-coweeta_arths(BS_180_2013_merged,BS_180_2013_cowsurvs)
BB_100_2013_cowarths<-coweeta_arths(BB_100_2013_merged,BB_100_2013_cowsurvs)
BB_140_2013_cowarths<-coweeta_arths(BB_140_2013_merged,BB_140_2013_cowsurvs)
BB_180_2013_cowarths<-coweeta_arths(BB_180_2013_merged,BB_180_2013_cowsurvs)


BS_100_2014_cowarths<-coweeta_arths(BS_100_2014_merged,BS_100_2014_cowsurvs)
BS_140_2014_cowarths<-coweeta_arths(BS_140_2014_merged,BS_140_2014_cowsurvs)
BS_180_2014_cowarths<-coweeta_arths(BS_180_2014_merged,BS_180_2014_cowsurvs)
BB_100_2014_cowarths<-coweeta_arths(BB_100_2014_merged,BB_100_2014_cowsurvs)
BB_140_2014_cowarths<-coweeta_arths(BB_140_2014_merged,BB_140_2014_cowsurvs)
BB_180_2014_cowarths<-coweeta_arths(BB_180_2014_merged,BB_180_2014_cowsurvs)

BS_100_2015_cowarths<-coweeta_arths(BS_100_2015_merged,BS_100_2015_cowsurvs)
BS_140_2015_cowarths<-coweeta_arths(BS_140_2015_merged,BS_140_2015_cowsurvs)
BS_180_2015_cowarths<-coweeta_arths(BS_180_2015_merged,BS_180_2015_cowsurvs)
BB_100_2015_cowarths<-coweeta_arths(BB_100_2015_merged,BB_100_2015_cowsurvs)
BB_140_2015_cowarths<-coweeta_arths(BB_140_2015_merged,BB_140_2015_cowsurvs)
BB_180_2015_cowarths<-coweeta_arths(BB_180_2015_merged,BB_180_2015_cowsurvs)

BS_100_2016_cowarths<-coweeta_arths(BS_100_2016_merged,BS_100_2016_cowsurvs)
BS_140_2016_cowarths<-coweeta_arths(BS_140_2016_merged,BS_140_2016_cowsurvs)
BS_180_2016_cowarths<-coweeta_arths(BS_180_2016_merged,BS_180_2016_cowsurvs)
BB_100_2016_cowarths<-coweeta_arths(BB_100_2016_merged,BB_100_2016_cowsurvs)
BB_140_2016_cowarths<-coweeta_arths(BB_140_2016_merged,BB_140_2016_cowsurvs)
BB_180_2016_cowarths<-coweeta_arths(BB_180_2016_merged,BB_180_2016_cowsurvs)

BS_100_2017_cowarths<-coweeta_arths(BS_100_2017_merged,BS_100_2017_cowsurvs)
BS_140_2017_cowarths<-coweeta_arths(BS_140_2017_merged,BS_140_2017_cowsurvs)
BS_180_2017_cowarths<-coweeta_arths(BS_180_2017_merged,BS_180_2017_cowsurvs)
BB_100_2017_cowarths<-coweeta_arths(BB_100_2017_merged,BB_100_2017_cowsurvs)
BB_140_2017_cowarths<-coweeta_arths(BB_140_2017_merged,BB_140_2017_cowsurvs)
BB_180_2017_cowarths<-coweeta_arths(BB_180_2017_merged,BB_180_2017_cowsurvs)

BS_100_2018_cowarths<-coweeta_arths(BS_100_2018_merged,BS_100_2018_cowsurvs)
BS_140_2018_cowarths<-coweeta_arths(BS_140_2018_merged,BS_140_2018_cowsurvs)
BS_180_2018_cowarths<-coweeta_arths(BS_180_2018_merged,BS_180_2018_cowsurvs)
BB_100_2018_cowarths<-coweeta_arths(BB_100_2018_merged,BB_100_2018_cowsurvs)
BB_140_2018_cowarths<-coweeta_arths(BB_140_2018_merged,BB_140_2018_cowsurvs)
BB_180_2018_cowarths<-coweeta_arths(BB_180_2018_merged,BB_180_2018_cowsurvs)


# Write files
write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#Merge arth and surv======
merge_fun<-function(cowsurv,cowarths){
merged_set<-cowsurv%>%
  left_join(cowarths, by= c('ID'= 'SurveyFK'))%>%
  rename(arthID=ID.y)
}

BS_100_2010_final<-merge_fun(BS_100_2010_cowsurvs,BS_100_2010_cowarths)
BS_140_2010_final<-merge_fun(BS_140_2010_cowsurvs,BS_140_2010_cowarths)
BS_180_2010_final<-merge_fun(BS_180_2010_cowsurvs,BS_180_2010_cowarths)
BB_100_2010_final<-merge_fun(BB_100_2010_cowsurvs,BB_100_2010_cowarths)
BB_140_2010_final<-merge_fun(BB_140_2010_cowsurvs,BB_140_2010_cowarths)
BB_180_2010_final<-merge_fun(BB_180_2010_cowsurvs,BB_180_2010_cowarths)

BS_100_2011_final<-merge_fun(BS_100_2011_cowsurvs,BS_100_2011_cowarths)
BS_140_2011_final<-merge_fun(BS_140_2011_cowsurvs,BS_140_2011_cowarths)
BS_180_2011_final<-merge_fun(BS_180_2011_cowsurvs,BS_180_2011_cowarths)
BB_100_2011_final<-merge_fun(BB_100_2011_cowsurvs,BB_100_2011_cowarths)
BB_140_2011_final<-merge_fun(BB_140_2011_cowsurvs,BB_140_2011_cowarths)
BB_180_2011_final<-merge_fun(BB_180_2011_cowsurvs,BB_180_2011_cowarths)

BS_100_2012_final<-merge_fun(BS_100_2012_cowsurvs,BS_100_2012_cowarths)
BS_140_2012_final<-merge_fun(BS_140_2012_cowsurvs,BS_140_2012_cowarths)
BS_180_2012_final<-merge_fun(BS_180_2012_cowsurvs,BS_180_2012_cowarths)
BB_100_2012_final<-merge_fun(BB_100_2012_cowsurvs,BB_100_2012_cowarths)
BB_140_2012_final<-merge_fun(BB_140_2012_cowsurvs,BB_140_2012_cowarths)
BB_180_2012_final<-merge_fun(BB_180_2012_cowsurvs,BB_180_2012_cowarths)

BS_100_2013_final<-merge_fun(BS_100_2013_cowsurvs,BS_100_2013_cowarths)
BS_140_2013_final<-merge_fun(BS_140_2013_cowsurvs,BS_140_2013_cowarths)
BS_180_2013_final<-merge_fun(BS_180_2013_cowsurvs,BS_180_2013_cowarths)
BB_100_2013_final<-merge_fun(BB_100_2013_cowsurvs,BB_100_2013_cowarths)
BB_140_2013_final<-merge_fun(BB_140_2013_cowsurvs,BB_140_2013_cowarths)
BB_180_2013_final<-merge_fun(BB_180_2013_cowsurvs,BB_180_2013_cowarths)

BS_100_2014_final<-merge_fun(BS_100_2014_cowsurvs,BS_100_2014_cowarths)
BS_140_2014_final<-merge_fun(BS_140_2014_cowsurvs,BS_140_2014_cowarths)
BS_180_2014_final<-merge_fun(BS_180_2014_cowsurvs,BS_180_2014_cowarths)
BB_100_2014_final<-merge_fun(BB_100_2014_cowsurvs,BB_100_2014_cowarths)
BB_140_2014_final<-merge_fun(BB_140_2014_cowsurvs,BB_140_2014_cowarths)
BB_180_2014_final<-merge_fun(BB_180_2014_cowsurvs,BB_180_2014_cowarths)

BS_100_2015_final<-merge_fun(BS_100_2015_cowsurvs,BS_100_2015_cowarths)
BS_140_2015_final<-merge_fun(BS_140_2015_cowsurvs,BS_140_2015_cowarths)
BS_180_2015_final<-merge_fun(BS_180_2015_cowsurvs,BS_180_2015_cowarths)
BB_100_2015_final<-merge_fun(BB_100_2015_cowsurvs,BB_100_2015_cowarths)
BB_140_2015_final<-merge_fun(BB_140_2015_cowsurvs,BB_140_2015_cowarths)
BB_180_2015_final<-merge_fun(BB_180_2015_cowsurvs,BB_180_2015_cowarths)

BS_100_2016_final<-merge_fun(BS_100_2016_cowsurvs,BS_100_2016_cowarths)
BS_140_2016_final<-merge_fun(BS_140_2016_cowsurvs,BS_140_2016_cowarths)
BS_180_2016_final<-merge_fun(BS_180_2016_cowsurvs,BS_180_2016_cowarths)
BB_100_2016_final<-merge_fun(BB_100_2016_cowsurvs,BB_100_2016_cowarths)
BB_140_2016_final<-merge_fun(BB_140_2016_cowsurvs,BB_140_2016_cowarths)
BB_180_2016_final<-merge_fun(BB_180_2016_cowsurvs,BB_180_2016_cowarths)

BS_100_2017_final<-merge_fun(BS_100_2017_cowsurvs,BS_100_2017_cowarths)
BS_140_2017_final<-merge_fun(BS_140_2017_cowsurvs,BS_140_2017_cowarths)
BS_180_2017_final<-merge_fun(BS_180_2017_cowsurvs,BS_180_2017_cowarths)
BB_100_2017_final<-merge_fun(BB_100_2017_cowsurvs,BB_100_2017_cowarths)
BB_140_2017_final<-merge_fun(BB_140_2017_cowsurvs,BB_140_2017_cowarths)
BB_180_2017_final<-merge_fun(BB_180_2017_cowsurvs,BB_180_2017_cowarths)

BS_100_2018_final<-merge_fun(BS_100_2018_cowsurvs,BS_100_2018_cowarths)
BS_140_2018_final<-merge_fun(BS_140_2018_cowsurvs,BS_140_2018_cowarths)
BS_180_2018_final<-merge_fun(BS_180_2018_cowsurvs,BS_180_2018_cowarths)
BB_100_2018_final<-merge_fun(BB_100_2018_cowsurvs,BB_100_2018_cowarths)
BB_140_2018_final<-merge_fun(BB_140_2018_cowsurvs,BB_140_2018_cowarths)
BB_180_2018_final<-merge_fun(BB_180_2018_cowsurvs,BB_180_2018_cowarths)



#date_change<-function(Final_set){
#  juliandate=Final_set%>%
#  Final_set$LocalDate=as.Date(Final_set$LocalDate,format="%Y-%m-%d")
#  Final_set$Year = format(Final_set$LocalDate, "%Y")
#  Final_set$julianday = yday(Final_set$LocalDate)
#}
  

#Change Date-------
BS_100_2010_final$LocalDate=as.Date(BS_100_2010_final$LocalDate,format="%Y-%m-%d")
BS_100_2010_final$Year = format(BS_100_2010_final$LocalDate, "%Y")
BS_100_2010_final$julianday = yday(BS_100_2010_final$LocalDate)
BS_140_2010_final$LocalDate=as.Date(BS_140_2010_final$LocalDate,format="%Y-%m-%d")
BS_140_2010_final$Year = format(BS_140_2010_final$LocalDate, "%Y")
BS_140_2010_final$julianday = yday(BS_140_2010_final$LocalDate)
BS_180_2010_final$LocalDate=as.Date(BS_180_2010_final$LocalDate,format="%Y-%m-%d")
BS_180_2010_final$Year = format(BS_180_2010_final$LocalDate, "%Y")
BS_180_2010_final$julianday = yday(BS_180_2010_final$LocalDate)

BB_100_2010_final$LocalDate=as.Date(BB_100_2010_final$LocalDate,format="%Y-%m-%d")
BB_100_2010_final$Year = format(BB_100_2010_final$LocalDate, "%Y")
BB_100_2010_final$julianday = yday(BB_100_2010_final$LocalDate)
BB_140_2010_final$LocalDate=as.Date(BB_140_2010_final$LocalDate,format="%Y-%m-%d")
BB_140_2010_final$Year = format(BB_140_2010_final$LocalDate, "%Y")
BB_140_2010_final$julianday = yday(BB_140_2010_final$LocalDate)
BB_180_2010_final$LocalDate=as.Date(BB_180_2010_final$LocalDate,format="%Y-%m-%d")
BB_180_2010_final$Year = format(BB_180_2010_final$LocalDate, "%Y")
BB_180_2010_final$julianday = yday(BB_180_2010_final$LocalDate)

BB_100_2011_final$LocalDate=as.Date(BB_100_2011_final$LocalDate,format="%Y-%m-%d")
BB_100_2011_final$Year = format(BB_100_2011_final$LocalDate, "%Y")
BB_100_2011_final$julianday = yday(BB_100_2011_final$LocalDate)
BB_140_2011_final$LocalDate=as.Date(BB_140_2011_final$LocalDate,format="%Y-%m-%d")
BB_140_2011_final$Year = format(BB_140_2011_final$LocalDate, "%Y")
BB_140_2011_final$julianday = yday(BB_140_2011_final$LocalDate)
BB_180_2011_final$LocalDate=as.Date(BB_180_2011_final$LocalDate,format="%Y-%m-%d")
BB_180_2011_final$Year = format(BB_180_2011_final$LocalDate, "%Y")
BB_180_2011_final$julianday = yday(BB_180_2011_final$LocalDate)

BS_100_2011_final$LocalDate=as.Date(BS_100_2011_final$LocalDate,format="%Y-%m-%d")
BS_100_2011_final$Year = format(BS_100_2011_final$LocalDate, "%Y")
BS_100_2011_final$julianday = yday(BS_100_2011_final$LocalDate)
BS_140_2011_final$LocalDate=as.Date(BS_140_2011_final$LocalDate,format="%Y-%m-%d")
BS_140_2011_final$Year = format(BS_140_2011_final$LocalDate, "%Y")
BS_140_2011_final$julianday = yday(BS_140_2011_final$LocalDate)
BS_180_2011_final$LocalDate=as.Date(BS_180_2011_final$LocalDate,format="%Y-%m-%d")
BS_180_2011_final$Year = format(BS_180_2011_final$LocalDate, "%Y")
BS_180_2011_final$julianday = yday(BS_180_2011_final$LocalDate)

BB_100_2012_final$LocalDate=as.Date(BB_100_2012_final$LocalDate,format="%Y-%m-%d")
BB_100_2012_final$Year = format(BB_100_2012_final$LocalDate, "%Y")
BB_100_2012_final$julianday = yday(BB_100_2012_final$LocalDate)
BB_140_2012_final$LocalDate=as.Date(BB_140_2012_final$LocalDate,format="%Y-%m-%d")
BB_140_2012_final$Year = format(BB_140_2012_final$LocalDate, "%Y")
BB_140_2012_final$julianday = yday(BB_140_2012_final$LocalDate)
BB_180_2012_final$LocalDate=as.Date(BB_180_2012_final$LocalDate,format="%Y-%m-%d")
BB_180_2012_final$Year = format(BB_180_2012_final$LocalDate, "%Y")
BB_180_2012_final$julianday = yday(BB_180_2012_final$LocalDate)

BS_100_2012_final$LocalDate=as.Date(BS_100_2012_final$LocalDate,format="%Y-%m-%d")
BS_100_2012_final$Year = format(BS_100_2012_final$LocalDate, "%Y")
BS_100_2012_final$julianday = yday(BS_100_2012_final$LocalDate)
BS_140_2012_final$LocalDate=as.Date(BS_140_2012_final$LocalDate,format="%Y-%m-%d")
BS_140_2012_final$Year = format(BS_140_2012_final$LocalDate, "%Y")
BS_140_2012_final$julianday = yday(BS_140_2012_final$LocalDate)
BS_180_2012_final$LocalDate=as.Date(BS_180_2012_final$LocalDate,format="%Y-%m-%d")
BS_180_2012_final$Year = format(BS_180_2012_final$LocalDate, "%Y")
BS_180_2012_final$julianday = yday(BS_180_2012_final$LocalDate)

BB_100_2013_final$LocalDate=as.Date(BB_100_2013_final$LocalDate,format="%Y-%m-%d")
BB_100_2013_final$Year = format(BB_100_2013_final$LocalDate, "%Y")
BB_100_2013_final$julianday = yday(BB_100_2013_final$LocalDate)
BB_140_2013_final$LocalDate=as.Date(BB_140_2013_final$LocalDate,format="%Y-%m-%d")
BB_140_2013_final$Year = format(BB_140_2013_final$LocalDate, "%Y")
BB_140_2013_final$julianday = yday(BB_140_2013_final$LocalDate)
BB_180_2013_final$LocalDate=as.Date(BB_180_2013_final$LocalDate,format="%Y-%m-%d")
BB_180_2013_final$Year = format(BB_180_2013_final$LocalDate, "%Y")
BB_180_2013_final$julianday = yday(BB_180_2013_final$LocalDate)

BS_100_2013_final$LocalDate=as.Date(BS_100_2013_final$LocalDate,format="%Y-%m-%d")
BS_100_2013_final$Year = format(BS_100_2013_final$LocalDate, "%Y")
BS_100_2013_final$julianday = yday(BS_100_2013_final$LocalDate)
BS_140_2013_final$LocalDate=as.Date(BS_140_2013_final$LocalDate,format="%Y-%m-%d")
BS_140_2013_final$Year = format(BS_140_2013_final$LocalDate, "%Y")
BS_140_2013_final$julianday = yday(BS_140_2013_final$LocalDate)
BS_180_2013_final$LocalDate=as.Date(BS_180_2013_final$LocalDate,format="%Y-%m-%d")
BS_180_2013_final$Year = format(BS_180_2013_final$LocalDate, "%Y")
BS_180_2013_final$julianday = yday(BS_180_2013_final$LocalDate)

BB_100_2014_final$LocalDate=as.Date(BB_100_2014_final$LocalDate,format="%Y-%m-%d")
BB_100_2014_final$Year = format(BB_100_2014_final$LocalDate, "%Y")
BB_100_2014_final$julianday = yday(BB_100_2014_final$LocalDate)
BB_140_2014_final$LocalDate=as.Date(BB_140_2014_final$LocalDate,format="%Y-%m-%d")
BB_140_2014_final$Year = format(BB_140_2014_final$LocalDate, "%Y")
BB_140_2014_final$julianday = yday(BB_140_2014_final$LocalDate)
BB_180_2014_final$LocalDate=as.Date(BB_180_2014_final$LocalDate,format="%Y-%m-%d")
BB_180_2014_final$Year = format(BB_180_2014_final$LocalDate, "%Y")
BB_180_2014_final$julianday = yday(BB_180_2014_final$LocalDate)

BS_100_2014_final$LocalDate=as.Date(BS_100_2014_final$LocalDate,format="%Y-%m-%d")
BS_100_2014_final$Year = format(BS_100_2014_final$LocalDate, "%Y")
BS_100_2014_final$julianday = yday(BS_100_2014_final$LocalDate)
BS_140_2014_final$LocalDate=as.Date(BS_140_2014_final$LocalDate,format="%Y-%m-%d")
BS_140_2014_final$Year = format(BS_140_2014_final$LocalDate, "%Y")
BS_140_2014_final$julianday = yday(BS_140_2014_final$LocalDate)
BS_180_2014_final$LocalDate=as.Date(BS_180_2014_final$LocalDate,format="%Y-%m-%d")
BS_180_2014_final$Year = format(BS_180_2014_final$LocalDate, "%Y")
BS_180_2014_final$julianday = yday(BS_180_2014_final$LocalDate)

BB_100_2015_final$LocalDate=as.Date(BB_100_2015_final$LocalDate,format="%Y-%m-%d")
BB_100_2015_final$Year = format(BB_100_2015_final$LocalDate, "%Y")
BB_100_2015_final$julianday = yday(BB_100_2015_final$LocalDate)
BB_140_2015_final$LocalDate=as.Date(BB_140_2015_final$LocalDate,format="%Y-%m-%d")
BB_140_2015_final$Year = format(BB_140_2015_final$LocalDate, "%Y")
BB_140_2015_final$julianday = yday(BB_140_2015_final$LocalDate)
BB_180_2015_final$LocalDate=as.Date(BB_180_2015_final$LocalDate,format="%Y-%m-%d")
BB_180_2015_final$Year = format(BB_180_2015_final$LocalDate, "%Y")
BB_180_2015_final$julianday = yday(BB_180_2015_final$LocalDate)

BS_100_2015_final$LocalDate=as.Date(BS_100_2015_final$LocalDate,format="%Y-%m-%d")
BS_100_2015_final$Year = format(BS_100_2015_final$LocalDate, "%Y")
BS_100_2015_final$julianday = yday(BS_100_2015_final$LocalDate)
BS_140_2015_final$LocalDate=as.Date(BS_140_2015_final$LocalDate,format="%Y-%m-%d")
BS_140_2015_final$Year = format(BS_140_2015_final$LocalDate, "%Y")
BS_140_2015_final$julianday = yday(BS_140_2015_final$LocalDate)
BS_180_2015_final$LocalDate=as.Date(BS_180_2015_final$LocalDate,format="%Y-%m-%d")
BS_180_2015_final$Year = format(BS_180_2015_final$LocalDate, "%Y")
BS_180_2015_final$julianday = yday(BS_180_2015_final$LocalDate)

BB_100_2016_final$LocalDate=as.Date(BB_100_2016_final$LocalDate,format="%Y-%m-%d")
BB_100_2016_final$Year = format(BB_100_2016_final$LocalDate, "%Y")
BB_100_2016_final$julianday = yday(BB_100_2016_final$LocalDate)
BB_140_2016_final$LocalDate=as.Date(BB_140_2016_final$LocalDate,format="%Y-%m-%d")
BB_140_2016_final$Year = format(BB_140_2016_final$LocalDate, "%Y")
BB_140_2016_final$julianday = yday(BB_140_2016_final$LocalDate)
BB_180_2016_final$LocalDate=as.Date(BB_180_2016_final$LocalDate,format="%Y-%m-%d")
BB_180_2016_final$Year = format(BB_180_2016_final$LocalDate, "%Y")
BB_180_2016_final$julianday = yday(BB_180_2016_final$LocalDate)

BS_100_2016_final$LocalDate=as.Date(BS_100_2016_final$LocalDate,format="%Y-%m-%d")
BS_100_2016_final$Year = format(BS_100_2016_final$LocalDate, "%Y")
BS_100_2016_final$julianday = yday(BS_100_2016_final$LocalDate)
BS_140_2016_final$LocalDate=as.Date(BS_140_2016_final$LocalDate,format="%Y-%m-%d")
BS_140_2016_final$Year = format(BS_140_2016_final$LocalDate, "%Y")
BS_140_2016_final$julianday = yday(BS_140_2016_final$LocalDate)
BS_180_2016_final$LocalDate=as.Date(BS_180_2016_final$LocalDate,format="%Y-%m-%d")
BS_180_2016_final$Year = format(BS_180_2016_final$LocalDate, "%Y")
BS_180_2016_final$julianday = yday(BS_180_2016_final$LocalDate)

BB_100_2017_final$LocalDate=as.Date(BB_100_2017_final$LocalDate,format="%Y-%m-%d")
BB_100_2017_final$Year = format(BB_100_2017_final$LocalDate, "%Y")
BB_100_2017_final$julianday = yday(BB_100_2017_final$LocalDate)
BB_140_2017_final$LocalDate=as.Date(BB_140_2017_final$LocalDate,format="%Y-%m-%d")
BB_140_2017_final$Year = format(BB_140_2017_final$LocalDate, "%Y")
BB_140_2017_final$julianday = yday(BB_140_2017_final$LocalDate)
BB_180_2017_final$LocalDate=as.Date(BB_180_2017_final$LocalDate,format="%Y-%m-%d")
BB_180_2017_final$Year = format(BB_180_2017_final$LocalDate, "%Y")
BB_180_2017_final$julianday = yday(BB_180_2017_final$LocalDate)

BS_100_2017_final$LocalDate=as.Date(BS_100_2017_final$LocalDate,format="%Y-%m-%d")
BS_100_2017_final$Year = format(BS_100_2017_final$LocalDate, "%Y")
BS_100_2017_final$julianday = yday(BS_100_2017_final$LocalDate)
BS_140_2017_final$LocalDate=as.Date(BS_140_2017_final$LocalDate,format="%Y-%m-%d")
BS_140_2017_final$Year = format(BS_140_2017_final$LocalDate, "%Y")
BS_140_2017_final$julianday = yday(BS_140_2017_final$LocalDate)
BS_180_2017_final$LocalDate=as.Date(BS_180_2017_final$LocalDate,format="%Y-%m-%d")
BS_180_2017_final$Year = format(BS_180_2017_final$LocalDate, "%Y")
BS_180_2017_final$julianday = yday(BS_180_2017_final$LocalDate)

BB_100_2018_final$LocalDate=as.Date(BB_100_2018_final$LocalDate,format="%Y-%m-%d")
BB_100_2018_final$Year = format(BB_100_2018_final$LocalDate, "%Y")
BB_100_2018_final$julianday = yday(BB_100_2018_final$LocalDate)
BB_140_2018_final$LocalDate=as.Date(BB_140_2018_final$LocalDate,format="%Y-%m-%d")
BB_140_2018_final$Year = format(BB_140_2018_final$LocalDate, "%Y")
BB_140_2018_final$julianday = yday(BB_140_2018_final$LocalDate)
BB_180_2018_final$LocalDate=as.Date(BB_180_2018_final$LocalDate,format="%Y-%m-%d")
BB_180_2018_final$Year = format(BB_180_2018_final$LocalDate, "%Y")
BB_180_2018_final$julianday = yday(BB_180_2018_final$LocalDate)

BS_100_2018_final$LocalDate=as.Date(BS_100_2018_final$LocalDate,format="%Y-%m-%d")
BS_100_2018_final$Year = format(BS_100_2018_final$LocalDate, "%Y")
BS_100_2018_final$julianday = yday(BS_100_2018_final$LocalDate)
BS_140_2018_final$LocalDate=as.Date(BS_140_2018_final$LocalDate,format="%Y-%m-%d")
BS_140_2018_final$Year = format(BS_140_2018_final$LocalDate, "%Y")
BS_140_2018_final$julianday = yday(BS_140_2018_final$LocalDate)
BS_180_2018_final$LocalDate=as.Date(BS_180_2018_final$LocalDate,format="%Y-%m-%d")
BS_180_2018_final$Year = format(BS_180_2018_final$LocalDate, "%Y")
BS_180_2018_final$julianday = yday(BS_180_2018_final$LocalDate)

#Plotting-----
par(mfrow=c(3,2))
merged_meanDens_BS_100_2010<-meanDensityByWeek(BS_100_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black', cex=2)
merged_meanDens_BS_140_2010<-meanDensityByWeek(BS_140_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2010<-meanDensityByWeek(BS_180_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2010<-meanDensityByWeek(BB_100_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2010<-meanDensityByWeek(BB_140_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2010<-meanDensityByWeek(BB_180_2010_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2011<-meanDensityByWeek(BS_100_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2011<-meanDensityByWeek(BS_140_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2011<-meanDensityByWeek(BS_180_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2011<-meanDensityByWeek(BB_100_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2011<-meanDensityByWeek(BB_140_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2011<-meanDensityByWeek(BB_180_2011_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)
plot(10,10)

merged_meanDens_BS_100_2012<-meanDensityByWeek(BS_100_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2012<-meanDensityByWeek(BS_140_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2012<-meanDensityByWeek(BS_180_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2012<-meanDensityByWeek(BB_100_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2012<-meanDensityByWeek(BB_140_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2012<-meanDensityByWeek(BB_180_2012_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)
plot(10,10)
merged_meanDens_BS_100_2013<-meanDensityByWeek(BS_100_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2013<-meanDensityByWeek(BS_140_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2013<-meanDensityByWeek(BS_180_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2013<-meanDensityByWeek(BB_100_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2013<-meanDensityByWeek(BB_140_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2013<-meanDensityByWeek(BB_180_2013_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2014<-meanDensityByWeek(BS_100_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2014<-meanDensityByWeek(BS_140_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2014<-meanDensityByWeek(BS_180_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2014<-meanDensityByWeek(BB_100_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2014<-meanDensityByWeek(BB_140_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2014<-meanDensityByWeek(BB_180_2014_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2015<-meanDensityByWeek(BS_100_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2015<-meanDensityByWeek(BS_140_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2015<-meanDensityByWeek(BS_180_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2015<-meanDensityByWeek(BB_100_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2015<-meanDensityByWeek(BB_140_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2015<-meanDensityByWeek(BB_180_2015_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2016<-meanDensityByWeek(BS_100_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2016<-meanDensityByWeek(BS_140_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2016<-meanDensityByWeek(BS_180_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2016<-meanDensityByWeek(BB_100_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2016<-meanDensityByWeek(BB_140_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2016<-meanDensityByWeek(BB_180_2016_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2017<-meanDensityByWeek(BS_100_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2017<-meanDensityByWeek(BS_140_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2017<-meanDensityByWeek(BS_180_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2017<-meanDensityByWeek(BB_100_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2017<-meanDensityByWeek(BB_140_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2017<-meanDensityByWeek(BB_180_2017_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BS_100_2018<-meanDensityByWeek(BS_100_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BS_140_2018<-meanDensityByWeek(BS_140_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BS_180_2018<-meanDensityByWeek(BS_180_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)

merged_meanDens_BB_100_2018<-meanDensityByWeek(BB_100_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black')
merged_meanDens_BB_140_2018<-meanDensityByWeek(BB_140_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='blue',lty=2)
merged_meanDens_BB_180_2018<-meanDensityByWeek(BB_180_2018_final,ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=F,color='red',lty=3)


#-----

# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)



