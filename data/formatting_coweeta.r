# Convert UGA Coweeta Caterpillars dataset into format of Caterpillars Count!
# --original file provided by Bob Cooper, Coweeta UGA Caterpillar data.xlsx
 
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
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
  filter(Plot == Plot) %>%
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

#for(i in 2010:2018){
#  g<-freq_plot(i,"BB")
#  print(g)
#}


samp_days<-function(field_year,field_plot){
  coweeta_data<-cowplusnotes%>%
    filter(cowplusnotes$Year==field_year,cowplusnotes$Plot==field_plot)%>%
    select(Plot, Yearday, Point,TreeSpecies, Sample)%>%
    distinct()%>%
    group_by(Plot,Point, Yearday)%>%
    summarize(n=n())%>%
    arrange(Yearday)%>%
    mutate(gridLetter=substr(Point,1,1),
           gridY=as.numeric(substr(Point,2,nchar(Point))),
           gridX=which(LETTERS == gridLetter))
  par(mfrow = c(6, 5), mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  pdf(paste0("coweeta_plots_",field_year,"_",field_plot,".pdf"))
  for (i in unique(coweeta_data$Yearday)) {
    tmp = filter(coweeta_data, Yearday == i)
    plots<-ggplot(tmp,aes(x=gridX,y=gridY))+geom_point(aes(size=n))+xlim(0,26)+ylim(0,max(coweeta_data$gridY))+facet_wrap(~Yearday,ncol=5)
    #print(plots)
    grid.arrange(plots)
  }
 
  dev.off()
  return(coweeta_data)
}

BBsamp10<-samp_days(2010,"BB")
BBsamp11<-samp_days(2011,"BB")
BBsamp12<-samp_days(2012,"BB")
BSsamp12<-samp_days(2012,"BS")




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



cowsurvs = cowplusnotes%>%
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
         

# 69 records with 2 or more "survey events" per branch/date
multSurveyRecs2 = cowsurvs %>%
  count(ID, PlantFK, LocalDate, Notes) %>%
  filter(n > 1)
  

# arthropods table
cowarths = cowplusnotes %>%
  mutate(Branch = paste(Plot, Point, TreeSpecies, Sample, sep = "_"),
         LocalDate = as.Date(Yearday, as.Date(paste(Year, "-01-01", sep = "")))) %>%
  left_join(cowsurvs[, c('Branch', 'ID', 'LocalDate', 'Notes')], by = c('Branch', 'LocalDate', 'Notes')) %>%
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
  


# Write files
write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta.txt", sep = '\t', row.names = F)
write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta.txt", sep = '\t', row.names = F)
write.table(cowarths, "ArthropodSighting_Coweeta.txt", sep = '\t', row.names = F)

# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)

