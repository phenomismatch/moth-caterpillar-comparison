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


# 69 records with 2 or more "survey events" per branch/date
#multSurveyRecs2 = cowsurvs %>%
#  count(ID, PlantFK, LocalDate, Notes) %>%
#  filter(n > 1)
  

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



# Write files
#write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)

#Merge arth and surv======
merge_fun<-function(cowsurv,cowarths){
merged_set<-cowsurv%>%
  left_join(cowarths, by= c('ID'= 'SurveyFK'))%>%
  rename(arthID=ID.y)

}

#Function to filter out for threshold value, plot, and year
cow_filter<-function(threshold_value){
  cow_WeeklySurv<-cowplusnotes%>%
    filter(Year>2009, Plot %in% c("BB", "BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
    select(Year,Yearday,Plot,Point,TreeSpecies,Sample)%>%
    distinct()%>%
    group_by(Year,Yearday)%>%
    tally()%>%
    rename(nSurveys=n)%>%
    mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
    group_by(JulianWeek)%>%
    mutate(WeeklySurv=sum(nSurveys))%>%
    filter(WeeklySurv>=threshold_value)
}

cow_fil<-function(site_thresh_year_filter){
  coweetanotes<-cowplusnotes%>%
    filter(Year>2009, Plot %in% c("BB","BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
    left_join(site_thresh_year_filter,by=c('Year', 'Yearday'))%>%
    filter(!is.na(WeeklySurv))%>%
    subset(select=-c(surveyed,nSurveys,JulianWeek, WeeklySurv))
}


#Changed yday(localDate) to subtract 1 to account for weird date shift

date_change<-function(Final_set){
  juliandate <- Final_set%>%
    mutate(LocalDate = as.Date(LocalDate,format="%Y-%m-%d"))%>%
    mutate(Year = format(LocalDate, "%Y"))%>%
    mutate(julianday = yday(LocalDate)-1)%>%
    mutate(julianweek=7*floor((julianday)/7)+4)
}


site_filter<-function(threshold_value){
  filter<-cow_filter(threshold_value)
  fil<-cow_fil(filter)
  cow_surv<-coweeta_surveys(fil)
  cow_arth<-coweeta_arths(fil,cow_surv)
  merged<-merge_fun(cow_surv,cow_arth)
  date<-date_change(merged)
}
final_cow<-site_filter(0)

#-----

# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
#"8" (1081)
#"9" (554)



