# Convert UGA Coweeta Caterpillars dataset into format of Caterpillars Count!
# --original file provided by Bob Cooper, Coweeta UGA Caterpillar data.xlsx
 
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

source('../caterpillars-analysis-public/code/analysis_functions.r')

# Read in data, clean up leading/trailing spaces, weird symbols
cats = read.table('data/Coweeta_cats.txt', header = T, sep = '\t', fill = TRUE, stringsAsFactors = FALSE) %>%
  filter(Plot != "") %>%
  mutate(Point = trimws(Point)) %>%
  mutate(Point = gsub("\v", "", Point)) %>%
  mutate(CaterpillarFamily = trimws(CaterpillarFamily)) %>%
  mutate(CaterpillarFamily = gsub("\v", "", CaterpillarFamily),Comments = gsub("\v", "", Comments))

catcomments = dplyr::count(cats, Comments)
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
  dplyr::select(Branch, ID, SiteFK, Circle, Orientation, Code, Species)


# surveys

multSurveyRecs = catplus %>%
  dplyr::select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Comments) %>%
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
  dplyr::select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Notes)

# surveys table for Coweeta data; 62097 recs

cowplusnotes = left_join(catplus, dups019, by = c('Year', 'Plot', 'Yearday', 'Point', 'TreeSpecies', 'Sample')) %>%
  mutate(Notes = ifelse(is.na(Notes), Comments, Notes))

cowplusnotes$surveyed<-ifelse(cowplusnotes$NumCaterpillars>=0,"1",NA)


#Filter out for threshold value, plot, and year
cow_filter<-cowplusnotes%>%
  filter(Year>2009, Plot %in% c("BB", "BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  mutate(JulianWeek=7*floor((Yearday)/7)+4)


#Cow_survs converting to Caterpillars Count format------
#Setting threshold value of 50
cow_thresh<-cowplusnotes%>%
  filter(Year>2009, Plot%in% c("BB","BS"), TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))%>%
  dplyr::select(Year,Yearday,Plot,Point,TreeSpecies,Sample, NumCaterpillars)%>%
  distinct()%>%
  group_by(Year,Yearday)%>%
  tally()%>%
  rename(nSurveys=n)%>%
  mutate(JulianWeek=7*floor((Yearday)/7)+4)%>%
  #aggregate(cow_thresh$nSurveys,by=list(Year=cow_thresh$Year,cow_thresh$JulianWeek=JWeek),FUN=sum)
  group_by(Year,JulianWeek)%>%
  mutate(nJulianWeekSurvey=sum(nSurveys))%>%
  filter(nJulianWeekSurvey>50)

cow_pheno<-left_join(cowplusnotes,cow_thresh,by=c("Year","Yearday"))%>%
  drop_na(JulianWeek)%>%
  filter(TreeSpecies%in% c("American-Chestnut", "Striped-Maple", "Red-Oak", "Red-Maple"))

coweeta_surveys<-cow_pheno %>%
  dplyr::select(Year, Plot, Yearday, Point, TreeSpecies, Sample, Notes) %>%
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
  dplyr::select(ID, SubmissionTimestamp, UserFKOfObserver, PlantFK, LocalDate, LocalTime, ObservationMethod, Notes, 
         WetLeaves, PlantSpecies, NumberOfLeaves, AverageLeafLength, HerbivoryScore, SubmittedThroughApp,
         MinimumTemperature, MaximumTemperature, NeedToSendToSciStarter, CORRESPONDING_OLD_DATABASE_SURVEY_ID, Branch)



# 69 records with 2 or more "survey events" per branch/date
#multSurveyRecs2 = cowsurvs %>%
#  count(ID, PlantFK, LocalDate, Notes) %>%
#  filter(n > 1)
  

# arthropods table------
coweeta_arths<- cow_pheno%>%
  mutate(Branch = paste(Plot, Point, TreeSpecies, Sample, sep = "_"),
         LocalDate = as.Date(Yearday, as.Date(paste(Year, "-01-01", sep = "")))) %>%
  left_join(coweeta_surveys[, c('Branch', 'ID', 'LocalDate', 'Notes')], by = c('Branch', 'LocalDate', 'Notes')) %>%
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
         Group = ifelse(CaterpillarFamily == "Sawfly", "bee", Group),
         Biomass_mg=CaterpillarBiomass_ma) %>%
  dplyr::select(ID, SurveyFK, Group, Length, Quantity, PhotoURL, Notes, Hairy, Rolled, Tented, Sawfly, BeetleLarva, NeedToSendToINaturalist,Biomass_mg)



#Sites
greenup = raster("data/inca_midgup_median_nad83_02deg.tif")
sites<-data.frame(Site=c('Coweeta_BB', 'Coweeta_BS'),Latitude=c(35.041667, 35.025), Longitude=c(-83.475,-83.458333))
sites$medianGreenup = round(raster::extract(greenup, sites[, c('Longitude', 'Latitude')]))


# Write files
#write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)



#Merge arth, surv, and sites, used as final dataset format
  final_cow_set<-coweeta_surveys%>%
    left_join(coweeta_arths, by= c('ID'= 'SurveyFK'))%>%
    rename(arthID=ID.y)%>%
    mutate(LocalDate = as.Date(LocalDate,format="%Y-%m-%d"))%>%
    mutate(Year = format(LocalDate, "%Y"))%>%
    mutate(julianday = yday(LocalDate)-1)%>%
    mutate(julianweek=7*floor((julianday)/7)+4)%>% 
    mutate(site="Coweeta", foo=substring(Branch,0,2))%>%
    unite(Name, site:foo,remove=TRUE,sep="_")%>%
    mutate(Plot=substring(Branch, 0, 2))%>%
    left_join(sites,by=c('Name'='Site'))

  
  updatedphenosum = function(fullDataset, # fullDataset format
                          postGreenupBeg = 40,     # number of days post-greenup marking the beginning of the time window
                          postGreenupEnd = 75,     # number of days post-greenup marking the end of the time window
                          fullWindowBeg = 136,     # julian day of the beginning of a specified time window (default May 15)
                          fullWindowEnd = 182,     # julian day of the end of a specified time window (default July 31)
                          minNumWeeks = 5,         # minimum number of weeks of survey data to calculate pheno summaries
                          ...) {
    
    years = unique(fullDataset$Year)
    output = data.frame(Name = NA, Year = NA, medianGreenup = NA, minJulianWeek = NA, maxJulianWeek = NA, totalSurveys = NA,
                        numGoodWeeks = NA, numWeeksPostSolsticeWindow = NA, numWeeksPostGreenupWindow = NA, 
                        pctSolstice = NA, densSolstice = NA, massSolstice= NA, pctPostGU = NA, densPostGU = NA, massPostGU = NA,
                        pctPeakDate = NA, densPeakDate = NA, massPeakDate = NA, pctPeakDateWindow = NA, densPeakDateWindow = NA,
                        massPeakDateWindow = NA, pctPeakDateGreenupWindow = NA, densPeakDateGreenupWindow = NA, 
                        massPeakDateGreenupWindow = NA, pctRollingPeakDateWindow = NA, densRollingPeakDateWindow = NA, massRollingPeakDateWindow = NA)
    
    for (y in years) {
      yearFilteredDataset = dplyr::filter(final_cow_set, Year == y)
      uniqueSites = unique(yearFilteredDataset$Name)
      
      for (site in uniqueSites) {
        siteYearFilteredDataset = dplyr::filter(yearFilteredDataset, Name==site)
        
        pheno = meanDensityByWeek(siteYearFilteredDataset, allDates = TRUE, plot = FALSE, ...)
        
        if (nrow(pheno) >= minNumWeeks) {
          
          greenup = siteYearFilteredDataset$medianGreenup[1]
          
          siteoutput = pheno %>%
            # calculate 3-week rolling averages
            mutate(rollingPct = frollmean(fracSurveys, 3, align = "center"),
                   rollingDensity = frollmean(meanDensity, 3, align = "center"),
                   rollingBiomass = frollmean(meanBiomass, 3, align = "center")) %>%
            summarize(# mean for the month of July
              Name = site,
              Year = y,
              medianGreenup = greenup,
              minJulianWeek = min(julianweek),
              maxJulianWeek = max(julianweek),
              totalSurveys = sum(nSurveys),
              numGoodWeeks = sum(okWeek == 1),
              numWeeksPostSolsticeWindow = sum(okWeek[julianweek >= 172 & julianweek <= 202] == 1),
              numWeeksPostGreenupWindow = sum(okWeek[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)] == 1),
              pctSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                   mean(fracSurveys[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
              densSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                    mean(meanDensity[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
              massSolstice = ifelse(sum(julianweek >= 172 & julianweek <= 202) > 0, 
                                    mean(meanBiomass[julianweek >= 172 & julianweek <= 202], na.rm = TRUE), NA),
              # mean for the post-greenup window specified
              pctPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                                 mean(fracSurveys[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
              densPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                                  mean(meanDensity[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
              massPostGU = ifelse(sum(julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)) > 0, 
                                  mean(meanBiomass[julianweek >= (greenup + postGreenupBeg) & julianweek <= (greenup + postGreenupEnd)], na.rm = TRUE), NA),
              # peak date of the time-series unconstrained
              pctPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                   julianweek[fracSurveys == max(fracSurveys, na.rm = TRUE)][1]),
              densPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                    julianweek[meanDensity == max(meanDensity, na.rm = TRUE)][1]),
              massPeakDate = ifelse(sum(totalCount) == 0, NA, 
                                    julianweek[meanBiomass == max(meanBiomass, na.rm = TRUE)][1]),
              # peak date within a specified, hard-coded window; [1] selects the 1st date if multiple dates have the same peak value
              pctPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                         julianweek[fracSurveys == max(fracSurveys[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
              densPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                          julianweek[meanDensity == max(meanDensity[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
              massPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                          julianweek[meanBiomass == max(meanBiomass[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE)][1]),
              # peak date between the beginning of the post-greenup window and the end of July; [1] selects the 1st date if multiple dates have the same peak value
              pctPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                                julianweek[fracSurveys == max(fracSurveys[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
              densPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                                 julianweek[meanDensity == max(meanDensity[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
              massPeakDateGreenupWindow = ifelse(sum(totalCount) == 0, NA, 
                                                 julianweek[meanBiomass == max(meanBiomass[julianweek >= (greenup + postGreenupBeg) & julianweek <= 212], na.rm = TRUE)][1]),
              # peak date for the 3-week rolling average between the beginning of the post-greenup window and the end of July;
              #    -1 at the end to select the middle (rather than end) of the 3-week window
              pctRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                                julianweek[which(rollingPct == max(rollingPct[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]),
              densRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                                 julianweek[which(rollingDensity == max(rollingDensity[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]),
              massRollingPeakDateWindow = ifelse(sum(totalCount) == 0, NA, 
                                                 julianweek[which(rollingBiomass == max(rollingBiomass[julianweek >= fullWindowBeg & julianweek <= fullWindowEnd], na.rm = TRUE))][1]))
          
          output = rbind(output, siteoutput)        
        }
      } # end site
    } # end year
    out = output[-1, ]
    out[is.na(out)] = NA # converts NaN's to NA's
    return(output[-1, ])
    
  }
  phenometrics<-updatedphenosum(final_cow_set,  postGreenupBeg = 40,     # number of days post-greenup marking the beginning of the time window
                    postGreenupEnd = 75,     # number of days post-greenup marking the end of the time window
                    fullWindowBeg = 136,     # julian day of the beginning of a specified time window (default May 15)
                    fullWindowEnd = 182,     # julian day of the end of a specified time window (default July 31)
                    minNumWeeks = 0)
write.table(phenometrics, "Coweeta_Phenometrics_136_182.txt", row.names=F, sep="\t", quote=FALSE)
write.table(final_cow_set,"Coweeta_Filtered_136_182.txt", sep='\t',row.names=F)

  
#Changed yday(localDate) to subtract 1 to account for weird date shift

# juliandate <- merged_set%>%
#    mutate(LocalDate = as.Date(LocalDate,format="%Y-%m-%d"))%>%
#    mutate(Year = format(LocalDate, "%Y"))%>%
#    mutate(julianday = yday(LocalDate)-1)%>%
#    mutate(julianweek=7*floor((julianday)/7)+4)



#site_filter<-function(threshold_value){
 # filter<-cow_filter(threshold_value)
 # fil<-cow_fil(filter) this is now just cow_filter, does the same thing 
 # cow_surv<-coweeta_surveys(fil)
 # cow_arth<-coweeta_arths(fil,cow_surv)
 # merged<-merge_fun(cow_surv,cow_arth)
 # date<-date_change(merged)

#site_filter(0)

  


#Need to reorganize this and make the process of reading in these files more clear

#-----

# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
#"8" (1081)
#"9" (554)



