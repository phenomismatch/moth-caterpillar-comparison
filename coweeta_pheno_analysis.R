
source('data/formatting_coweeta.r')
source('C:/git/caterpillars-analysis-public/code/analysis_functions.r')


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
    filter(WeeklySurv>=threshold_value)
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
cow_filter<-function(field_year,field_plot, site_thresh_year_filter){
    coweetanotes<-cowplusnotes%>%
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
