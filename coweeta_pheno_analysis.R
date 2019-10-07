
source('data/formatting_coweeta.r')
source('C:/git/caterpillars-analysis-public/code/analysis_functions.r')

site_filter<-function(field_year,field_plot,threshold_value){
  filter<-cow_filter(field_year,field_plot,threshold_value)
  fil<-cow_fil(field_year,field_plot,filter)
  cow_surv<-coweeta_surveys(fil)
  cow_arth<-coweeta_arths(fil,cow_surv)
  merged<-merge_fun(cow_surv,cow_arth)
  date<-date_change(merged)
  plot<-meanDensityByWeek(surveyData = date, ordersToInclude = "All",minLength = 0,jdRange=c(1,365),outlierCount=10000, plot=TRUE, plotVar="fracSurveys", minSurveyCoverage = 0, allDates=TRUE, new=TRUE, color='black', cex=2)
  return(plot)
   }

par(mfrow=c(3,2))
BB_100_2010<-site_filter(2010,"BB",100)
BB_140_2010<-site_filter(2010,"BB",140)
BB_180_2010<-site_filter(2010,"BB",180)

BS_100_2010<-site_filter(2010,"BS",100)
BS_140_2010<-site_filter(2010,"BS",140)
BS_180_2010<-site_filter(2010,"BS",180)

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

BB_100_2010_merged<-cow_fil(2010,"BB",BB_100_2010_filter)
BS_100_2010_merged<-cow_fil(2010,"BS",BS_100_2010_filter)
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



df<-date_change(BB_100_2010)

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
