
source('data/formatting_coweeta.r')
source('C:/git/caterpillars-analysis-public/code/analysis_functions.r')

site_filter<-function(field_year,field_plot,threshold_value){
  filter<-cow_filter(field_year,field_plot,threshold_value)
  fil<-cow_fil(field_year,field_plot,filter)
  cow_surv<-coweeta_surveys(fil)
  cow_arth<-coweeta_arths(fil,cow_surv)
  merged<-merge_fun(cow_surv,cow_arth)
  date<-date_change(merged)
}
  
   
par(mfrow=c(3,2))


BB_50_2010<-meanDensityByWeek(surveyData = site_filter(2010,"BB",50),plot = TRUE,new=TRUE)
BB_100_2010<-meanDensityByWeek(surveyData = site_filter(2010,"BB",100),plot = TRUE,new=FALSE, color='blue', lty=2)

BS_50_2010<-meanDensityByWeek(surveyData = site_filter(2010,"BS",50),plot = TRUE,new=TRUE)
BS_100_2010<-meanDensityByWeek(surveyData = site_filter(2010,"BS",100),plot = TRUE,new=FALSE, color='blue', lty=2)


# Write files
write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)


# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)
