
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

par(mfrow=c(5,2))
for (i in 2010:2018){
  Tree<-site_filter(i, "BB", 50)%>%
  group_by(PlantSpecies,julianweek)%>%
   summarize(count=n())%>%
  spread(PlantSpecies, count)
  plot(x=Tree$julianweek, y=Tree$`American chestnut`,xlab="Julian Week", ylab="nSurveys", type="b")
  points(x=Tree$julianweek, y=Tree$`Red maple`, col="red", type ="b")
  points(x=Tree$julianweek, y=Tree$`Red oak`, col="blue", type="b")
  points(x=Tree$julianweek, y=Tree$`Striped maple`, col="green", type= "b")
}

par(mfrow=c(3,2))
for (i in 2010:2015){
Tree<-site_filter(i, "BS", 50)%>%
  group_by(PlantSpecies,julianweek)%>%
  summarize(count=n())  
plot(x=Tree$julianweek,y=Tree$count, xlab="Julian Week", ylab="nSurveys", type="b")
}


par(mfrow=c(3,2))
cow_plots<-for (i in 2010:2018){
  tryCatch({
    meanDensityByWeek(surveyData = site_filter(i,"BS",50),plot = TRUE,new=TRUE, xlab="Julian Week", ylab="Frac of Surveys")
    meanDensityByWeek(surveyData = site_filter(i,"BS",100),plot = TRUE,new=FALSE, color='blue', lty=2)
    meanDensityByWeek(surveyData = site_filter(i,"BB",50),plot = TRUE,new=TRUE,xlab="Julian Week", ylab="Frac of Surveys")
    meanDensityByWeek(surveyData = site_filter(i,"BB",100),plot = TRUE,new=FALSE, color='blue', lty=2)
 
    },error=function(e){}
  )
}


for (tree in c(American-Chestnut, Striped-Maple, Red-Oak, Red-Maple)){
  
  
}

# Write files
#write.table(branches[, !names(branches) %in% "Branch"], "Plants_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowsurvs[, !names(cowsurvs) %in% "Branch"], "Survey_Coweeta_2010_BB.txt", sep = '\t', row.names = F)
#write.table(cowarths, "ArthropodSighting_Coweeta_2010_BB.txt", sep = '\t', row.names = F)


# BS 2002-2018, BB 2003-2018, RK 2002-2008
# Roughly twice as many surveys were conducted at BS and BB in 2012

#TreeSpecies
"8" (1081)
"9" (554)
