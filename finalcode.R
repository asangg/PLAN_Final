library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
setwd("C:/Users/alexs/Documents/plan372-sp23/final")

data=read_csv("crimedata.csv")

pop=read_csv("nypop.csv")

unique(data$BORO_NM)

#Subset population data to just 2020
pop=pop[2:6,-(3:16)]
pop=pop[1:6,-(4:8)]
pop=pop[,-1]
pop=na.omit(pop)
colnames(pop)[2]<- "Population" #Change Column header to population

#Recode Borough names to match case sensitivity 
data$BORO_NM=recode(data$BORO_NM,
                    "BRONX" = "Bronx",
                    "MANHATTAN" = "Manhattan",
                    "STATEN ISLAND" = "Staten Island",
                    "QUEENS" = "Queens",
                    "BROOKLYN" = "Brooklyn")

#Merge population data and crime data by borough 

merged <-merge(data,pop, by.x="BORO_NM",by.y="Borough")
unique(merged$BORO_NM)


#total crimes committed in each borough

a=group_by(merged, BORO_NM)%>%
  summarize(Total_Crime=n())%>%
  arrange(-Total_Crime)


#Divide by per capita
totalcrimerate <-merge(pop,a, by.x="Borough",by.y="BORO_NM")

totalcrimerate$Rate_Per_Capita=0

totalcrimerate$Rate_Per_Capita=totalcrimerate$Total_Crime/totalcrimerate$Population


totalcrimerate=totalcrimerate[order(-totalcrimerate$Rate_Per_Capita),] #sorts the total crime rate per capita





#Most Common Crime in Each Borough

totalcrimerate$Felonies=0
totalcrimerate$Felony_Percentage=0

totalcrimerate$Violations=0
totalcrimerate$Violations_Percentage=0

totalcrimerate$Misdemeanors=0
totalcrimerate$Misdemeanors_Percentage=0

totalcrimerate=totalcrimerate[order(totalcrimerate$Borough),]

#Bronx
totalcrimerate[1,5]=nrow(merged[merged$BORO_NM=="Bronx" & merged$LAW_CAT_CD=="FELONY",]) #54,840 Felonies
totalcrimerate$Felony_Percentage[1]=totalcrimerate$Felonies[1]/totalcrimerate$Total_Crime[1] #Divides total felonies by total crime in that borough, repeated with different level of offenses and boroughs down below

totalcrimerate[1,7]= totalcrimerate$Violations[1]=nrow(merged[merged$BORO_NM=="Bronx" & merged$LAW_CAT_CD=="VIOLATION",]) #33,230 Violations
totalcrimerate$Violations_Percentage[1]=totalcrimerate$Violations[1]/totalcrimerate$Total_Crime[1]


totalcrimerate[1,9]=nrow(merged[merged$BORO_NM=="Bronx" & merged$LAW_CAT_CD=="MISDEMEANOR",]) #112,423 Misdemeanors
totalcrimerate$Misdemeanors_Percentage[1]=totalcrimerate$Misdemeanors[1]/totalcrimerate$Total_Crime[1]

#Queens

totalcrimerate[4,5]=nrow(merged[merged$BORO_NM=="Queens" & merged$LAW_CAT_CD=="FELONY",]) #57,381 Felonies
totalcrimerate$Felony_Percentage[4]=totalcrimerate$Felonies[4]/totalcrimerate$Total_Crime[4]



totalcrimerate[4,7]=nrow(merged[merged$BORO_NM=="Queens" & merged$LAW_CAT_CD=="VIOLATION",]) #29,932 Violations
totalcrimerate$Violations_Percentage[4]=totalcrimerate$Violations[4]/totalcrimerate$Total_Crime[4]


totalcrimerate[4,9]=nrow(merged[merged$BORO_NM=="Queens" & merged$LAW_CAT_CD=="MISDEMEANOR",]) #94,025 Misdemeanors 
totalcrimerate$Misdemeanors_Percentage[4]=totalcrimerate$Misdemeanors[4]/totalcrimerate$Total_Crime[4]

#Manhattan
totalcrimerate[3,5]=nrow(merged[merged$BORO_NM=="Manhattan" & merged$LAW_CAT_CD=="FELONY",]) #71,630 Felonies
totalcrimerate$Felony_Percentage[3]=totalcrimerate$Felonies[3]/totalcrimerate$Total_Crime[3]

totalcrimerate[3,7]=nrow(merged[merged$BORO_NM=="Manhattan" & merged$LAW_CAT_CD=="VIOLATION",]) #29,619 Violations
totalcrimerate$Violations_Percentage[3]=totalcrimerate$Violations[3]/totalcrimerate$Total_Crime[3]

totalcrimerate[3,9]=nrow(merged[merged$BORO_NM=="Manhattan" & merged$LAW_CAT_CD=="MISDEMEANOR",]) #127,902 Misdemeanors 
totalcrimerate$Misdemeanors_Percentage[3]=totalcrimerate$Misdemeanors[3]/totalcrimerate$Total_Crime[3]


#Brooklyn
totalcrimerate[2,5]=nrow(merged[merged$BORO_NM=="Brooklyn" & merged$LAW_CAT_CD=="FELONY",]) #87,108 Felonies
totalcrimerate$Felony_Percentage[2]=totalcrimerate$Felonies[2]/totalcrimerate$Total_Crime[2]

totalcrimerate[2,7]=nrow(merged[merged$BORO_NM=="Brooklyn" & merged$LAW_CAT_CD=="VIOLATION",]) #41,674 Violations
totalcrimerate$Violations_Percentage[2]=totalcrimerate$Violations[2]/totalcrimerate$Total_Crime[2]

totalcrimerate[2,9]=nrow(merged[merged$BORO_NM=="Brooklyn" & merged$LAW_CAT_CD=="MISDEMEANOR",]) #138,656 Misdemeanors 
totalcrimerate$Misdemeanors_Percentage[2]=totalcrimerate$Misdemeanors[2]/totalcrimerate$Total_Crime[2]

#Staten Island
totalcrimerate[5,5]=nrow(merged[merged$BORO_NM=="Staten Island" & merged$LAW_CAT_CD=="FELONY",]) #9,372 Felonies
totalcrimerate$Felony_Percentage[5]=totalcrimerate$Felonies[5]/totalcrimerate$Total_Crime[5]

totalcrimerate[5,7]=nrow(merged[merged$BORO_NM=="Staten Island" & merged$LAW_CAT_CD=="VIOLATION",]) #8,625 Violations
totalcrimerate$Violations_Percentage[5]=totalcrimerate$Violations[5]/totalcrimerate$Total_Crime[5]

totalcrimerate[5,9]=nrow(merged[merged$BORO_NM=="Staten Island" & merged$LAW_CAT_CD=="MISDEMEANOR",]) #21,546 Misdemeanors 
totalcrimerate$Misdemeanors_Percentage[5]=totalcrimerate$Misdemeanors[5]/totalcrimerate$Total_Crime[5]

#Which hour are the crimes most common? 

#Split Hour from Column

merged$Most_Common_Hour_of_Crime=0#New Column

merged$Most_Common_Hour_of_Crime<-as.numeric(str_match(merged$CMPLNT_FR_TM,"(\\d+):")[,2]) #seperates hour from the time


Crime_Hours=group_by(merged, Most_Common_Hour_of_Crime)%>% #Finds and arranges the crimes in each hours are committed
  summarize(Occurences=n())%>%
  arrange(-Occurences)

counts<-merged%>% #creates a new data frame counting each hour for each borough
  group_by(BORO_NM,Most_Common_Hour_of_Crime)%>%
  summarize(Occurences=n())

common<- counts%>%
  group_by(BORO_NM)%>%
  filter(Occurences==max(Occurences))%>%
  select(BORO_NM,Most_Common_Hour_of_Crime,Occurences)

#Merge Data
totalcrimerate <-merge(totalcrimerate,common, by.x="Borough",by.y="BORO_NM")

#Format columns to two places after converting them to numeric
totalcrimerate$Rate_Per_Capita=as.numeric(totalcrimerate$Rate_Per_Capita)
totalcrimerate$Rate_Per_Capita<-round(totalcrimerate$Rate_Per_Capita, 2)


totalcrimerate$Felony_Percentage=as.numeric(totalcrimerate$Felony_Percentage)
totalcrimerate$Felony_Percentage<-round(totalcrimerate$Felony_Percentage, 2)

totalcrimerate$Violations_Percentage=as.numeric(totalcrimerate$Violations_Percentage)
totalcrimerate$Violations_Percentage<-round(totalcrimerate$Violations_Percentage, 2)

totalcrimerate$Misdemeanors_Percentage=as.numeric(totalcrimerate$Misdemeanors_Percentage)
totalcrimerate$Misdemeanors_Percentage<-round(totalcrimerate$Misdemeanors_Percentage, 2)


#Table
write.csv(totalcrimerate,"test.csv",row.names=FALSE) #save crime table as csv

#Plots

#Crime Rate Plot
ggplot(totalcrimerate,aes(x=Borough,y=Rate_Per_Capita))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=Rate_Per_Capita), vjust=-0.3,size=3.5)+
  theme_minimal()+
  labs(x="Boroughs", y="Crime Rate per Capita", title="Crime Rate per Capita of New York City Boroughs")

#Crime Type Percentages
subsettedata=totalcrimerate[,-(2:5)]
subsettedata=subsettedata[,-(7:10)]
subsettedata=subsettedata[,-(3)]
subsettedata=subsettedata[,-(4)]


#Used melt function from reshape2 package
melted<-melt(subsettedata,id.vars="Borough",variable.name = "CrimePercentage", value.name = "y")


ggplot(melted,aes(x=Borough,y=y,fill=CrimePercentage))+
  geom_bar(stat="identity",position="dodge")+ #dodge function places the bars next to each other
  geom_text(aes(label=y), position=position_dodge(width=0.9), vjust=-0.5) +#Position_dodge used to position the text labels and vjust used to adjust the vertical positioning
  theme_minimal()+
  labs(x="Boroughs", y="Crime Percentage based on the level of Offense", title="Crime Type Percentage of New York City Boroughs")




