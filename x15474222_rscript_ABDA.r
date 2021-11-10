library(tidyr)
library(dplyr)

# Working with a large data set and my R studio will not print the info I want
options(max.print=99999999)

#Load the dataset
rawresults <- read.csv("Merge_Data.csv") 
#Check size of the dataset
dim(rawresults)

#Identify the columns I will work with
rawresults.dontip <- rawresults %>%
  select("GUID", "COUNTY",
         'T11_2_T1',
         'T11_2_T2',
         'T11_2_T3',
         'T11_2_T4',
         'T11_2_T5',
         'T11_2_T6',
         'T11_2_T7',
         'T11_2_T8')
dim(rawresults.filter)

#Subset the data so that we only have data from the 3 Cities requird
cities_wide <- subset(rawresults.dontip, rawresults.filt$COUNTY =="TY" | rawresults.filt$COUNTY=="DL" )

dim(cities_wide)

#Check first few columns
head(cities_wide)

#Reshape the data so as the travel times are variables in one column
cities_long <- cities_wide %>%                                  # Apply gather function
  gather(leavetime, value, - c(GUID, COUNTY))
#Check the size of the data
dim(cities_long)
head(cities_long)


cities_long_totals <- cities_long %>%
  group_by(COUNTY, schooltransport) %>%
  summarize(sum(value)) 
write.csv(cities_long,"cities_longdontip.csv", row.names = TRUE)

citi <- read.csv("cities_long_total.csv") 
head(citi)

kruskal.test(sum.value. ~ COUNTY, data=citi)

####Kolmogorov-Smirnov- test for normal distribution
###15 min travle time 
ks.test(df1$T11_3_D1, 'pnorm', mean(df1$T11_3_D1), sd(df1$T11_3_D1))
library(nortest)
lillie.test(df1$T11_3_D1) 

#### mode of transport to work car 
ks.test(df4$T11_1_CDW, 'pnorm', mean(df4$T11_1_CDW), sd(df4$T11_1_CDW))
library(nortest)
lillie.test(df4$T11_1_CDW) 

qqplot(df1$T11_3_D1,)
?qqplot

library(ggplot2)
qplot(sample = T11_3_D1, data = df1, main = "QQ Plot for Census data", col= 'pink', xlab = 'Commute Under 15 minutes')

