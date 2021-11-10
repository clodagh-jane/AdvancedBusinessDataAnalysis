#read the file 
diet <- read.csv(file="diet database.csv",head=TRUE,sep=",")

#MEAN - calculate the mean of the 2 chosen variables from the diet data set: 
#Pre Weight
mean(diet$pre.weight) #result returned 72.5
#Height
mean(diet$Age) #result returned 39.2

#MEDIAN - calculate the median of the 2 chosen variables from the diet data set: 
#Pre Weight 
median(diet$pre.weight) #result 72
#Weight 6 weeks later
median(diet$Age) #result 39

#MODE - calculate the mode of the 2 chosen variables from the diet data set: 
#Pre Weight 
#step 1: I create a vector with weight variable
v <- c(diet$pre.weight)
#step 2: I summarize the frequency on a table
f <- table(v)
#step 3: Check what is the number that has more repetition on that table
which(f==max(f)) #result 72 & 78

#AGE
#step 1: I create a vector with my variable
v <- c(diet$Age)
#step 2: I summarize the frequency on a table
f <- table(v)
#step 3: I check what is the number that has more repetition on that table
which(f==max(f)) #results 37

#STANDARD DEVIATION - calculate the standard deviation of the 2 chosen variables from the diet data set: 
#Pre weight 
sd(diet$pre.weight) # result 8.72
#AGE
sd(diet$Age) # result 9.8

#interperate the results of the Mean, Meadian, Mode and Standar deviation for both of the variables from the Diet dataset
#Pre Weight:
#Mean = 72.5 , Medain = 72, Mode = 72 and 78, Standard Deviation = 8.72
#What these results show is that the mean median and the mode of the pre weight variable produce a very similar/the same result of 72
#the mean and median are quite similar implying that there are no major ouliers which are influencing the mean
#the standard deviation of 8.72 shows reflects how the weight differs from the average amongst the group

#Age:
#Mean = 39.2, Median = 39, Mode = 37, Standard deviation = 9.8
#these results show that the average age of the people taking part in the diets are 39 years old 
#there is very little difference between the median and the mode again reflecting that there are no ouliers influencing the mean
#and that the ages of the people who have taken part are of similar age 
#the most frequent age obsetved is 37 years old 
#the standard deviation is 9.8 meaing that theye is an average difference of 9.8yrs from the mean 



###################################################################
#PLOTS
#histogram of Pre Diet weight 
hist(x = diet$pre.weight, #the variable used for the graph 
     main= "Pre Diet Weight Histogram", #Title of the graph 
     xlab = "Weight (kg)", #label the x axis
     ylab = "Frequency", #label the y axis
     col = "pink", #colour of the bars
     border = "purple") #colour of the outline of the bars 

#this histogram shows the distribution of how much people weighed in kg before they completed their diets 
#the histogram is skewed to the right - positivley skewed, meaning there are more people weighing between 60kg - 80kg than there people who weigh 80kg - 100kg
#it also shows that most people weigh between 75kg - 80kg



### make a bar chart to see if one diet was more effective than the other 
#make a new variable of the difference between the Pre Diet weight and the new weight 
diet$weightresult <- (diet$pre.weight - diet$weight6weeks)

library(dplyr) #load the dplyr library 
DietRes <- diet %>% group_by(Diet) %>% summarise(inc = mean(weightresult)) 
#create a table showing the mean weight lost as grouped by type of diet 

library(ggplot2)
dietplot <- ggplot(aes(x = Diet, y = inc), data = DietRes) + 
  geom_bar(stat = "identity", fill = '#FF7890') +   ###bar plot and colour of the bars
  ggtitle("Average amount of weigh loss by Diet type") + ### title of graph 
  labs(y="Mean weight lost (kg)", x = "Diet type") ### x and y titles 
dietplot #load the plot

#interpretation of bar chart - based of of the average amount of weight lost as grouped by diet type,
#this bar chart shows that thoes who followed Diet type 3 lost the most weight on average, at an average of 5kg
# those who followed diet types 1 and 2 lost an average of 3kg and 3.3kg respectivley - which are quite similar results
#this bar blot can be used to show which of the diet types were most effective for weight loss
#it is important to keep in mind that the bars represent the mean wight lost as grouped by diet type 
#the mean is susceptible to outliers meaning that there is a possibility that one person who lost a lot of weigh may skew the results 



