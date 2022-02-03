
#Import Libraries used
library(gridExtra) #used for plotting histograms side-by-side
library(lubridate) #we will use a function called 'weekday' to extract days of week

#Import Data
ny = read.csv('./new-york-city.csv', sep = ',')
wash = read.csv('./washington.csv', sep = ',')
chi = read.csv('./chicago.csv', sep = ',')

#Preview what the data looks like
head(ny)
head(chi)
head(wash)

#Observe column names for each data set. Note: only Chicago and NYC have birth year and gender info.
names(ny)
names(chi)
names(wash)



#Question 1: What insights can we analyze about the riders' gender and age?
# First we can plot the rider's birth year out side-by-side in each city
grid.arrange(qplot (x=Birth.Year, data =ny, binwidth=1)+
  ggtitle("NYC Rider Birth Year"),
qplot (x=Birth.Year, data =chi, binwidth=1)+
  ggtitle("Chicago Rider Birth Year")
, nrow=1)

#From here, we can see that there are a couple of outliers, which makes the histogram skew left. 
#In both NYC and Chicago, there are at least one rider's whose birth year is before 1900.
#This probably isn't truthful, and therefore should be excluded from analysis.
#We can run a summary on birth year for each city to see how the distribution falls.

summary(ny$Birth.Year)
summary(chi$Birth.Year)

#Birth years are fine, but I think that age may be a more useful way of understanding 
#just who the bike share riders are.
#Additionally, I'm going to limit the ages from 10-85 to get rid of the outliers
#to both the left and the right.
#I'm going to add some descriptive titles to the plots as well as to both the X and Y axis.
grid.arrange(qplot (x=2017-Birth.Year, data =ny, binwidth=1, 
                    xlab = 'Ages of NYC Riders',
                    ylab = "Number of Riders") +
               ggtitle("NYC Riders by Age in 2017")+
               scale_x_continuous(limits = c(10, 85), breaks = seq(10, 85, 5))+
               facet_wrap(~Gender),
             qplot (x=2017-Birth.Year, data =chi, binwidth=1, 
                    xlab = 'Ages of Chicago Riders',
                    ylab = "Number of Riders") +
               ggtitle("Chicago Riders by Age in 2017")+
               scale_x_continuous(limits = c(10, 85), breaks = seq(10, 85, 5))+
               facet_wrap(~Gender)
             , nrow=1)

#I see that there are some blank values given for gender. We should exclude those and run the above code again.
#First we set the blank values in the two datasets to 'NA'

ny[ny==""] <- NA #setting the blanks to NA
chi[chi==""] <- NA #setting the blanks to NA

#Next we use the subset command to eliminate the NA values, and we are left only with Female and Male
grid.arrange(qplot (x=2017-Birth.Year, data = subset(ny, !is.na(Gender)), binwidth=1, 
                    xlab = 'Ages of NYC Riders',
                    ylab = "Number of Riders") +
               ggtitle("NYC Riders by Age in 2017")+
               scale_x_continuous(limits = c(10, 85), breaks = seq(10, 85, 5))+
               facet_wrap(~Gender),
             qplot (x=2017-Birth.Year, data =subset(chi, !is.na(Gender)), binwidth=1, 
                    xlab = 'Ages of Chicago Riders',
                    ylab = "Number of Riders") +
               ggtitle("Chicago Riders by Age in 2017")+
               scale_x_continuous(limits = c(10, 85), breaks = seq(10, 85, 5))+
               facet_wrap(~Gender)
             , nrow=1)


#From here we can see that the bulk of the riders, in both cities, are male.
#For males, the bulk of the riders in both Chicago and NYC are ages 25-40. 
#while females account for fewer riders, and they're average ages range from 25-35.

# We can confirm the median age for riders by gender, but first we need to eliminate 
# any rows containing 'NA' across any column so that we can use the 'by' function
# without causing any errors. Here we clean the data by running an 'na.omit' function.
na.omit(ny)
na.omit(chi)

#Next, I'm going to add a column called 'Age' to both the Chicago and NY data frames
ny["Age"] <- 2017-ny$Birth.Year
chi["Age"] <- 2017-chi$Birth.Year

#Next we use the 'by' function to to see statistical rider information for age by gender 
# in each city (NY/Chicago).

by(ny$Age, ny$Gender, summary)
by(chi$Age, chi$Gender, summary)

#This seems to line up with our histograms by age and gender.


# For our next couple of questions, we will need to combine the data from all three sources (minus the gender and age data)
#into one dataframe. The first thing I'll do is add a city column to all three data sets.

ny["City"] <- "NYC"
chi["City"] <- "Chicago"
wash["City"] <- "DC"

#The next thing I'll do is drop the gender, age, and birth year columns from the NYC and Chicago dataframes.
ny <- subset(ny, select = -Age)
ny <- subset(ny, select = -Gender)
ny <- subset(ny, select = -Birth.Year)
chi <- subset(chi, select = -Age)
chi <- subset(chi, select = -Gender)
chi <- subset(chi, select = -Birth.Year)


#The next thing I'll do is to combine the columns of all three cities into one dataframe called 'all'.
all <- rbind(ny, chi, wash)



#Question #2: What is the busiest day of the week by city?   
#First, I'll add a column called 'Weekday' and get the day of thew week using the weekday function from the lubridate library:
#I will base it upon the start.Time column. 
all["Weekday"] <- weekdays(as.Date(all$Start.Time))

#Next I will order the days of the week in the dataframe.
all$Weekday <- factor(all$Weekday, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

#Next I will plot by day of the week and color the cities.
ggplot(data=all, aes(x=Weekday, fill = City))+
  geom_bar(position = 'dodge') +
  ggtitle('Volume of Rides per City') +
  theme(plot.title = element_text(hjust = 0.5)) + #used this page to help me center title (https://github.com/duttashi/visualizer/issues/8)
  labs(x='Day of the Week', y='Count of Rides')  #Used this page to help me sort (https://sebastiansauer.github.io/ordering-bars/)

#The answer to question #2 is: Wednesday is the busiest day of the week for both DC and NYC,
#but Tuesday is the busiest day of the week for Chicago.


#Question #3 What was the average trip duration by city?
# This is probably best expressed by a bar chart in hours. Minutes are too hard to visualize when
#they get to be in the hundreds or thousands. I'm also going to order this from greatest to least
#along the x axis.

ggplot(aes(x= reorder(City, -Trip.Duration), y=Trip.Duration/60), data=all) +
  geom_bar(position = 'dodge2', stat = 'summary', fun ='mean', fill = 'cyan2') +
  ggtitle('Average Trip Duration by City In Hours') +
  theme(plot.title = element_text(hjust = 0.5)) + #used this page to help me center title (https://github.com/duttashi/visualizer/issues/8)
  labs(x='City', y='Average Trip Duration (Hours)')  #Used this page to help me sort (https://sebastiansauer.github.io/ordering-bars/)

#The answer to this question is just over 20 hours for DC, nearly 16 hours for Chicago, and about 15 hours for NYC.
#to verify this, we can use the BY function.

by(all$Trip.Duration/60, all$City, summary)
