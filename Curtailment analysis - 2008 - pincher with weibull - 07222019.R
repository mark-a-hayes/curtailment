#####################
##
## Curtailment Analysis for Pincher Creek 2008 point data
## With Weibull distribution analysis
##
## Mark A. Hayes
## Normandeau Associates, Inc.
##
## 7/22/2019 
##
##
## The purpose of this analysis is to develop reproducible code for comparative curtailment 
## analysis. Here, I analyze data for an area I am calling "Pincher 
## Creek. This area is the location of several key Alberta Canada wind facilities.
## This is point data for lat = 49.479, lon = 246.047.
##
## Prep the csv file provided by the Canadian Wind Atlas Project
## by splitting the numerical date-time string in column A in Excel such that you have  
## Year, Month, Day, and Hour, Min, Sec columns. Add a column for seconds (sec) with "00".
## Organize the date & time columns in this order: YYYY, MM, DD, HH, MM, SS.
## Concatenate the date and time information into a Date_Time column using the concatenate function in Excel,
## such that you have a Date_Time columns in the "YYYY/MM/DD HH:MM:SS" format.
## In the R code, this Date_Time information will be standardized so it agrees with sunset and 
## sunrise times that are calculated later.
##
## Do this by converting the original Canada Wind Atlas data csv into an excel workbook,
## do the concatenation in this workbook then convert back into csv.
## NOTE: The Canada Wind Atlas data will be named by latitude and longitude, such as 
## "la9479lo246047.csv", so you can tell the location of the data by the file name.
## When saving and working on this file add "_DateTime" to the name so the Excel and csv file will
## now be "la9479lo246047_DateTime.xls". Once the concatenated Date_Time column is created, 
## close and reopen the csv and check that all seems to be well.   
## Save the file as a csv as "la9479lo246047_DateTime.csv".
##
## Aug = 31 days, Sept = 30 days, Oct = 31 days, so 92 days in autumn
## 6322 elements in Risky, so 6322 elements / 92 days = 68.7 elements/day
## 6 elements per hour, so 68.7/6 = 12 elements per day
##
## Bat Activity Scenarios:
## Uniform_high = Activity is based on highest nightly bat activity level recorded in Alberta, uniformly distributed across increments 
## Uniform_average = Activity is based on average nightly bat activity level recorded across Alberta
## Uniform_low = Activity is based on lowest non-zero nightly bat activity level recorded in Alberta 
## Random_high = A random number between 0-1 is specified for each increment based on highest activity observed and activity is for each element with < the probability threshold (e.g., < 0.50)
## Random_average = Activity is randomly selected using average activity 
## These scenario set-ups are created below.
##
## 
#####################

## Install libraries

rm(list=ls()) # Clear the global environment as needed
help(list) # as needed

install.packages("suncalc") # as needed
library(suncalc)

install.packages("bReeze") # as needed
library(bReeze)


## Load dataframe

# using the 2008 csv. This is the full set of WIND 10-minute increment data for the year 

data = read.table(file.choose(), header=TRUE, sep=",") # e.g., "la9479lo246047_DateTime.csv".
summary(data)

## Histogram of speed

hist(data$UV_080m)

## Convert the date-time information in the Date_Time column into date-time information that R recognizes 
# as time information, in the POSIXct format.

myformat <- "%Y/%m/%d%H:%M:%S"

date_time <- as.POSIXct(strptime(data$Date_Time, myformat, tz = "MST")) # POSIXct is in calendar time

# help("as.POSIXlt") # If help is needed

data <- cbind(data, date_time)
class(date_time) # should be POSIXct
date_time

# Rounding wind speed data to the nearest interger.
# For power analysis later we need to round up or down to the nearest 
# integer and add this vector to the dataframe, then
# use this vector for power curve analysis, using the "round" function:

windspeed <- round(data$UV_080m)
windspeed
data <- cbind(data, windspeed)
hist(windspeed)

###############################
##
## Power curve data & Power Production
##
###############################

## Terms:
# Power is in kW or MW and does not include time
# Energy is in kW hours or MW hours and does include time.

# As an example, a Vestas V82 turbine will produce 1.65 MW of power at maximum load
# and will produce 1.65 MW hour of energy at maximum load over 1 hour.

# If for this data we have 10 minute increments, we can divide by 6 to yield MW hours for the year.

## Estimating power generated in each 10 minute increment

## Using power curves available in the bReeze package
# Load example data as needed for bReeze
# data("winddata", package="bReeze")

## Original power curve data can be found here:

# http://www.wind-power-program.com/download.htm

# Import power curve, in this case for the Vestas V82 turbine at 80 m. (not needed for V82)

pc_v82 <- pc("Vestas_V82_1.65MW.pow") # Hub height = 80 m, rotor diameter = 82 m
pc_v82 # This shows the power curve

# I want the power curve data in a dataframe, so will create one using the pc_v82 data. I need to enter the data
# in the vectors, which takes a couple of minutes, but it will be nice to have the dataframe available.

windspeed_v82 <- c(0:25)
length(windspeed_v82)
power_v82 <- c(0,0,0,0,28,144,309,511,758,1017,1285,1504,1637,1637,1650,1650,1650,1650,1650,1650,1650,1650,1650,1650,1650,1650)
length(power_v82)
power_curve_v82 <- data.frame(windspeed_v82, power_v82)
summary(power_curve_v82)

# NOTE: If needed add an entry for windspeed of 0, which may not be included in the power curve data


## For each 10-minute increment in data, look up the power generated, create a vector and 
# add this to the data dataframe
# Using "match" to match the wind speed in data with the power curve data

# Checking for wind speed values in data not included in pc

summary(data$UV_080m)
summary(power_curve_v82$windspeed_v82)

power <- power_curve_v82$power_v82[match(data$windspeed,power_curve_v82$windspeed_v82)]
data <- cbind(data, power)
summary(power) # Check for NA's 
sum(power) # This is in kilowatts
# To get megawatts, divide by 1,000:

sum(power)/1000 # 36,243 MW total

# Divide by 6 because there are 6 10-minute increments per hour to yield MW-hours of energy
E_total <- (sum(power)/1000)/6 # 6,040 MWh for the year
E_total # 6041 MWh


## The maximum this turbine could produce in a year at maximum production is:
# Max MW capacity x number of 10-minute increments
# = (1.65 MW * 52704 10-min increments (366 days) ) / 6 10-min increments per hour

(1.65*52704)/6 # 14,493 MWh per year

# Capacity factor for this location and turbine model:

6040/14493 # 0.4167

###############################
##
## Sunset and Sunrise data
##
###############################

## Add sunrise and sunset times to the dataframe for this location, assuming Mountain Time (MST) for 
# autumn (August-October).

# First figuring out the date range needed:

Sys.Date()-4186 # 1/1/2008 on 3/14/2019. Today's date minus 4088 days. 
Sys.Date()-3821 # 12/31/2008/


# NOTE: Be sure to change Sys.Date() times and lat/lon coordinates. 
sun <- getSunlightTimes(date = seq.Date(Sys.Date()-4186, Sys.Date()-3821, by = 1), keep = c("sunrise", "sunset"), lat = 49.479, lon = 246.047, tz = "MST")
sun
summary(sun)

# Create a vector with sunrise  and sunset times
# Also, note that the only sunset and sunrise times that really matter are the times in the "Risky" dataframe
# for Aug-Sept.

sunrises <- sun$sunrise
sunsets <- sun$sunset
sunrises
sunsets
class(sunrises) # Matches POSIX classes as used above. This is good.

# Subtract 30 minutes from sunset time and add 30 minutes to sunrise time
# POSIXct objects are measured in seconds from an origin. So to add/subtract 30 minutes
# 30 min * 60 sec/min = 
30*60 # 1800 seconds


# Creating sun vectors that repeat each time 144 times (number of 10 minute increments 
# in a day: 24 * 6 = 144)

sunrises_rep <- rep(sunrises,each = 144) # Repeates each sun time 144 times in a row, then moves to the next time
sunsets_rep <- rep(sunsets, each = 144)
sunrises_rep
sunsets_rep

# Adding and subtracting 30 minutes from sunrise/sunset reps
sunsets_rep_30 <- sunsets_rep-1800
sunrises_rep_30 <- sunrises_rep+1800

# Binding the sunset-30 and sunrise+30 vectors to the original dataframe to create a new dataframe

data <- cbind(data, sunrises_rep_30)
data <- cbind(data, sunsets_rep_30)

# Check to make sure dates for these times equals actual date. 

###############################
##
## Risk evaluation
##
###############################

# Creating "risk" columns, where 1 means at risk for date and time 
# (e.g., time is before sunrise or after sunset and bats 
# are generally at risk.

# help(ifelse)

before_sunrise <- ifelse(data$date_time < data$sunrises_rep_30, 1, 0)
before_sunrise
summary(before_sunrise)
data <- cbind(data, before_sunrise) # Check to make sure this makes sense

after_sunset <- ifelse(data$date_time > data$sunsets_rep_30, 1, 0)
after_sunset
summary(after_sunset)
data <- cbind(data, after_sunset) # Check to make sure this makes sense

# If before sunrise or after sunset, the risk based on time = 1

Risk_time <- before_sunrise + after_sunset
summary(Risk_time)
Risk_time
data <- cbind(data, Risk_time)

# Now seasonal risk (autumn risk):

autumn <- c(8,9,10)
autumn_element <- is.element(data$Month, c(8,9,10))
autumn_element
autumn_risk <- ifelse(autumn_element == FALSE, 0,1)                      
autumn_risk                      
summary(autumn_risk)
data <- cbind(data, autumn_risk) # Make sure this makes sense                      
                      
# Evaluating total risk, where risk_time + autumn_risk = 2 for risky times

Total_risk <- data$Risk_time + data$autumn_risk

Season_Time_Risk <- ifelse(Total_risk==2,1,0)
summary(Total_risk)
summary(Season_Time_Risk)
data <- cbind(data, Season_Time_Risk) # Make sure this makes sense regarding risky times

###############################
##
## Creating subsets of autumn-night at different windspeed thresholds
##
###############################

## Use sequential subsetting to create subsets of data

summary(data)

# Full Autumn period (data$autumn_risk==1)

autumn <- subset(data, data$autumn_risk==1)
E_autumn <- (sum(autumn$power)/1000)/6 # MWh for autumn
E_autumn

# Autumn day-time

autumn_day <- subset(autumn, autumn$Risk_time==0)
E_autumn_day <- (sum(autumn_day$power)/1000)/6 # MWh for autumn daytime
E_autumn_day

# Non-Autumn period (data$autumn_risk==0)

non_autumn <- subset(data, data$autumn_risk==0)
E_non_autumn <- (sum(non_autumn$power)/1000)/6 # MWh for the non-autumn period
E_non_autumn

#### Table 3 results by Operation Time and % of total time

## Subset out the Risky period:

# Time (hrs) in Risky period

risky <- subset(data, data$Season_Time_Risk==1)
length(risky$UV_080m) # 6874 10-minute increments
6874/6 # 1145.7 hrs = Risky increments / 6 increments/hour

# Time (hrs) in full year

52704/6 # 8784.0 hours = hours for the full year 
52704/6/24 # Double checking calculation. For this year it is 366 days


# Time (hrs) in non-risky period = Full year - risky

(52704/6)-(6874/6)

# Percentages
7638.3/8784
1145.7/8784

## Power in Table 3

E_risky <- (sum(risky$power)/1000)/6 # MWh for the autumn nighttime
E_risky # 685 

# Total power = 6040.6 MWh

# Non-Risky period power production

6040.6 - 684.6 # 5356.0
non_risky <- 6041-685
non_risky

# Power percentages

5356.0/6040.6 # Non-risky
684.6/6040.6

hist(risky$UV_080m)

# Wind speeds between 3.5 - 7.0 m/s

risky_wind <- subset (risky, risky$windspeed > 3.5)
risky_wind <- subset(risky_wind, risky_wind$windspeed < 7.0)
summary(risky_wind)
hist(risky_wind$windspeed)

non_risky_wind <- subset (risky, risky$windspeed > 6.9)
non_risky_wind_below_cutin <- subset (risky, risky$windspeed < 3.5)

###
##
## Windspeed summaries
##
###

summary(data$UV_080m)
summary(risky$UV_080m)


###
##
## Distribition analysis of Risky
##
###


## Fitting the Weibull distribution to wind data using the MASS library:

## MASS = "Modern Applied Statistics with S"
## pp 50 of package pdf
library("MASS")

summary(data$UV_080m)

# Subsetting the data > 0:
windspeed_data <- subset(data, data$UV_080m>0)
fitdistr(windspeed_data$UV_080m, "weibull", lower = 0.001)



windspeed_risky <- subset(risky, risky$UV_080m>0)
fitdistr(windspeed_risky$UV_080m, "weibull", lower = 0.001)

## Plotting Weibull distributions

# Using the 'stats' package

# Parameter estimation for a distribution with known shape parameters

# Load 'survival' and 'MASS' packages

y <- rweibull(1000, shape=1.954, scale=8.078) # using survival library
est.par <- fitdistr(y, "weibull", lower = 0.001) # using MASS library
plot(est.par)

# 6/27/2019 - can't figure out how to plot the Weibull distribution. Would be easy in Mathematica

#######################
##
## Creating vectors for bat activity data
## These vectors should be equal in the number of elements as the "risky" dataframe
##
#######################

## length(x) gives the length of the element x
## 

length(risky$UV_080m) #6874

length_risky <- length(risky$UV_080m)
length_risky

# Remove a column from a dataframe risky, as needed

# risky$before_sunrise = NULL;



#################
##
## Creating vectors of uniform bat activity
##
#################

# As an example: bat activity in every 9th increments 
1/9 # 11.1% of increments have bat activity

# Note: the probability of bat activity can be shifted up or down as needed.
# It's easiest to do this in intergers, so 1/10 increments, 1/9 increments etc.
# There is almost certainly an easier way to create uniform bat activity,
# but the following approach works.

### Example uniform data 
# ~11% of increments have bat activity
# Average increments per night during risky season
# 91 days
# Risky period (night in curtailment period):
# = 6874 increments
6874/91 # 75.5 nights

### Average, low and high bat activity per night
# Using pre- and post-construction passes/night from AEP

calls <- c(23,47,17,15,2,4,3,2,4,16,8,2,1,1,4,2,8,1,1,15,5)
calls
summary(calls)
length(calls)

calls_mean <- mean(calls)
calls_mean

calls_min <- min(calls)
calls_min

calls_max <- max(calls)
calls_max

## Finding the proportion of increments with calls for mean, min, amd max

calls_total_mean <- 75.5*calls_mean # Nights * mean_calls/night
calls_total_mean

calls_total_min <- 75.5*calls_min
calls_total_min

calls_total_max <- 75.5*calls_max
calls_total_max

call_prob_mean <- calls_total_mean/6874
call_prob_mean # 0.095

call_prob_min <- calls_total_min/6874
call_prob_min # 0.011

call_prob_max <- calls_total_max/6874
call_prob_max # 0.516

# For ease of computing, we will use 
# Mean calls = 1/10
# Min calls = 1/100
# Max calls = 1/2

### Creating a uniform set of calls from mean, min, and max

## Mean uniform calls: uniform_rep_mean 

# Create a vector with 0's and a 1 at the end of the desired length.
# First, I'm creating a vector of length = 10, with the last element being a 1, so 1/10.
# Calling the vector "a".

a <- c(1,0,0,0,0,0,0,0,0,0)
a
length(risky$UV_080m) # 6874...Find the length of the windspeed vector in risky, as needed 
length(risky$UV_080m)/10 # 687.4...Find how many times a vector of length = 10 will fit in the risky windspeed vector
uniform_rep_mean <- rep(a, times = 687) # create a vector that repeats the a vector 702 times
uniform_rep_mean
6874-length(uniform_rep_mean)

# Create a second vector of 0's so 0's can be added to the a vector to make length equal to the risky vectors

a1 <- c(0,0,0,0) # Create a vector with the two zeros

uniform_rep_mean <- c(uniform_rep_mean,a1) # Add a1 vector to the uniform_rep vector. 
length(uniform_rep_mean) # This should be the desired length
sum(uniform_rep_mean) # Take the sum of this vector 
sum(uniform_rep_mean)/length(uniform_rep_mean) # Check that this proportion is roughly what is desired for bat activity prob.
risky <- cbind(risky, uniform_rep_mean)
uniform_rep_mean

## Min uniform calls: uniform_rep_min 

# Create a vector with 0's and a 1 at the end of the desired length.
# First, I'm creating a vector of length = 10, with all elements being 0,
# Repeat 8 times and add a vector ending with 1 ate the end.
# Calling the vector "b".

b <- c(0,0,0,0,0,0,0,0,0,0)
b <- rep(b, times = 9)
# remove(b)
b
length(b)
b1 <- c(0,0,0,0,0,0,0,0,0,1)
length(b1)
uniform_rep_min <- c(b,b1)
length(uniform_rep_min)
sum(uniform_rep_min)

length(risky$UV_080m) # 6874...Find the length of the windspeed vector in risky, as needed 
length(risky$UV_080m)/100 # 687.4...Find how many times a vector of length = 10 will fit in the risky windspeed vector
uniform_rep_min <- rep(uniform_rep_min, times = 68) # create a vector that repeats 68 times
6874-length(uniform_rep_min)


# Create a second vector of 0's so 0's can be added to the a vector to make length equal to the risky vectors

b3 <- c(0,0,0,0,0,0,0,0,0,0)
length(b3)
b4 <- rep(b3, times = 7)
length(b4)
b5 <- c(0,0,0,0) # Create a vector with 4 zeros

uniform_rep_min <- c(uniform_rep_min,b4,b5) # Add vectors to the uniform_rep vector. 
length(uniform_rep_min) # 6874
sum(uniform_rep_min) # 68 
sum(uniform_rep_min)/length(uniform_rep_min) # Check that this proportion is roughly what is desired for bat activity prob.
risky <- cbind(risky, uniform_rep_min)

## Max uniform calls: uniform_rep_max 

# Create a vector with 0's and a 1 at the end of the desired length.
# First, I'm creating a vector of length = 10, with half of the elements having bat calls.
# Calling the vector "c".

c <- c(0,1,0,1,0,1,0,1,0,1)
length(risky$UV_080m) # 6874...Find the length of the windspeed vector in risky, as needed 
length(risky$UV_080m)/10 # 687.4...Find how many times a vector of length = 10 will fit in the risky windspeed vector
uniform_rep_max <- rep(c, times = 687) # create a vector that repeats the a vector 702 times
6874-length(uniform_rep_max)

# Create a second vector of 0's so 0's can be added to the a vector to make length equal to the risky vectors

c1 <- c(0,1,0,1) # Create a vector with the two zeros

uniform_rep_max <- c(uniform_rep_max,c1) # Add a1 vector to the uniform_rep vector. 
length(uniform_rep_max) # This should be the desired length
sum(uniform_rep_max) # Take the sum of this vector 
sum(uniform_rep_max)/length(uniform_rep_max) # Check that this proportion is roughly what is desired for bat activity prob.
risky <- cbind(risky, uniform_rep_max)

#################
##
## Creating vectors of random bat activity
##
#################

## Create a vector of random numbers between 0 - 1 of length equal to risky

rand <- runif(length(risky$UV_080m))
summary(rand)

risky <- cbind(risky,rand)

# Specifying if there is activity in a given increment in Risky

rand_mean <- ifelse(risky$rand <0.1, 1, 0)
risky <- cbind(risky, rand_mean)

rand_min <- ifelse(risky$rand <0.01, 1, 0)
risky <- cbind(risky, rand_min)

rand_max <- ifelse(risky$rand <0.5, 1, 0)
risky <- cbind(risky, rand_max)


#######################
##
## Table 4
## Create blanket curtailment and bat activity scenarios, then evaluate energy production
##
#######################


### Scenario = All is equivalent to blanket curtailment, so not needed

## Approach = "Normal". Cut-in speed 

below_cutin <- subset(risky, risky$UV_080m<3.5)
summary(below_cutin)
E_below_cutin <- (sum(below_cutin$power)/1000)/6 # MWh for the year
E_below_cutin # This should be 0, since turbine doesn't produce power below cut-in.

above_cutin <- subset(risky, risky$UV_080m>3.499)
summary(above_cutin)
E_above_cutin <- (sum(above_cutin$power)/1000)/6 # MWh for the year
E_above_cutin # E available in curtailment period
E_total # Total Energy Available
E_total-E_above_cutin # Total E in non-risky period
E_total/E_total # % of Total Energy Available

## 5.0 m/s scenarios 

# Blanket
above_5.0 <- subset(risky, risky$UV_080m>4.999)
summary(above_5.0)
E_above_5.0 <- (sum(above_5.0$power)/1000)/6 # MWh for the year
E_above_5.0 # 675 MWh = E in curtailment period
cutin_to_5.0 <- subset(risky, risky$UV_080m<5.000)
total_E_5.0 <- E_above_5.0 + non_risky
total_E_5.0 # 6031 MWh
(total_E_5.0/E_total)*100 # 99.84

#SC-Uniform_mean
cutin_to_5.0_Uniform_mean <- subset(cutin_to_5.0, cutin_to_5.0$uniform_rep_mean==0)
E_cutin_to_5.0_Uniform_mean <- (sum(cutin_to_5.0_Uniform_mean$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Uniform_mean
E_cutin_to_5.0_Uniform_mean+E_above_5.0 # E generated during curtailment period
E_cutin_to_5.0_Uniform_mean+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Uniform_mean+E_above_5.0+non_risky)/E_total)*100 # %

#SC-Uniform_min
cutin_to_5.0_Uniform_min <- subset(cutin_to_5.0, cutin_to_5.0$uniform_rep_min==0)
E_cutin_to_5.0_Uniform_min <- (sum(cutin_to_5.0_Uniform_min$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Uniform_min
E_cutin_to_5.0_Uniform_min+E_above_5.0 # E generated during curtailment period
E_cutin_to_5.0_Uniform_min+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Uniform_min+E_above_5.0+non_risky)/E_total)*100 # %

#SC-Uniform_max
cutin_to_5.0_Uniform_max <- subset(cutin_to_5.0, cutin_to_5.0$uniform_rep_max==0)
E_cutin_to_5.0_Uniform_max <- (sum(cutin_to_5.0_Uniform_max$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Uniform_max
E_cutin_to_5.0_Uniform_max+E_above_5.0 # E generated during curtailment period
E_cutin_to_5.0_Uniform_max+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Uniform_max+E_above_5.0+non_risky)/E_total)*100 # %

#SC-Random_Mean
cutin_to_5.0_Random_mean <- subset(cutin_to_5.0, cutin_to_5.0$rand_mean==0)
E_cutin_to_5.0_Random_mean <- (sum(cutin_to_5.0_Random_mean$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Random_mean 
E_cutin_to_5.0_Random_mean+E_above_5.0 # Power generated during curtailment period 
E_cutin_to_5.0_Random_mean+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Random_mean+E_above_5.0+non_risky)/E_total)*100 # %

#SC-Random_Min
cutin_to_5.0_Random_min <- subset(cutin_to_5.0, cutin_to_5.0$rand_min==0)
E_cutin_to_5.0_Random_min <- (sum(cutin_to_5.0_Random_min$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Random_min 
E_cutin_to_5.0_Random_min+E_above_5.0 # Power generated during curtailment period 
E_cutin_to_5.0_Random_min+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Random_min+E_above_5.0+non_risky)/E_total)*100 # %

#SC-Random_Max
cutin_to_5.0_Random_max <- subset(cutin_to_5.0, cutin_to_5.0$rand_max==0)
E_cutin_to_5.0_Random_max <- (sum(cutin_to_5.0_Random_max$power)/1000)/6 # MWh for the year
E_cutin_to_5.0_Random_max 
E_cutin_to_5.0_Random_max+E_above_5.0 # Power generated during curtailment period 
E_cutin_to_5.0_Random_max+E_above_5.0+non_risky # annual production
((E_cutin_to_5.0_Random_max+E_above_5.0+non_risky)/E_total)*100 # %


## 5.5 m/s scenarios 

# Blanket
above_5.5 <- subset(risky, risky$UV_080m>5.499)
summary(above_5.5)
E_above_5.5 <- (sum(above_5.5$power)/1000)/6 # MWh for the year
E_above_5.5 # 670 MWh = E in curtailment period
cutin_to_5.5 <- subset(risky, risky$UV_080m<5.500)
total_E_5.5 <- E_above_5.5 + non_risky
total_E_5.5 # 6026 MWh
(total_E_5.5/E_total)*100 # 99.76

#SC-Uniform_mean
cutin_to_5.5_Uniform_mean <- subset(cutin_to_5.5, cutin_to_5.5$uniform_rep_mean==0)
E_cutin_to_5.5_Uniform_mean <- (sum(cutin_to_5.5_Uniform_mean$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Uniform_mean
E_cutin_to_5.5_Uniform_mean+E_above_5.5 # E generated during curtailment period
E_cutin_to_5.5_Uniform_mean+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Uniform_mean+E_above_5.5+non_risky)/E_total)*100 # %

#SC-Uniform_min
cutin_to_5.5_Uniform_min <- subset(cutin_to_5.5, cutin_to_5.5$uniform_rep_min==0)
E_cutin_to_5.5_Uniform_min <- (sum(cutin_to_5.5_Uniform_min$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Uniform_min
E_cutin_to_5.5_Uniform_min+E_above_5.5 # E generated during curtailment period
E_cutin_to_5.5_Uniform_min+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Uniform_min+E_above_5.5+non_risky)/E_total)*100 # %

#SC-Uniform_max
cutin_to_5.5_Uniform_max <- subset(cutin_to_5.5, cutin_to_5.5$uniform_rep_max==0)
E_cutin_to_5.5_Uniform_max <- (sum(cutin_to_5.5_Uniform_max$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Uniform_max
E_cutin_to_5.5_Uniform_max+E_above_5.5 # E generated during curtailment period
E_cutin_to_5.5_Uniform_max+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Uniform_max+E_above_5.5+non_risky)/E_total)*100 # %

#SC-Random_Mean
cutin_to_5.5_Random_mean <- subset(cutin_to_5.5, cutin_to_5.5$rand_mean==0)
E_cutin_to_5.5_Random_mean <- (sum(cutin_to_5.5_Random_mean$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Random_mean 
E_cutin_to_5.5_Random_mean+E_above_5.5 # Power generated during curtailment period 
E_cutin_to_5.5_Random_mean+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Random_mean+E_above_5.5+non_risky)/E_total)*100 # %

#SC-Random_Min

cutin_to_5.5_Random_min <- subset(cutin_to_5.5, cutin_to_5.5$rand_min==0)
E_cutin_to_5.5_Random_min <- (sum(cutin_to_5.5_Random_min$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Random_min 
E_cutin_to_5.5_Random_min+E_above_5.5 # Power generated during curtailment period 
E_cutin_to_5.5_Random_min+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Random_min+E_above_5.5+non_risky)/E_total)*100 # %

#SC-Random_Max
cutin_to_5.5_Random_max <- subset(cutin_to_5.5, cutin_to_5.5$rand_max==0)
E_cutin_to_5.5_Random_max <- (sum(cutin_to_5.5_Random_max$power)/1000)/6 # MWh for the year
E_cutin_to_5.5_Random_max 
E_cutin_to_5.5_Random_max+E_above_5.5 # Power generated during curtailment period 
E_cutin_to_5.5_Random_max+E_above_5.5+non_risky # annual production
((E_cutin_to_5.5_Random_max+E_above_5.5+non_risky)/E_total)*100 # %


## 6.0 m/s scenarios scenarios

# Blanket
above_6.0 <- subset(risky, risky$UV_080m>5.999)
summary(above_6.0)
E_above_6.0 <- (sum(above_6.0$power)/1000)/6 # MWh for the year
E_above_6.0 # 675 MWh = E in curtailment period
cutin_to_6.0 <- subset(risky, risky$UV_080m<6.000)
total_E_6.0 <- E_above_6.0 + non_risky
total_E_6.0 # 6031 MWh
(total_E_6.0/E_total)*100 # 99.84

#SC-Uniform_mean
cutin_to_6.0_Uniform_mean <- subset(cutin_to_6.0, cutin_to_6.0$uniform_rep_mean==0)
E_cutin_to_6.0_Uniform_mean <- (sum(cutin_to_6.0_Uniform_mean$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Uniform_mean
E_cutin_to_6.0_Uniform_mean+E_above_6.0 # E generated during curtailment period
E_cutin_to_6.0_Uniform_mean+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Uniform_mean+E_above_6.0+non_risky)/E_total)*100 # %

#SC-Uniform_min
cutin_to_6.0_Uniform_min <- subset(cutin_to_6.0, cutin_to_6.0$uniform_rep_min==0)
E_cutin_to_6.0_Uniform_min <- (sum(cutin_to_6.0_Uniform_min$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Uniform_min
E_cutin_to_6.0_Uniform_min+E_above_6.0 # E generated during curtailment period
E_cutin_to_6.0_Uniform_min+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Uniform_min+E_above_6.0+non_risky)/E_total)*100 # %

#SC-Uniform_max
cutin_to_6.0_Uniform_max <- subset(cutin_to_6.0, cutin_to_6.0$uniform_rep_max==0)
E_cutin_to_6.0_Uniform_max <- (sum(cutin_to_6.0_Uniform_max$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Uniform_max
E_cutin_to_6.0_Uniform_max+E_above_6.0 # E generated during curtailment period
E_cutin_to_6.0_Uniform_max+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Uniform_max+E_above_6.0+non_risky)/E_total)*100 # %

#SC-Random_Mean
cutin_to_6.0_Random_mean <- subset(cutin_to_6.0, cutin_to_6.0$rand_mean==0)
E_cutin_to_6.0_Random_mean <- (sum(cutin_to_6.0_Random_mean$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Random_mean 
E_cutin_to_6.0_Random_mean+E_above_6.0 # Power generated during curtailment period 
E_cutin_to_6.0_Random_mean+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Random_mean+E_above_6.0+non_risky)/E_total)*100 # %

#SC-Random_Min
cutin_to_6.0_Random_min <- subset(cutin_to_6.0, cutin_to_6.0$rand_min==0)
E_cutin_to_6.0_Random_min <- (sum(cutin_to_6.0_Random_min$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Random_min 
E_cutin_to_6.0_Random_min+E_above_6.0 # Power generated during curtailment period 
E_cutin_to_6.0_Random_min+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Random_min+E_above_6.0+non_risky)/E_total)*100 # %

#SC-Random_Max
cutin_to_6.0_Random_max <- subset(cutin_to_6.0, cutin_to_6.0$rand_max==0)
E_cutin_to_6.0_Random_max <- (sum(cutin_to_6.0_Random_max$power)/1000)/6 # MWh for the year
E_cutin_to_6.0_Random_max 
E_cutin_to_6.0_Random_max+E_above_6.0 # Power generated during curtailment period 
E_cutin_to_6.0_Random_max+E_above_6.0+non_risky # annual production
((E_cutin_to_6.0_Random_max+E_above_6.0+non_risky)/E_total)*100 # %


## 6.5 m/s scenarios scenarios 

# Blanket
above_6.5 <- subset(risky, risky$UV_080m>6.499)
summary(above_6.5)
E_above_6.5 <- (sum(above_6.5$power)/1000)/6 # MWh for the year
E_above_6.5 # 675 MWh = E in curtailment period
cutin_to_6.5 <- subset(risky, risky$UV_080m<6.500)
total_E_6.5 <- E_above_6.5 + non_risky
total_E_6.5 # 6031 MWh
(total_E_6.5/E_total)*100 # 99.84

#SC-Uniform_mean
cutin_to_6.5_Uniform_mean <- subset(cutin_to_6.5, cutin_to_6.5$uniform_rep_mean==0)
E_cutin_to_6.5_Uniform_mean <- (sum(cutin_to_6.5_Uniform_mean$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Uniform_mean
E_cutin_to_6.5_Uniform_mean+E_above_6.5 # E generated during curtailment period
E_cutin_to_6.5_Uniform_mean+E_above_6.5+non_risky # annual production
((E_cutin_to_6.5_Uniform_mean+E_above_6.5+non_risky)/E_total)*100 # %

#SC-Uniform_min
cutin_to_6.5_Uniform_min <- subset(cutin_to_6.5, cutin_to_6.5$uniform_rep_min==0)
E_cutin_to_6.5_Uniform_min <- (sum(cutin_to_6.5_Uniform_min$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Uniform_min
E_cutin_to_6.5_Uniform_min+E_above_6.5 # E generated during curtailment period
E_cutin_to_6.5_Uniform_min+E_above6.5+non_risky # annual production
((E_cutin_to_6.5_Uniform_min+E_above_6.5+non_risky)/E_total)*100 # %

#SC-Uniform_max
cutin_to_6.5_Uniform_max <- subset(cutin_to_6.5, cutin_to_6.5$uniform_rep_max==0)
E_cutin_to_6.5_Uniform_max <- (sum(cutin_to_6.5_Uniform_max$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Uniform_max
E_cutin_to_6.5_Uniform_max+E_above_6.5 # E generated during curtailment period
E_cutin_to_6.5_Uniform_max+E_above_6.5+non_risky # annual production
((E_cutin_to_6.5_Uniform_max+E_above_6.5+non_risky)/E_total)*100 # %

#SC-Random_Mean
cutin_to_6.5_Random_mean <- subset(cutin_to_6.5, cutin_to_6.5$rand_mean==0)
E_cutin_to_6.5_Random_mean <- (sum(cutin_to_6.5_Random_mean$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Random_mean 
E_cutin_to_6.5_Random_mean+E_above_6.5 # Power generated during curtailment period 
E_cutin_to_6.5_Random_mean+E_above_6.5+non_risky # annual production
((E_cutin_to_6.5_Random_mean+E_above_6.5+non_risky)/E_total)*100 # %

#SC-Random_Min
cutin_to_6.5_Random_min <- subset(cutin_to_6.5, cutin_to_6.5$rand_min==0)
E_cutin_to_6.5_Random_min <- (sum(cutin_to_6.5_Random_min$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Random_min 
E_cutin_to_6.5_Random_min+E_above_6.5 # Power generated during curtailment period 
E_cutin_to_6.5_Random_min+E_above_6.5+non_risky # annual production
((E_cutin_to_6.5_Random_min+E_above_6.5+non_risky)/E_total)*100 # %

#SC-Random_Max
cutin_to_6.5_Random_max <- subset(cutin_to_6.5, cutin_to_6.5$rand_max==0)
E_cutin_to_6.5_Random_max <- (sum(cutin_to_6.5_Random_max$power)/1000)/6 # MWh for the year
E_cutin_to_6.5_Random_max 
E_cutin_to_6.5_Random_max+E_above_6.5 # Power generated during curtailment period 
E_cutin_to_6.5_Random_max+E_above_6.5+non_risky # annual production
((E_cutin_to_6.5_Random_max+E_above_6.5+non_risky)/E_total)*100 # %


## 7.0 m/s scenarios scenarios 

# Blanket
above_7.0 <- subset(risky, risky$UV_080m>6.999)
summary(above_7.0)
E_above_7.0 <- (sum(above_7.0$power)/1000)/6 # MWh for the year
E_above_7.0 # 675 MWh = E in curtailment period
cutin_to_7.0 <- subset(risky, risky$UV_080m<7.000)
total_E_7.0 <- E_above_7.0 + non_risky
total_E_7.0 # 6031 MWh
(total_E_7.0/E_total)*100 # 99.84

#SC-Uniform_mean
cutin_to_7.0_Uniform_mean <- subset(cutin_to_7.0, cutin_to_7.0$uniform_rep_mean==0)
E_cutin_to_7.0_Uniform_mean <- (sum(cutin_to_7.0_Uniform_mean$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Uniform_mean
E_cutin_to_7.0_Uniform_mean+E_above_7.0 # E generated during curtailment period
E_cutin_to_7.0_Uniform_mean+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Uniform_mean+E_above_7.0+non_risky)/E_total)*100 # %

#SC-Uniform_min
cutin_to_7.0_Uniform_min <- subset(cutin_to_7.0, cutin_to_7.0$uniform_rep_min==0)
E_cutin_to_7.0_Uniform_min <- (sum(cutin_to_7.0_Uniform_min$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Uniform_min
E_cutin_to_7.0_Uniform_min+E_above_7.0 # E generated during curtailment period
E_cutin_to_7.0_Uniform_min+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Uniform_min+E_above_7.0+non_risky)/E_total)*100 # %

#SC-Uniform_max
cutin_to_7.0_Uniform_max <- subset(cutin_to_7.0, cutin_to_7.0$uniform_rep_max==0)
E_cutin_to_7.0_Uniform_max <- (sum(cutin_to_7.0_Uniform_max$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Uniform_max
E_cutin_to_7.0_Uniform_max+E_above_7.0 # E generated during curtailment period
E_cutin_to_7.0_Uniform_max+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Uniform_max+E_above_7.0+non_risky)/E_total)*100 # %

#SC-Random_Mean
cutin_to_7.0_Random_mean <- subset(cutin_to_7.0, cutin_to_7.0$rand_mean==0)
E_cutin_to_7.0_Random_mean <- (sum(cutin_to_7.0_Random_mean$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Random_mean 
E_cutin_to_7.0_Random_mean+E_above_7.0 # Power generated during curtailment period 
E_cutin_to_7.0_Random_mean+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Random_mean+E_above_7.0+non_risky)/E_total)*100 # %

#SC-Random_Min
cutin_to_7.0_Random_min <- subset(cutin_to_7.0, cutin_to_7.0$rand_min==0)
E_cutin_to_7.0_Random_min <- (sum(cutin_to_7.0_Random_min$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Random_min 
E_cutin_to_7.0_Random_min+E_above_7.0 # Power generated during curtailment period 
E_cutin_to_7.0_Random_min+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Random_min+E_above_7.0+non_risky)/E_total)*100 # %

#SC-Random_Max
cutin_to_7.0_Random_max <- subset(cutin_to_7.0, cutin_to_7.0$rand_max==0)
E_cutin_to_7.0_Random_max <- (sum(cutin_to_7.0_Random_max$power)/1000)/6 # MWh for the year
E_cutin_to_7.0_Random_max 
E_cutin_to_7.0_Random_max+E_above_7.0 # Power generated during curtailment period 
E_cutin_to_7.0_Random_max+E_above_7.0+non_risky # annual production
((E_cutin_to_7.0_Random_max+E_above_7.0+non_risky)/E_total)*100 # 


#######
##
## To complete tables for AEP report we can stop here and don't need to proceed. Remaining code
## is optional and for information only.
##
#######

#####################
## 
## Subsetting data by Month - Optional
##
#####################

# Summary Stats

summary(data)

## Kernel density plots of windpeed

# All data

summary(data$UV_080m)
d <- density(data$UV_080m)
plot(d)
length(data$UV_080m)
d_power <- density(data$power)
plot(d_power)
hist(data$power)

# Monthly windspeed data

help(subset)

#January

jan <- subset(data, data$Month==1)
jan$UV_080m
d_jan <- density(jan$UV_080m)
plot(d_jan)
summary(jan$UV_080m)
length(jan$UV_080m)


#February
feb <- subset(data, data$Month==2)
feb$UV_080m
d_feb <- density(feb$UV_080m)
plot(d_feb)
summary(feb$UV_080m)
length(feb$UV_080m)

#March
mar <- subset(data, data$Month==3)
mar$UV_080m
d_mar <- density(mar$UV_080m)
plot(d_mar)
summary(mar$UV_080m)
length(mar$UV_080m)

#April
apr <- subset(data, data$Month==4)
apr$UV_080m
d_apr <- density(apr$UV_080m)
plot(d_apr)
summary(apr$UV_080m)
length(apr$UV_080m)

#May
may <- subset(data, data$Month==5)
may$UV_080m
d_may <- density(may$UV_080m)
plot(d_may)
summary(may$UV_080m)
length(may$UV_080m)

#June
jun <- subset(data, data$Month==6)
jun$UV_080m
d_jun <- density(jun$UV_080m)
plot(d_jun)
summary(may$UV_080m)
length(jun$UV_080m)

#July
jul <- subset(data, data$Month==7)
jul$UV_080m
d_jul <- density(jul$UV_080m)
plot(d_jul)
summary(jul$UV_080m)
length(jul$UV_080m)

#August, with night time subset
aug <- subset(data, data$Month==8)
aug$UV_080m
d_aug <- density(aug$UV_080m)
plot(d_aug)
summary(aug$UV_080m)
length(aug$UV_080m)

aug_night <- subset(aug, aug$Risk_time==1)
aug_night$UV_080m
d_aug_night <- density(aug_night$UV_080m)
plot(d_aug_night)
summary(aug_night$UV_080m)
length(aug_night$UV_080m)
(length(aug_night$UV_080m)/length(data$UV_080m))*100 # % of total year

#September, with night time subset
sep <- subset(data, data$Month==9)
sep$UV_080m
d_sep <- density(sep$UV_080m)
plot(d_sep)
summary(sep$UV_080m)
length(sep$UV_080m)
(length(sep_night$UV_080m)/length(data$UV_080m))*100 # % of total year

sep_night <- subset(sep, sep$Risk_time==1)
sep_night$UV_080m
d_sep_night <- density(sep_night$UV_080m)
plot(d_sep_night)
summary(sep_night$UV_080m)
length(sep_night$UV_080m)
(length(sep_night$UV_080m)/length(data$UV_080m))*100 # % of total year

#October, with night time subset
oct <- subset(data, data$Month==10)
oct$UV_080m
d_oct <- density(oct$UV_080m)
plot(d_oct)
summary(oct$UV_080m)
length(oct$UV_080m)

oct_night <- subset(oct, oct$Risk_time==1)
oct_night$UV_080m
d_oct_night <- density(oct_night$UV_080m)
plot(d_oct_night)
summary(oct_night$UV_080m)
length(oct_night$UV_080m)
(length(oct_night$UV_080m)/length(data$UV_080m))*100 #% of total year

#November
nov <- subset(data, data$Month==11)
nov$UV_080m
d_nov <- density(nov$UV_080m)
plot(d_nov)
summary(nov$UV_080m)
length(nov$UV_080m)

#December
dec <- subset(data, data$Month==12)
dec$UV_080m
d_dec <- density(dec$UV_080m)
plot(d_dec)
summary(dec$UV_080m)
length(dec$UV_080m)

# Fall at night
fall_night_risk <- subset(data, data$Season_Time_Risk==1)
d_fall_night_risk <- density(fall_night_risk$UV_080m)
plot(d_fall_night_risk)
summary(fall_night_risk$UV_080m)
length(fall_night_risk$UV_080m)
(length(fall_night_risk$UV_080m)/length(data$UV_080m))*100 # % of total year

# Fall at night and wind speed < 7.0 m/s
fall_night_7.0 <- subset(fall_night_risk, fall_night_risk$UV_080m<7.0)
summary(fall_night_7.0)
d_fall_night_7.0 <- density(fall_night_7.0$UV_080m)
plot(d_fall_night_7.0)
summary(fall_night_7.0$UV_080m)
length(fall_night_7.0$UV_080m)
(length(fall_night_7.0$UV_080m)/length(data$UV_080m))*100 # % of total year

## Summaries all together

# Windspeed summaries 

summary(data$UV_080m)
summary(jan$UV_080m)
summary(feb$UV_080m)
summary(mar$UV_080m)
summary(apr$UV_080m)
summary(may$UV_080m)
summary(jun$UV_080m)
summary(jul$UV_080m)
summary(aug$UV_080m)
summary(sep$UV_080m)
summary(oct$UV_080m)
summary(nov$UV_080m)
summary(dec$UV_080m)
summary(aug_night$UV_080m)
summary(sep_night$UV_080m)
summary(oct_night$UV_080m)
summary(fall_night_risk$UV_080m)
summary(fall_night_7.0$UV_080m)

# Power summaries 

summary(data$power)
summary(jan$power)
summary(feb$power)
summary(mar$power)
summary(apr$power)
summary(may$power)
summary(jun$power)
summary(jul$power)
summary(aug$power)
summary(sep$power)
summary(oct$power)
summary(nov$power)
summary(dec$power)
summary(aug_night$power)
summary(sep_night$power)
summary(oct_night$power)
summary(fall_night_risk$power)
summary(fall_night_7.0$power)


# All together, with simple density plots

par(mfrow=c(2,1))
plot(d, main = "Kernel Density of the Full Year", xlim = c(0,20), ylim = c(0,0.15)) 
plot(d_fall_night_risk, main = "Kernel Density of Fall Night Time", xlim = c(0,20), ylim = c(0,0.15))



###############################
##
## Histograms of windspeed and direction - optional
##
###############################


## Histograms of windspeed and direction

summary(data$UV_080m)
hist(data$UV_080m, xlim=c(0,30), main = "Histogram of 80 mWindspeed (m/s)", xlab = "Windspeed (m/s)")
dev.print(tiff, "speedhist_80.tiff", height=4, width=6, units='in', res=300)

hist(data$WD_080m, xlim = c(0,360), main = "Histogram of Wind Direction", xlab = "Degrees")
dev.print(tiff, "powerhist_100.tiff", height=4, width=6, units='in', res=300)

## Plots of windpeed against other variables

plot(data$Month,data$UV_080m, xlim = c(0,12),main = "Windpeed (m/s) by Month", xlab = "Month", ylab = "Windspeed (m/s)")
dev.print(tiff, "speed_vs_power_100.tiff", height=4, width=6, units='in', res=300)

plot(direction, speed, xlab = "Windspeed Direction (degrees)", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.10, main = "Windspeed Direction vs. Windspeed")
dev.print(tiff, "direction_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

plot(Hour, speed, xlab = "Hour", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.5, main = "Hour vs. Windspeed")
dev.print(tiff, "hour_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

plot(Month, speed, xlab = "Month", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.5, main = "Month vs. Windspeed")
dev.print(tiff, "month_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

plot(temp, speed, xlab = "Temperature (Kelvin)", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.10, main = "Temperature vs. Windspeed")
dev.print(tiff, "temperature_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

plot(pressure, speed, xlab = "Pressure", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.10, main = "Pressure vs. Windspeed")
dev.print(tiff, "pressure_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

plot(density, speed, xlab = "Density", ylab = "Windspeed (m/s)", 
     col = "blue", pch = 1, cex = 0.10, main = "Density vs. Windspeed")
dev.print(tiff, "density_vs_speed_100.tiff", height=4, width=6, units='in', res=300)

## Weibull distribution analysis

###
##
## Distribition analysis of Risky
##
###

## Using fitdistrplus package

# I believe the data dataframe is too long. Just using risky

library("fitdistrplus")
risky_wind <- risky$UV_080m

plotdist(risky$UV_080m, histo = TRUE, demp = TRUE)
descdist(risky$UV_080m, boot = 1000)
fw <- fitdist(risky_wind, "weibull")
summary(fw)

# I think the data dataframe is too long.

# Trying risky

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

## End






