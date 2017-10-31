"Github collaboration (final codes) – due Nov 2
Report – due Nov 2
Presentation at next class
Think about who, what, and how
Think about your storyline
Grade will be given after next class

Netflix’s CEO is impressed with your initial findings. Now he asked a few follow-up questions

Does higher rating movie also have a higher adjusted gross revenue?
Shall we make a short movie or a long movie?
If a movie does well in US, does it also usually do well overseas?
Now you have explored many variables and how they affect revenue, 
please recommend a strategy for Netflix’s next investment in a movie."

library(ggplot2)
setwd("/Users/scottvirshup/Documents/UC Davis/Q1/Data Visualization/Group Project assignment 2-1")
movie <- read.csv("movie.csv")

##################
### Question 1 ###
##################
### Does higher rating movie also have a higher adjusted gross revenue? ###
# clean both metrics
# gross revenue first
movie$Adjusted_Gross2 <- as.numeric(gsub(",", "", as.character(movie$Adjusted_Gross)))
is.na(movie$Adjusted_Gross2)
#check that all incomplete cases are gone
movie[!complete.cases(movie),]

# IMDb_Rating does not have any NA's
movie$IMDb_Rating2 <- as.numeric(as.character(movie$IMDb_Rating))
is.na(movie$IMDb_Rating2)
# this checks which rows are not complete
movie[!complete.cases(movie),]

# find the correlation between rating and adjusted gross revenue
cor(movie$Adjusted_Gross2, movie$IMDb_Rating)

# plot as a scatter plot with visual trend line
ggplot(data = movie, aes(x = log(Adjusted_Gross2), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth()

# plot as a scatter plot with visual trend line. sqrt()
ggplot(data = movie, aes(x = (Adjusted_Gross2)^(1/8), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth()

"Generally speaking, there is a positive relationship between these two metrics. 
However, with very clustered datapoints, this trendline should not be considered
without further analysis"

"even though there are two rating scales (imdb and movielens), we don't need to compare
scatter plots for both against adjusted gross revenue because both rating scales are
highly correlated"

"try out other forms of transformation to the variables to enhance things that are also there
scaling (in python but not R, we need to figure that out)
sqrt()
"


##################
### Question 2 ###
##################
### Shall we make a short movie or a long movie? ###
"In order to decide whether we should make a short or long movie, we should define
what the difference between short and long movies are. First, we should look at the 
data to see the distribution of movie durations to try and bucket these."
# We know that runtime is clean because of previous tests

# Plot it as a histogram to see distribution
ggplot(data = movie, aes(x = Runtime_min)) +
  geom_histogram(binwidth = 10)
average_runtime <- mean(movie$Runtime_min)
median_runtime <- median(movie$Runtime_min)
"at this point, we should probably remove outliers, but first identify them through the empirical test."


"Looks approximately normally distributed at first glance. 
Mean and median are basically equal at just under 2 hours.
Let us arbitrarily assume that there are actually 3 buckets 
for movie lengths:
long: greater than mean plus 1 SD
medium: between +- 1 SD away from mean 
short: less than mean minus 1 SD "



mean_plus_sd <- average_runtime + sd(movie$Runtime_min)
mean_minus_sd <- average_runtime - sd(movie$Runtime_min)

movie_length_vector <- with(movie, movie$movie_length_bucket <- 
                              ifelse(movie$Runtime_min >= mean_plus_sd, "Long",
                              ifelse(movie$Runtime_min <= mean_minus_sd, "Short", "Medium")))
movie["movie_length_bucket"] <- movie_length_vector

ggplot(data = movie, aes(x = movie_length_bucket, y = Profit)) +
  geom_col()


"based on Yixin's correlation matrix, there are 3 types of movie genres."



##################
### Question 3 ###
##################
### If a movie does well in US, does it also usually do well overseas? ###
movie$Adjusted_Gross2 <- as.numeric(gsub(",", "", as.character(movie$Overseas_rev)))
cor(movie$US_rev, movie$Overseas_rev)



##################
### Question 4 ###
##################
### Now you have explored many variables and how they affect revenue, 
### please recommend a strategy for Netflix’s next investment in a movie.
"what are the metrics that we should analyze for this?
Netflix doesn't necessarily want to increase their revenue. They want to increase profit.
we should look at studio, genre, "

"How can we present this? can we present this as a skit? "