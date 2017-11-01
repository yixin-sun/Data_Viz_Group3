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
"Our process here involved finding the relationship between ratings and revenue. When we really
thought about why this question would be asked, we thought it would make the most sense for the
CEO of Netflix to be interested in seeing what types of movies would be most popular on their
site, and which types of movies they should focus their production efforts on making. The insight
that 'well liked movies make a lot of money' is not really helpful because that does not guide their
business much. Instead, we want to see whether specific genres experience larger effects here. 
For example, if there is a genre that makes a lot of money regardless of how highly the audience
regards the quality of the movie, then that would be of extreme interest to Netflix's CEO. We've
included both our interpretation of this question along with the literal interpretation so that 
the CEO can see the extent to which we care about solving their root issues in addition to being
faithful to their requests."


# clean both metrics
# gross revenue first
movie$Adjusted_Gross2 <- as.numeric(gsub(",", "", as.character(movie$Adjusted_Gross)))
is.na(movie$Adjusted_Gross2)
#check that all incomplete cases are gone
movie[!complete.cases(movie),]
# IMDb_Rating does not have any NA's

# Find the correlation between IMDB ratings and MovieLens ratings
cor(movie$MovieLens_Rating, movie$IMDb_Rating)
"There is a very strong relationship between MovieLens ratings and IMDB ratings. This can be
expected because generally high quality movies are going to be rated highly across all rating
systems. The differences could be due to movies that have a small volume of ratings, for which 
the small sample size is not representative of the 'true' quality of the movie. Since these are
so highly correlated, we will simplify our process to evaluating the relationship between just
one of the rating metrics so that we avoid multicollinearity and a needlessly complex model."

# Find the correlation between Revenue and Profit
cor(movie$Adjusted_Gross2, movie$Profit2)
"There is not as strong of a relationship between Adjusted Gross Revenue and Profit as there
is between the two movie rating scores. On one hand this makes sense because one could have a 
highly profitable movie that did not make much money, as long as the production costs are low. 
However, it is also important to look at profit as a percent of budget because high revenue movies
are more likely to make large profits, even if they aren't as successful. For example, a low
Revenue movie like The Blair Witch Project racked up an extremely large profit (>40000%) relative to its 
budget, but the total profit number does not represent that because it is small compared other 
movies, like World War Z, which made more total profit but as a much smaller percent of budget.

The moral of the story here is that it might be beneficial for us to do this analysis on profit
in addition to revenue, since there could be hidden trends. While revenue alone is an important
metric for how successful movies are measured, having high revenue movies will low profit margins
is not nearly as impressive as making highly profitable movies."


# Find the correlation between rating and adjusted gross revenue / profit
cor(movie$Adjusted_Gross2, movie$IMDb_Rating)
cor(movie$Profit2, movie$IMDb_Rating)
"This correlation is relatively low for both. In order to get a more hollistic understanding of 
why this is the case, we should plot these variables on a scatter plot."

#########################
## Revenue scatterplot ##
#########################
# plot as a scatter plot with visual trend line
ggplot(data = movie, aes(x = Adjusted_Gross2, y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)
"The data points are highly clustered towards the lower end of Adjusted Gross Revenue. In order
to account for this clustering, we can transform the data so that it enhances the trends that 
are not visible currently"

######
## Transformation techniques
######
## Sqrt()
cor(sqrt(movie$Adjusted_Gross2), movie$IMDb_Rating)
ggplot(data = movie, aes(x = sqrt(Adjusted_Gross2), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)

## Remove outliers
# identify outliers Adjusted Gross Revenue
temp_movie <- read.csv("movie.csv")
temp_movie$Adjusted_Gross2 <- as.numeric(gsub(",", "", as.character(movie$Adjusted_Gross)))

q1 <- quantile(temp_movie$Adjusted_Gross2, 0.25)
q3 <- quantile(temp_movie$Adjusted_Gross2, 0.75)
iqr <- IQR(temp_movie$Adjusted_Gross2, na.rm = TRUE)

mild_lower_bound <- q1 - (1.5 * iqr)
mild_upper_bound <- q3 + (1.5 * iqr)
extreme_lower_bound <- q1 - (3 * iqr)
extreme_upper_bound <- q3 + (3 * iqr)

outlier_vector <- with(temp_movie, temp_movie$Outlier <- ifelse(temp_movie$Adjusted_Gross2 <= extreme_lower_bound, "Extreme", 
                                                  ifelse(temp_movie$Adjusted_Gross2 >= extreme_upper_bound, "Extreme",
                                                         ifelse(temp_movie$Adjusted_Gross2 <= mild_lower_bound, "Mild",
                                                                ifelse(temp_movie$Adjusted_Gross2 >= mild_upper_bound, "Mild", "Not an Outlier")))))

temp_movie["Outliers"] <- outlier_vector


# remove outliers
Removed_outliers <- subset(temp_movie, Outliers == "Not an Outlier")


# plot results
cor(Removed_outliers$Adjusted_Gross2, Removed_outliers$IMDb_Rating)
ggplot(data = Removed_outliers, aes(x = Adjusted_Gross2, y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)

## Log()
cor(log(movie$Adjusted_Gross2), movie$IMDb_Rating)
ggplot(data = movie, aes(x = log(Adjusted_Gross2), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)

########################
## Profit scatterplot ##
########################
movie$Profit2 <- as.numeric(gsub(",", "", as.character(movie$Profit)))
# plot as a scatter plot with visual trend line
ggplot(data = movie, aes(x = Profit2, y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)
"The data points are highly clustered towards the lower end of Adjusted Gross Revenue. In order
to account for this clustering, we can transform the data so that it enhances the trends that 
are not visible currently"

######
## Transformation techniques
######
## Sqrt()
cor(sqrt(movie$Profit2), movie$IMDb_Rating)
ggplot(data = movie, aes(x = sqrt(Profit2), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)

## Remove outliers
# identify outliers Adjusted Gross Revenue
temp_movie <- read.csv("movie.csv")
temp_movie$Profit2 <- as.numeric(gsub(",", "", as.character(movie$Profit)))

q1 <- quantile(temp_movie$Profit2, 0.25)
q3 <- quantile(temp_movie$Profit2, 0.75)
iqr <- IQR(temp_movie$Profit2, na.rm = TRUE)

mild_lower_bound <- q1 - (1.5 * iqr)
mild_upper_bound <- q3 + (1.5 * iqr)
extreme_lower_bound <- q1 - (3 * iqr)
extreme_upper_bound <- q3 + (3 * iqr)

outlier_vector <- with(temp_movie, temp_movie$Outlier <- ifelse(temp_movie$Profit2 <= extreme_lower_bound, "Extreme", 
                                                                ifelse(temp_movie$Profit2 >= extreme_upper_bound, "Extreme",
                                                                       ifelse(temp_movie$Profit2 <= mild_lower_bound, "Mild",
                                                                              ifelse(temp_movie$Profit2 >= mild_upper_bound, "Mild", "Not an Outlier")))))

temp_movie["Outliers"] <- outlier_vector


# remove outliers
Removed_outliers <- subset(temp_movie, Outliers == "Not an Outlier")


# plot results
cor(Removed_outliers$Profit2, Removed_outliers$IMDb_Rating)
ggplot(data = Removed_outliers, aes(x = Profit2, y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)

## Log()
cor(log(movie$Profit2), movie$IMDb_Rating)
ggplot(data = movie, aes(x = log(Profit2), y = IMDb_Rating)) +
  geom_point() + 
  geom_smooth(method = lm)



"In both examples, transforming the variable with log() seems to result in the most representative dataset. 
You're not ignoring important datapoints by removing outliers, but you're still plotting the data in such a 
way that you can clearly see trends that are important to the analysis.

Ultimately, what this shows us is that there is a weak overall positive relationship between adjusted gross revenue
and IMDB ratings and a similarly weak (but slightly stronger) correlation between profit and IMDB ratings.

In order to further investigate this and provide a recommendation to the Netflix CEO, we need to look at this
based on groupings of genre so that the production studios can have data to guide their decision of what genre
movie will be most impacted by viewer scores."

# Scatter plots with trendlines for each genre
ggplot(data = movie, aes(x = Adjusted_Gross2, y = IMDb_Rating, fill = Genre)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_grid(Genre~. , )
"it's too crowded, so we're going to take the correlation coefficient for each genre, put it into a dataframe,
and then plot that."

# Find correlation coefficient for each genre
action_subset <- subset(movie, Genre == "action")
cor_1 <- cor(action_subset$Adjusted_Gross2, action_subset$IMDb_Rating)

adventure_subset <- subset(movie, Genre == "adventure")
cor_2 <- cor(adventure_subset$Adjusted_Gross2, adventure_subset$IMDb_Rating)

animation_subset <- subset(movie, Genre == "animation")
cor_3 <- cor(animation_subset$Adjusted_Gross2, animation_subset$IMDb_Rating)

biography_subset <- subset(movie, Genre == "biography")
cor_4 <- cor(biography_subset$Adjusted_Gross2, biography_subset$IMDb_Rating)

comedy_subset <- subset(movie, Genre == "comedy")
cor_5 <- cor(comedy_subset$Adjusted_Gross2, comedy_subset$IMDb_Rating)

crime_subset <- subset(movie, Genre == "crime")
cor_6 <- cor(crime_subset$Adjusted_Gross2, crime_subset$IMDb_Rating)

documentary_subset <- subset(movie, Genre == "documentary")
cor_7 <- cor(documentary_subset$Adjusted_Gross2, documentary_subset$IMDb_Rating)

drama_subset <- subset(movie, Genre == "drama")
cor_8 <- cor(drama_subset$Adjusted_Gross2, drama_subset$IMDb_Rating)

fantasy_subset <- subset(movie, Genre == "fantasy")
cor_9 <- cor(fantasy_subset$Adjusted_Gross2, fantasy_subset$IMDb_Rating)

horror_subset <- subset(movie, Genre == "horror")
cor_10 <- cor(horror_subset$Adjusted_Gross2, horror_subset$IMDb_Rating)

musical_subset <- subset(movie, Genre == "musical")
cor_11 <- cor(musical_subset$Adjusted_Gross2, musical_subset$IMDb_Rating)

mystery_subset <- subset(movie, Genre == "mystery")
cor_12 <- cor(mystery_subset$Adjusted_Gross2, mystery_subset$IMDb_Rating)

romance_subset <- subset(movie, Genre == "romance")
cor_13 <- cor(romance_subset$Adjusted_Gross2, romance_subset$IMDb_Rating)

sci_fi_subset <- subset(movie, Genre == "sci-fi")
cor_14 <- cor(sci_fi_subset$Adjusted_Gross2, sci_fi_subset$IMDb_Rating)

thriller_subset <- subset(movie, Genre == "thriller")
cor_15 <- cor(thriller_subset$Adjusted_Gross2, thriller_subset$IMDb_Rating)


correlation_vector <- c(cor_1,cor_2,cor_3,cor_4,cor_5,cor_6,cor_7,cor_8,cor_9,cor_10,cor_11,cor_12,cor_13,cor_14,cor_15)
Name_vector <- c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama","Fantasy","Horror","Musical","Mystery","Romance","Sci-fi","Thriller")

correlation_by_genre <- data.frame(Name_vector, correlation_vector)

# plot it as a column graph to visualize relative correlations by genre
ggplot(data = correlation_by_genre, aes(x = Name_vector, y = correlation_vector, fill = Name_vector)) +
  geom_col() +
  xlab("Genre Name") + 
  ylab("Correlation Coefficients") +
  ggtitle("Correlations between Adj. Gross Revenue & IMDB Rating by Genre")
  

###############################
##### Same but for Profit #####
###############################
action_subset <- subset(movie, Genre == "action")
cor_1 <- cor(action_subset$Profit2, action_subset$IMDb_Rating)

adventure_subset <- subset(movie, Genre == "adventure")
cor_2 <- cor(adventure_subset$Profit2, adventure_subset$IMDb_Rating)

animation_subset <- subset(movie, Genre == "animation")
cor_3 <- cor(animation_subset$Profit2, animation_subset$IMDb_Rating)

biography_subset <- subset(movie, Genre == "biography")
cor_4 <- cor(biography_subset$Profit2, biography_subset$IMDb_Rating)

comedy_subset <- subset(movie, Genre == "comedy")
cor_5 <- cor(comedy_subset$Profit2, comedy_subset$IMDb_Rating)

crime_subset <- subset(movie, Genre == "crime")
cor_6 <- cor(crime_subset$Profit2, crime_subset$IMDb_Rating)

documentary_subset <- subset(movie, Genre == "documentary")
cor_7 <- cor(documentary_subset$Profit2, documentary_subset$IMDb_Rating)

drama_subset <- subset(movie, Genre == "drama")
cor_8 <- cor(drama_subset$Profit2, drama_subset$IMDb_Rating)

fantasy_subset <- subset(movie, Genre == "fantasy")
cor_9 <- cor(fantasy_subset$Profit2, fantasy_subset$IMDb_Rating)

horror_subset <- subset(movie, Genre == "horror")
cor_10 <- cor(horror_subset$Profit2, horror_subset$IMDb_Rating)

musical_subset <- subset(movie, Genre == "musical")
cor_11 <- cor(musical_subset$Profit2, musical_subset$IMDb_Rating)

mystery_subset <- subset(movie, Genre == "mystery")
cor_12 <- cor(mystery_subset$Profit2, mystery_subset$IMDb_Rating)

romance_subset <- subset(movie, Genre == "romance")
cor_13 <- cor(romance_subset$Profit2, romance_subset$IMDb_Rating)

sci_fi_subset <- subset(movie, Genre == "sci-fi")
cor_14 <- cor(sci_fi_subset$Profit2, sci_fi_subset$IMDb_Rating)

thriller_subset <- subset(movie, Genre == "thriller")
cor_15 <- cor(thriller_subset$Profit2, thriller_subset$IMDb_Rating)


correlation_vector <- c(cor_1,cor_2,cor_3,cor_4,cor_5,cor_6,cor_7,cor_8,cor_9,cor_10,cor_11,cor_12,cor_13,cor_14,cor_15)
Name_vector <- c("Action","Adventure","Animation","Biography","Comedy","Crime","Documentary","Drama","Fantasy","Horror","Musical","Mystery","Romance","Sci-fi","Thriller")

correlation_by_genre <- data.frame(Name_vector, correlation_vector)

# plot it as a column graph to visualize relative correlations by genre
ggplot(data = correlation_by_genre, aes(x = Name_vector, y = correlation_vector, fill = Name_vector)) +
  geom_col() +
  xlab("Genre Name") + 
  ylab("Correlation Coefficients") +
  ggtitle("Correlations between Profit & IMDB Rating by Genre")


"In conclusion, we found that a higher movie rating does have a higher adjusted gross revenue. Once we dug
deeper into the question, we found that this relationship is slightly different between Adjusted Gross 
Revenue and Profit. Since maximizing profit the the end goal for companies like Netflix, we focused our
attention on the relationship between profit and rating. Even though there was an overall weak positive
relationship between profit and rating, we found that there were significant differences when you compared
correlations grouped by Genre. Certain genres' profits were more influenced by movie ratings than others.
For example, Dramas and Thrillers see nearly non-existent correlations between these two metrics, while
Biography, Crime, and Fantasy exhibited very strong correlations.

What this means for our Netflix CEO is that, when making movies that are solidly Drama or Thrillers, they
do not need to worry as much about appealing to the mass audience. When viewer ratings do not impact the 
bottom line, they do not need to cater their product to ratings. However, since Biography, Crime, and 
Fantasy are all highly correlated, they should focus their efforts on making a crowd-pleasing movie in 
order to rake in as much profit as possible.

Another interesting trend was that only a handful of genres saw negative relationships between Profit and
Rating, and those were Documentary (small sample size), Romance, and Thriller (very low relationship). 
Since Romance is the only one that is negative, it implies that the worse the movie is perceived, the 
better is performs in terms of profit."

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

"How can we present this? can we present this as a skit? "# team-viz
