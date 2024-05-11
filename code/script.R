# Setting the correct working directory (Location where our data & R files are stored)
# We set it via relative paths such that we can work on the same code without 
# the need to change it all the time

# Clear current working environment
rm(list=ls())
# Get the directory of the current script
script_dir <- getwd()
Spotify <- read.csv("./data/spotify-2023.csv")


######################################### STEP 1 QUESTIONS WE WANT TO ANSWER #########################################

# 1. Which artists are the most famous ones ? WORK IN PROGRESS
# 2. Does it help more to be in a specific playlist stream-wise or are their 
#    effect the same ?
# 3. Is there a "mix" for the perfect song considering elements such as bpm, key
#    energy, valence etc. ? WORK IN PROGRESS
# 4. What are the most prevelant features when it comes to estimating streams 
#.   (Multiple Linear Regression, Feature Selection)
# 5. What Relationships can we find between features such as energy, danceability
#.   etc. ? Meaning, does one imply the other? (Partly Feature Selection)
# 6. Does the artist count in a song have any impact whatsoever on the success
#    of a song ?
# 7. How accurately can we predict an artists streams, based on the relase date
#.   (Regression with x-axis date, y-axis Streams, then do inference)
# 8. What about Outliers/High Leverage Points ? Do they influence regression results ? WORK IN PROGRESS
# 9. Can we find any relationship between the key / mode of a song and its 
#.   success in terms of streams? (Categorical Regressor key/mode)
# 10. Dancability & Energy similar distribution --> can we find any 
#.    significant difference in their means? (ANOVA / Tukeys Method) // or
#.    in lecture we did it with categorical variables after using dummy variables
#.    to make them to numerical ones, so we could do it on key/mode 
# 11. Can we predict the number of streams on Spotify based on the song’s 
#.    presence in Spotify, Apple Music, Deezer, and Shazam charts? 
#.    What is the correlation between these variables?
# 12. Is there a linear relationship between the song’s danceability, valence, 
#.    energy, acousticness, instrumentalness, liveness, speechiness, 
#.    and its popularity (number of streams or presence in charts)?
# 13. How well does (any of our) model fit the data? 
#.    What is the residual standard error and the R^2 statistic?
# 14. s the variance of the error terms constant or do we see patterns in the residual plots?
# 15. Are there any non-linear relationships between the predictors and the response? 
#.    Would a polynomial regression provide a better fit? (Again, to any model)
# 16. Is there a significant difference in the number of streams or presence 
#.    in charts between songs released in different years, months, or days (ANOVA/ANCOVA)
# 17. Does the number of artists contributing to a song have an effect on its popularity,
#.    after controlling for other factors?
# 18. Can we improve the model fit by using a generalized linear model that 
#.    allows for response distributions other than the normal distribution?
#.    (again, for any model)
# 19. Can we accurately classify songs into high-stream and low-stream categories 
#.    based on the given features? (Discriminant Analysis)
# 20. Can we find a difference in the stuff like valence, danceabilty etc. for songs that were
#.    released in summer vs winter ? WORK IN PROGESS


################## LIBRARIES ##################
library(corrplot)


################## FUNCTIONS ################## 

# REMOVE PLOTS
shutoff_plots <- function() {
  # Reset graphic windows
  if (!is.null(dev.list())) {
    dev.off()
  }
}


# MULTIPLE PLOTS 
setup_multiple_plots <- function(dataframe) {
  # Get number of columns
  num_of_columns <- length(names(dataframe))
  # Set up sizes
  par(mar=c(2,2,2,2))
  # Set up the graphics window to have a suitable number of rows and columns
  par(mfrow = c(ceiling(sqrt(num_of_columns)), ceiling(sqrt(num_of_columns))))
  
}

# FOR PAIRS PLOTS (from Lecture)
## panel.hist function
## puts histograms on the diagonal

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## panel.cor function
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}



######################################### STEP 2 DATA CLEANING & FILTERING #########################################

##### OVERVIEW OF DATA ######
 
str(Spotify)
summary(Spotify)

##### HANDLE MISSING DATA ######

# Approach : Remove all rows where there is at least one missing value in some column
Spotify <- na.omit(Spotify)


##### HANDLE DUPLICATES ######

# Check how many duplicates there are in the dataset
duplicates <- sum(duplicated(Spotify))
# Remove all duplicates from the dataset
Spotify <- unique(Spotify)


##### DATA INTO THE RIGHT FORMATS #####


### CATEGORICAL COLUMNS ###

# Next, we change columns with categorical values into the right format (i.e. factor)
Spotify$key <- as.factor(Spotify$key)
Spotify$mode <- as.factor(Spotify$mode)


### SOME MORE (columns that were in the wrong type for some reason) ###
Spotify$streams <- as.integer(Spotify$streams)
Spotify$in_deezer_playlists <- as.integer(Spotify$in_deezer_playlists)
Spotify$in_shazam_charts <- as.integer(Spotify$in_shazam_charts)


# Drop created NA's by coercion
Spotify <- na.omit(Spotify)


##### CHANGING STRUCTURES TO OUR LIKING #####


### SEPARATING MAIN ARTIST AND FEATURES ###
# We also want to separate the features in a song. It is important to notice that a song has a main artist and features.
# In our data, in the column artist.s._name, the names are separated by commas and the main artist stands at the first
# position

# Split the artist name column at each comma
split_col <- strsplit(Spotify$artist.s._name, ",")
# Get the maximum number of artists in a single cell
max_artists <- max(sapply(split_col, length))
# Make all the vectors the same length by padding with NA
split_col <- lapply(split_col, function(x) { c(x, rep(0, max_artists - length(x))) })
# Convert the list to a dataframe
split_df <- do.call(rbind, split_col)
# Name the columns # TODO : manually, as they don't stay that name after binding with original DF
#names(split_df) <- c("Main Artist", paste0("Feat ", seq_len(max_artists - 1)))
# Merge into our original dataframe
Spotify_with_splitted_artists <- cbind(Spotify, split_df)
# Write to .csv (for checking if we did it correctly)
write.csv(Spotify_with_splitted_artists, './data/spotify-2023-splitted-artists.csv', row.names = FALSE)



### DATE COLUMN ### 

# We have the date of release given in three different columns (day,month,year)
# We want to change this into one column and save it as datatype date
# Typical format for change : '2022-06-22' as string, then convert using as.Date

# First we create a new column and store the date in the wanted format 
Spotify$released_date <- paste(Spotify$released_year, Spotify$released_month, Spotify$released_day, sep = '-')
# We give it the wanted datatype of DATE
Spotify$released_date <- as.Date(Spotify$released_date, format = "%Y-%m-%d")
# We keep the date structure stored, as we will need it later on for question 20
Spotify_dates <- subset(Spotify, select = c(released_year, released_month,released_day))
# We can directly drop our other 3 columns storing the dates
Spotify <- subset(Spotify, select = -c(released_year, released_month,released_day))






######################################### STEP 3 EXPLORATION OF DATA #########################################


# Access ONLY numerical data
numerical_Spotify <- Spotify[sapply(Spotify, is.numeric)]
# Drop NA if needed
numerical_Spotify <- na.omit(numerical_Spotify)


##### CORRELATION MATRIX #####

shutoff_plots()

# Calculate correlation matrix
cor_matrix <- cor(numerical_Spotify) # Only use numeric data
# Create the correlation plot
corrplot(cor_matrix,type = 'upper', method = "number", number.cex = 0.5)




##### HISTOGRAMS #####

shutoff_plots()
setup_multiple_plots(numerical_Spotify)

# Now, create a histogram for each column
for(col in names(numerical_Spotify)) {
  hist(numerical_Spotify[[col]], breaks = 50, main = paste("Histogram of", col), xlab = col)
}

##### KERNEL DENSITY GRAPHS #####

shutoff_plots()
setup_multiple_plots(numerical_Spotify)

# Loop through each numerical column
for(col in names(numerical_Spotify)) {
  # Create a density object for the column
  dens <- density(numerical_Spotify[[col]], na.rm = FALSE)
  # Create a new plot
  plot(dens, main = paste("Kernel Density Plot of", col), xlab = col)
  # Add a filled polygon to the plot
  polygon(dens, col = "blue", border = "black")
}


##### PAIR GRAPHS #####

shutoff_plots()
setup_multiple_plots(numerical_Spotify)

pairs(numerical_Spotify, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)


####################################### QUESTION 1 #######################################
# First we will do a ranking of all the main artists, i.e. not the features
# and group their streams to do a ranking of how popular they are (in terms of 
# streams)


# Create an empty list to store the stream sums
streams_per_main_artist <- list()
energy_per_main_artist <- list()

group_by_artists <- split(Spotify_with_splitted_artists, Spotify_with_splitted_artists$"1")

for(i in 1:length(group_by_artists)) {
  # Calculate the sum for streams
  sum_value <- sum(group_by_artists[[i]]$streams, na.rm = TRUE)
  # Calculate the mean for specific elements in the song
  # e.g. energy
  mean_value <- mean(group_by_artists[[i]]$energy_., na.rm = TRUE)
  
  streams_per_main_artist[[i]] <- data.frame(group_column = names(group_by_artists)[i], sum = sum_value)
  energy_per_main_artist[[i]] <- data.frame(group_column = names(group_by_artists)[i], mean = mean_value)
}

# Combine all the dataframes 

grouped_main_artists_and_streams <- do.call(rbind, streams_per_main_artist)
grouped_main_artists_and_energy <- do.call(rbind, energy_per_main_artist)

grouped_main_artists <- merge(grouped_main_artists_and_streams, grouped_main_artists_and_energy, by = 'group_column')




# Order by the streams
# This dataframe gives us the ranking of main artists !
grouped_main_artists <- grouped_main_artists[order(grouped_main_artists$sum, decreasing = TRUE), ]

# Rename columns
colnames(grouped_main_artists) <- c("Artist", "Streams", 'Energy')

# Select the top 10 artists
top_10 <- grouped_main_artists[1:10, ]


shutoff_plots()

barplot(top_10$Streams, names.arg = top_10$Artist, xlab='Artists', ylab='Streams')



####################################### QUESTION 20 #######################################
# Can we find a difference in the stuff like valence, danceabilty etc. for songs that were released in summer vs winter ?

### !!!!!! is it a problem that we may have less data for winter entries than summer entries or vice versa ??? !!!!!! ####

# We will use ANOVA/Tukey for this analysis
# In particular, we will group songs by summer & winter time and check danceability & energy. 
# Our hypothesis is that in summer time, songs will have more danceability & energy percentages
# To put it in a statistical framework, we will compare the mean of the two groups (summer & winter)
# with respect to A. danceability and B. energy.

# We define "summer time" as the duration from may-august (5-8)
# We define "winter time" as the duration from october-january (10-12 % 1)

# Add a new column to the Spotify dataframe which contains the summer/winter information
# Other months will be just called "other"
Spotify$time_of_year <- ifelse(Spotify_dates$released_month %in% c(5, 6, 7, 8), "summer",
                               ifelse(Spotify_dates$released_month %in% c(10, 11, 12, 1), "winter", "other")) 





# Perform the ANOVA test ; Problem here : we perform between summer & winter & other !!!!
result_anova <- aov(Spotify$energy_. ~ Spotify$time_of_year, data = Spotify)

# Print the summary of the ANOVA test
summary(result_anova)


shutoff_plots()

# Plot the ANOVA result
boxplot(Spotify$energy_. ~ Spotify$time_of_year, data = Spotify, main = "ANOVA Results", xlab = "Time of Year", ylab = "Energy")




# So we do Tukey to perform between classes :

result_tuskey <- TukeyHSD(result_anova)

print(result_tuskey)

# and see p value 0.036 between winter & summer --> we reject H0 and say that there
# is a significant difference, as we assumed ! 
# In the ANOVA plot, we can see that the mean energy for summer is higher than for 
# winter ; just what we expected !


# Let us try for danceability

result_anova <- aov(Spotify$danceability_. ~ Spotify$time_of_year, data = Spotify)

shutoff_plots()

boxplot(Spotify$energy_. ~ Spotify$time_of_year, data = Spotify, main = "ANOVA Results", xlab = "Time of Year", ylab = "Danceability")
result_tuskey <- TukeyHSD(result)

print(result_tuskey)

# Again, we get similar results, as expected !


####################################### QUESTION 3 #######################################
# Is there a "mix" for the perfect song considering elements such as bpm, key
# energy, valence etc. ?


# For this question, we will look at the most successful songs (in terms of streams)
# and will see if we can find any pattern

# For this, let us check the correlation plots of streams with all other numerical features

# Let us calculate the correlation we are interested in
correlations_with_streams <- cor(numerical_Spotify$streams, numerical_Spotify[, names(numerical_Spotify) != "streams"])

shutoff_plots()

# Create the correlation plot
corrplot(correlations_with_streams, method = "number", number.cex = 0.5)

# Here, we do not see any pattern but that a lot of streams correlates with being in playlists
# and charts, which was more or less obvious to us already

# Now let us look at 2 different aspects

# 1. We will look at the top 10 artists, gather all their songs (where they are a main artists)
# and plot the correlation

# Create a vector of the top 10 artists

top_10_artists <- top_10$Artist

# Filter the dataframe given the artists

filtered_top_10 <- subset(Spotify_with_splitted_artists, `1` %in% top_10_artists)

# Also, we only want to take the numerical data and remove all the feature informations
filtered_top_10 <- filtered_top_10[sapply(filtered_top_10, is.numeric)]
filtered_top_10 <- filtered_top_10[ , !(names(filtered_top_10) %in% c('2', '3', '4', '5', '6', '7', '8'))]
  


# Let us calculate the correlation we are interested in
correlations_with_streams <- cor(filtered_top_10$streams, filtered_top_10[, names(numerical_Spotify) != "streams"])

shutoff_plots()

# Create the correlation plot
corrplot(correlations_with_streams, method = "number", number.cex = 0.5)

# We see a strong negative correlation with the year a song was released in --> people tended to listen to these 
# artists more in earlier years ? artists became more unfamous ? we could try to make an analysis on that

# but, again no real correlation seen with danceability, energy etc.



#2. We will look at the top 200 songs and plot the correlation (why 200 ? roughly 200 songs when we picked the top 10 artists)

# First, order by streams in descending order
Spotify_ordered <- Spotify[order(Spotify$streams, decreasing = TRUE), ]

# Pick the first 200 entries

top_200_songs_by_streams <- Spotify_ordered[1:200,]

# Apply filter for only numerical data
filtered_top_200_songs_by_streams <- top_200_songs_by_streams[sapply(top_200_songs_by_streams, is.numeric)]




# Let us calculate the correlation we are interested in
correlations_with_streams <- cor(filtered_top_200_songs_by_streams$streams, filtered_top_200_songs_by_streams[, names(filtered_top_200_songs_by_streams) != "streams"])

shutoff_plots()

corrplot(correlations_with_streams, method= 'number', number.cex = 0.5)

# Again, no strong correlations (we have 0.14 for acousticness atleast)



####################################### QUESTION 8 #######################################
#What about Outliers/High Leverage Points ? Do they influence regression results ?

# As we are mostly focused on the relationship of streams and all the other
# features, we will use streams as the dependent variable

# Let us plot all the scatterplots and add regression lines

shutoff_plots()
setup_multiple_plots(numerical_Spotify)

# Get the column names of the independent variables (all but streams)
independent_vars <- names(numerical_Spotify)[names(numerical_Spotify) != "streams"]



# Create a plot for each independent variable
for (var in independent_vars) {
  # Create a scatterplot
  plot(numerical_Spotify[[var]], numerical_Spotify$streams, 
       main = paste("Scatterplot of streams vs", var),
       xlab = var, ylab = "streams")
  
  # Add a regression line
  abline(lm(numerical_Spotify$streams ~ numerical_Spotify[[var]]), col = "red")
}


# streams vs. in spotify charts :
  # we have a weird high leverage point & also outlier
  # was in the most charts, but has nearly no streams
  # I'd say we could take that one out of the analysis

  # weirdly we only see it in spotify charts , in deezer, apple etc.
  # often more points in that 'field' or they are just a high leverage
  # point or just an outlier




########################## QUESTION 4 #############################
# What are the most prevelant features when it comes to estimating streams?
# What is the best model for predicting streams ? 
# (Multiple Linear Regression, Feature Selection)

# Let us start with all the numerical features that we have as independent
# variables and use streams as the dependent variable in the framework of
# Multiple Linear Regression (MLR)

# Get the column names of the independent variables
independent_vars <- names(numerical_Spotify)[names(numerical_Spotify) != "streams"]

# Create a formula for the regression
# The paste function is used to create a string that contains the names of the independent variables separated by +
# The as.formula function is used to convert this string into a formula

reg_formula <- as.formula(paste("streams ~", paste(independent_vars, collapse = " + ")))

# This we also used in class
# Perform the regression
model <- lm(reg_formula, data = numerical_Spotify)


# Print the summary of the model
summary(model)


# Let us now do backward feature selection as proposed in class 
# We will use the step function provided in R, which selects the best
# model according to AIC
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/step


# Perform backward stepwise selection
selected_model <- step(model, direction = "backward")

# Print the summary of the selected model
summary(selected_model)



# check the residual plot

shutoff_plots()
plot(fitted.values(selected_model), residuals(selected_model), pch=20)
abline(h=0)

# we see an increasing trend of residuals and have no constant variance. (-> heteroscedasticity)
# For linear regression, we use the assumption of homoscedasticity, which is not given here.

#Heteroscedasticity can lead to inefficient parameter estimates,
#though they will still be unbiased. 
#This means that while your model estimates are on average correct,
#they will have larger variance, leading to wider confidence intervals
#or larger standard errors.

#If you observe such a trend, you might want to explore data transformations 
#or different modeling approaches that can handle heteroscedasticity. 
#For instance, you could try transforming the dependent variable 
#(e.g., using a log or square root transformation) or you could consider 
#using weighted least squares (WLS) regression instead of ordinary least squares (OLS) 
#regression.

# Let us try some transformations to fix the heterosecadsticity problem
##### !!! should we do that before or after feature selection ?
# Also, I think it might not matter at all, because the residual plots look the same
# before / after feature selection

#Before Feature Selection: Transforming your variables before feature selection can be beneficial because it might help to reveal relationships between the predictors and the response that were not apparent in the untransformed variables. This could lead to a better set of features being selected. However, keep in mind that transforming your variables changes their interpretation, so the features selected on the transformed scale might not be the same as those that would be selected on the original scale.
#After Feature Selection: If you perform feature selection first and then transform your variables, you might end up with a model that’s easier to interpret, since the features have been selected on their original scale. However, this approach might not reveal relationships that could be apparent in the transformed variables.

# Log transformation (before feature selection)

# use a copy we can work on
numerical_Spotify_copy <- numerical_Spotify

# Apply the log transformation to the 'streams' column
numerical_Spotify_copy$streams <- log(numerical_Spotify_copy$streams)

# log reduces a bit, sqrt doesnt really help


# Get the column names of the independent variables
independent_vars <- names(numerical_Spotify_copy)[names(numerical_Spotify_copy) != "streams"]
reg_formula <- as.formula(paste("streams ~", paste(independent_vars, collapse = " + ")))

# This we also used in class
# Perform the regression
model <- lm(reg_formula, data = numerical_Spotify_copy)

# check the residual plot
shutoff_plots()
plot(fitted.values(model), residuals(model), pch=20)
abline(h=0)


















########################## MAYBE ##################################

# Next, we notice how we have huge differences in the number of streams and that the values get really large
# Thus, we want to scale the data


# First we have to change the datatype to int 
Spotify$streams <- as.integer(Spotify$streams)

# We created NAs by coercion (?), so we drop them
# happens e.g. when we change in_deezer_playlists : could be that it is stored as NULL or sum when it is in no playlist : In that case
# we shouldnt remove NAs but turn NULL to 0 in conversion --> we have to check that !
Spotify <- na.omit(Spotify)


# APPROACH 1 : NORMAL SCALING
# Problem : negative values
mean_value <- mean(Spotify$streams, na.rm = TRUE)
std_dev <- sd(Spotify$streams, na.rm = TRUE)

Spotify$streams <- (Spotify$streams - mean_value) / std_dev

# APPROACH 2 : LOG SCALING

Spotify$streams <- log(Spotify$streams)



# APPROACH 3 : MIN-MAX SCALING

min_value <- min(Spotify$streams, na.rm = TRUE)
max_value <- max(Spotify$streams, na.rm = TRUE)

Spotify$streams <- (Spotify$streams - min_value) / (max_value - min_value)


# Knowing that the artist count on a song will nearly always be between 1 and something like 8 at maximum, we can 
# also define it as a factor
#Spotify$artist_count <- as.factor(Spotify$artist_count) # Also unsure about this one
#Spotify$artist.s._name <- as.factor(Spotify$artist.s._name) # This one I don't know if we should do it


hist(Spotify$streams, main = "Histogram", xlab = "Values", col = "blue", border = "black")

# Because we see a long tail distribution, we apply log transformation


hist(log(Spotify$streams), breaks = 50, main = "Histogram", xlab = "Values", col = "blue", border = "black")

# We also look at other transformations

#MinMax
min_value <- min(Spotify$streams, na.rm = TRUE)
max_value <- max(Spotify$streams, na.rm = TRUE)

Streams_minmax <- (Spotify$streams - min_value) / (max_value - min_value)

hist(Streams_minmax, breaks = 50, main = "Histogram", xlab = "Values", col = "blue", border = "black")

#Normal distribution
mean_value <- mean(Spotify$streams, na.rm = TRUE)
std_dev <- sd(Spotify$streams, na.rm = TRUE)

Streams_normalized <- (Spotify$streams - mean_value) / std_dev


hist(Streams_normalized, breaks = 50, main = "Histogram", xlab = "Values", col = "blue", border = "black")




########################## MAYBE ##################################






