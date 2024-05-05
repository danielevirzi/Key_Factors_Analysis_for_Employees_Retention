# Setting the correct working directory (Location where our data & R files are stored)

setwd('/Users/marlon/Desktop/R_project/data')


Spotify <- read.csv("spotify-2023.csv")

#attach(Spotify) # s.t. we can use the column names directly

#detach(Spotify) # In case we want to load data new, always detach first

#plot(in_apple_charts, streams, pch=20) # pch --> chooses the point shape

# STEP 2 DATA CLEANING & FILTERING

# We first get an overview over our data

str(Spotify)
summary(Spotify)

# We first handle missing data 

# Approach : Remove all rows where there is atleast one missing value in some column

Spotify <- na.omit(Spotify)

# Next we need to bring our data into the right formats

# We have the date of release given in three different columns (day,month,year)
# We want to change this into one column and save it as datatype date
# Typical format for change : '2022-06-22' as string, then convert using as.Date

# First we create a new column and store the date in the wanted format 
Spotify$released_date <- paste(Spotify$released_year, Spotify$released_month, Spotify$released_day, sep = '-')


# We give it the wanted datatype of DATE
Spotify$released_date <- as.Date(Spotify$released_date, format = "%Y-%m-%d")



# We can directly drop our other 3 columns storing the dates
Spotify <- subset(Spotify, select = -c(released_year, released_month,released_day))



# Next, we change columns with categorical values into the right format (i.e. factor)
Spotify$key <- as.factor(Spotify$key)
Spotify$mode <- as.factor(Spotify$mode)



# Knowing that the artist count on a song will nearly always be between 1 and something like 8 at maximum, we can 
# also define it as a factor
Spotify$artist_count <- as.factor(Spotify$artist_count) # Also unsure about this one
Spotify$artist.s._name <- as.factor(Spotify$artist.s._name) # This one I don't know if we should do it


# We change some data that is just stored in wrong formats
Spotify$in_deezer_playlists <- as.integer(Spotify$in_deezer_playlists)
Spotify$in_shazam_charts <- as.integer(Spotify$in_shazam_charts)




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




str(Spotify)





# EXPLORATION OF DATA 
#plot(Spotify$streams, Spotify$in_apple_charts)

### CORRELATION MATRIX ###
# We look at a covariance matrix between our numerical features
# Also possible to change categorical data into numerical by giving them numbers (0,1,2...) , we could do that to have one big 
# covariance matrix 



# We leave out artists names because that doesn't make sense for correlation matrix
# We leave out the release date aswell for the same reason

# First we create a new dataframe where we change the categorical columns we want in the covariance matrix
# s.t. they become numerical ones, using dummy variables

data_copy <- Spotify


dummy_artist_count <- model.matrix(~Spotify$artist_count-1, Spotify)

data_copy <- cbind(data_copy, dummy_artist_count)

data_copy$artist_count <- NULL


dummy_key <- model.matrix(~Spotify$key-1, Spotify)

data_copy <- cbind(data_copy, dummy_key)

data_copy$key <- NULL

dummy_mode <- model.matrix(~Spotify$mode-1, Spotify)

data_copy <- cbind(data_copy, dummy_mode)

data_copy$mode <- NULL

# Remove columns we don't need
data_copy_selected <- data_copy[, !(names(data_copy) %in% c("released_date", "artist.s._name", "track_name"))]


str(data_copy_selected)

cor_matrix_data_copy_selected <- cor(data_copy_selected)

str(data_copy)

# We can see that we have very little correlation effect for the categorical ones, so we do only with the given ones

# copy again
data_copy <- Spotify

data_copy_selected <- data_copy[, !(names(data_copy) %in% c("released_date", "artist.s._name", "track_name", "key", "mode", "artist_count"))]

cor_matrix_data_copy_selected <- cor(data_copy_selected)

# VISUALIZING CORRELATION MATRIX
# If we turn the categorical into numerical ones, you can't see anything in the plot


library(corrplot)

options(repr.plot.width=20, repr.plot.height=20)

corrplot(cor_matrix_data_copy_selected, method = 'number', number.cex = 0.5)
corrplot(cor_matrix_data_copy_selected, method = 'color')
corrplot(cor_matrix_data_copy_selected, method = 'ellipse')

#### CORRELATION MATRIX ########


# We compute the sample size of our data (get the length of any column for this)

n <- length(streams)
n

