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


# Next, we notice how we have huge differences in the number of streams and that the values get really large
# Thus, we want to scale the data


# First we have to change the datatype to int 
Spotify$streams <- as.integer(Spotify$streams)

# We created NAs by coercion (?), so we drop them
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


plot(Spotify$streams, Spotify$in_apple_charts)





# We compute the sample size of our data (get the length of any column for this)

n <- length(streams)
n

