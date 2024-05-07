# Setting the correct working directory (Location where our data & R files are stored)

rm(list=ls())

setwd('/Users/marlon/Desktop/R_project')
Spotify <- read.csv("data/spotify-2023.csv")

#attach(Spotify) # s.t. we can use the column names directly

#detach(Spotify) # In case we want to load data new, always detach first

#plot(in_apple_charts, streams, pch=20) # pch --> chooses the point shape


######################################### STEP 1 QUESTIONS WE WANT TO ANSWER #########################################

# 1. Which artists are the most famous ones ?


######################################### STEP 2 DATA CLEANING & FILTERING #########################################

##### OVERVIEW OF DATA ######
 
str(Spotify)
summary(Spotify)

##### HANDLE MISSING DATA ######

# Approach : Remove all rows where there is atleast one missing value in some column
Spotify <- na.omit(Spotify)




##### DATA INTO THE RIGHT FORMATS #####


### DATE COLUMN ### 

# We have the date of release given in three different columns (day,month,year)
# We want to change this into one column and save it as datatype date
# Typical format for change : '2022-06-22' as string, then convert using as.Date

# First we create a new column and store the date in the wanted format 
Spotify$released_date <- paste(Spotify$released_year, Spotify$released_month, Spotify$released_day, sep = '-')
# We give it the wanted datatype of DATE
Spotify$released_date <- as.Date(Spotify$released_date, format = "%Y-%m-%d")
# We can directly drop our other 3 columns storing the dates
Spotify <- subset(Spotify, select = -c(released_year, released_month,released_day))


### CATEGORICAL COLUMNS ###

# Next, we change columns with categorical values into the right format (i.e. factor)
Spotify$key <- as.factor(Spotify$key)
Spotify$mode <- as.factor(Spotify$mode)


### SOME MORE (columns that were in the wrong type for some reason) ###
Spotify$streams <- as.integer(Spotify$streams)
Spotify$in_deezer_playlists <- as.integer(Spotify$in_deezer_playlists)
Spotify$in_shazam_charts <- as.integer(Spotify$in_shazam_charts)


# Drop NA's again
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
write.csv(Spotify_with_splitted_artists, 'data/spotify-2023-splitted-artists.csv', row.names = FALSE)


##### GROUPING ARTISTS FOR NOVEL INFORMATION #####


### RANKING MAIN ARTISTS BY STREAMS ###
## FOR QUESSTION 1 ##
# First we will do a ranking of all the main artists, i.e. not the features
# and group their streams to do a ranking of how popular they are (in terms of 
# streams)



# Get all the main artists



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

barplot(top_10$Streams, names.arg = top_10$Artist, xlab='Artists', ylab='Streams')





######################################### STEP 3 EXPLORATION OF DATA #########################################



##### CORRELATION MATRIX #####

library(corrplot)

# Reset graphic windows
dev.off()


cor_matrix <- cor(numerical_Spotify) # Only use numeric data


# Change the size of the plots in R
options(repr.plot.width=640, repr.plot.height=480)

# Create the correlation plot
corrplot(cor_matrix,type = 'upper', method = "number", number.cex = 0.5)



##### HISTOGRAMS #####



# Access ONLY numerical data
numerical_Spotify <- Spotify[sapply(Spotify, is.numeric)]
# Drop NA if needed
numerical_Spotify <- na.omit(numerical_Spotify)
# Get number of columns
num_of_columns <- length(names(numerical_Spotify))
# Set up the graphics window to have a suitable number of rows and columns
par(mfrow = c(ceiling(sqrt(num_of_columns)), ceiling(sqrt(num_of_columns))))
# Now, create a histogram for each column
for(col in names(numerical_Spotify)) {
  hist(numerical_Spotify[[col]], breaks = 50, main = paste("Histogram of", col), xlab = col)
}

##### KERNEL DENSITY GRAPHS #####

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

pairs(numerical_Spotify, 
      lower.panel = function(x, y) { },
      upper.panel = function(x, y) {
        points(x, y)
      }
)
# COMMENT : Artist count useless, no correlation information







##### FEATURE SELECTION #####






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






