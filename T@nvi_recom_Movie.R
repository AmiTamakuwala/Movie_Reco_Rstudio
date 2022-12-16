#"""What is a Recommendation System?
# A recommendation system provides suggestions to the users through a filtering process that is based on user preferences and browsing history.
# The information about the user is taken as an input.
# The information is taken from the input that is in the form of browsing data. 
# This information reflects the prior usage of the product as well as the assigned ratings. 
# A recommendation system is a platform that provides its users with various contents based on their preferences and likings.
# A recommendation system takes the information about the user as an input. 
# The recommendation system is an implementation of the machine learning algorithms.
# """

# Importing Essential Libraries:


install.packages("recommenderlab")
library(recommenderlab)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("reshape2")
library(reshape2)
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)

# Retrieving the Data:
# We will now retrieve our data from movies.csv into movies_data dataframe 
# and ratings.csv into ratings_data.

#-----------------    Movies    ---------------------
movies <- "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\T@nvi_dataset\\movies.csv\\movies.csv"
movies_data <- read.csv(movies)
head(movies_data)


#-----------------    Rating    -----------------------
ratings <- "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\T@nvi_dataset\\ratings.csv\\ratings.csv"
ratings_data <- read.csv(ratings)
head(ratings_data)

#-----------------    Overview the Summary    ----------------

summary(movies_data)
summary(ratings_data)

dim(movies_data)
dim(ratings_data)

#--------------   Data pre-processing   -----------------------------
# """From the above table, we observe that the userId column, as well as the movieId column, consist of integers. 
# Furthermore, we need to convert the genres present in the movie_data dataframe into a more usable format by the users.
# In order to do so, we will first create a one-hot encoding to create a matrix that comprises of corresponding genres for each of the films.
# """

#----------- creating a matrix using one-hot encoding    -----------------------
# checking the genres column from the movie_data table:

movie_genre <- as.data.frame(movies_data$genres, stringsAsFactors = FALSE)
library(data.table)

# --------------       Insights:      --------------------
# we create "movie_genre" table.
# As, we check in "movie_genre" table, we can see in each row there are multiple genres available.
# let's split them.

#creating one more table to split the genre:

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]',
                                        type.convert = TRUE),
                              stringsAsFactors = FALSE)
colnames(movie_genre2) <- c(1:6)

# here, we are saving the table in our local device:
write.table(movie_genre2, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\movie_genre2.csv", row.names = F, sep=",")

#---------------      Insights:   ---------------------------
# here, we created 'movie_genre2' table where we split genre from the each row.

# let's check unique genres and matrix:

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_matrix1 <- matrix(0, 9743, 18)
genre_matrix1[1,] <- list_genre
colnames(genre_matrix1) <- list_genre

for (index in 1:nrow(movie_genre)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_matrix1[1,] == movie_genre2[index, col])
    genre_matrix1[index+1, gen_col] <- 1
  }
}

# saving the table in local device:
write.table(genre_matrix1, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\genre_matrix1.csv", row.names = F, sep=",")

# removing first row, which as the genre list
genre_matrix2 <- as.data.frame(genre_matrix1[-1,], strinsAsFactors=FALSE)


for (col in 1: ncol(genre_matrix2)) {
  genre_matrix2[,col] <- as.integer(genre_matrix2[,col]) # convert from character to integers.
}
str(genre_matrix2)

# Creating a "search matrix" - searching films by specifying the genre:

SearchMatrix <- cbind(movies_data[,1:2], genre_matrix2[])
head(SearchMatrix)

RatingMatrix <- dcast(ratings_data, userId~movieId, value.var = 'rating', na.rm=FALSE)
RatingMatrix <- as.matrix(RatingMatrix[,-1]) #remove userId

# save table in local device:

write.table(SearchMatrix, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\SearchMatrix.csv", row.names = F, sep=",")


#-----------------    'recommenderlab'    -----------------------
#-----------------    'sparse matrix'     ---------------------
# Covert rating matrix into a 'recommenderlab' sparse matrix

RatingMatrix <- as(RatingMatrix, "realRatingMatrix")
RatingMatrix

# Overview of some important parameters for building recommendation system for movies:

# ---------    Model building: register 'recommendation model'    --------------

recommendation_model <- recommenderRegistry$get_entries(dataType = 'realRatingMatrix')
names(recommendation_model)
lapply(recommendation_model,"[[", "description")

# Implementing a single model in the RStudio.


# ----------------    "Collaborative Filtering"   ----------------
# Collaborative filtering suggesting movies to the users based on collecting preferences from many other users.

recommendation_model$IBCF_realRatingMatrix$parameters

# with the help of 'recommenderlab', we can compute similarities between users.

similarity_matrix <- similarity(RatingMatrix[1:4, ],
                                method = 'cosine',
                                which = 'users')
as.matrix(similarity_matrix)
image(as.matrix(similarity_matrix), main = "User's Similarity")

# Portray the similarity that is shared between the films.

movie_similarity <- similarity(RatingMatrix[, 1:4],
                               method = 'cosine',
                               which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main  ="Movies Smilarity")
rating_values <- as.vector(RatingMatrix@data)
unique(rating_values) # extracting unique ratings
Table_of_Ratings <- table(rating_values) # creating a count of movie ratings.
Table_of_Ratings


# --------------    Most Viewed Movies    -------------------
# --------------    Visualization     -----------------------

install.packages("ggplot2")
library(ggplot2)

movie_views <- colCounts(RatingMatrix) # count views for each movie.
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE),] # sort by number of views.
table_views$title <- NA

for (index in 1:9743) {
  table_views[index,3] <- as.character(subset(movies_data,
                                              movies_data$movieId == table_views[index,1])$title)
}
table_views[1:10,]

# save table in local device:

write.table(table_views, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\table_views.csv", row.names = F, sep=",")


# Insights:
#------ 1. create movie_views for count of the view.
#------ 2. after that created table_views table for total count of views of each movies.
#------ 3. now, open the table_view table and we can see that "Forest Gump(1994)" is the most viewed movie with count 329 here.
#------ 4. decreasing = True gives us views in descending order.

#-------------    Visualizing a bar plot    -------------------
# bar plot for the total number of views of the top films.

ggplot(table_views[1:10, ], aes(x = reorder(title, -views), y = views )) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  geom_text(aes(label=views), vjust = -0.3, size = 3.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Total Views of the Top 10 Films")

# insights:
#------------ 1. visualize the total views of the each movie in bar chart.
#------------ 2. as, bar plot is showing us in sort as we include x = reorder.
#------------ 3. visualized the same things as we can see in "table_view" table

#-------------    heatmap of Movie Ratings    -----------------
# Visualizing a heatmap of the movie rating:

image(RatingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")


#--------------------------   Data Preparation    -----------------

movie_rating <- RatingMatrix[rowCounts(RatingMatrix) > 50,
                             colCounts(RatingMatrix) > 50]
movie_rating

# describing matrix of relevant users.

min_movies <- quantile(rowCounts(movie_rating), 0.98)
min_users <- quantile(colCounts(movie_rating), 0.98)
image(movie_rating[rowCounts(movie_rating) > min_movies,
                   colCounts(movie_rating) > min_users],
      main = "Heatmap of the top users and movies")

# Insights:
#--- in heatmap dark black box is shows up high rated and seeing by users. 
#------- which called most recommended movies.
#--- on the other hand white color or light grey is showing low rated movie, 
#--- where most of the users are not much interested to see those movies which has low ratings.
#--- like in this chart white box is in between 2 to 4.
#--- which means rating is 2.5 where users are low in range.


#---------------    Distribution of the average ratings   -----------
#--- Visualizing the distribution of the average ratings per user.

average_ratings <- rowMeans(movie_rating)
qplot(average_ratings, fill = I("skyblue"), col = I("red")) + 
        ggtitle("Distribution of the average rating per User")


#---------------------    Data Normalization    ---------------------

normalized_ratings <- normalize(movie_rating)
sum(rowMeans(normalized_ratings) > 0.00001)
image(normalized_ratings[rowCounts(normalized_ratings) > min_movies,
                         colCounts(normalized_ratings) > min_users],
      main = "Normalized Rating of the Top Users")

#--- Insights:
#--- here, we created "normalized_rating" for the top users.
#--- where we use matrix which gave us list with null values, characters from 1 to 436,
#--- also integer from 1 to 36214, and factor is list. 
#---  we normalized list into mean value, value is in between 1 to 378.


#----------------- Data Binarization    ------------------------

binary_min_movies <- quantile(rowCounts(movie_rating), 0.95)
binary_min_users <- quantile(colCounts(movie_rating), 0.95)
# movies_watched <- binarize(movie_rating, minRating = 1)
good_rated_films <- binarize(movie_rating, minRating = 3)
image(good_rated_films[rowCounts(movie_rating) > binary_min_movies,
                       colCounts(movie_rating) > binary_min_users],
      main = "Heatmap of the top users and movies")

#---------   Insights:
#--- in "binary_min_movies", we can see min_movies are 249.
#--- in "binary_min_users", there are 163 users.
#--- In "good_rated_films" we only include data from "binary_min_movies" and "binary_min_users".
#--- So, only top users and and top movies are included in this map(binarize heatmap)


#-------------    "Collaborative Filtering System"    --------------------
# Splitting the dataset into train  and test dataset.
# where "train" dataset ratio will be "80%",
# and "test" dataset ratio will be 20%.


sample_data <- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_rating),
                      replace = TRUE,
                      prob = c(0.8, 0.2)) # train and test data ratio.
training_data <- movie_rating[sample_data, ]
testing_data <- movie_rating[sample_data, ]

#--- now we divided the data into train and test set.
#---- So, we can move further step, which is "build the recommendation System".


#-----------------    "Build the Recommendation System   -----------------

recommendation_system <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                                    method = "IBCF",
                                    parameter = list(k = 30))
recommen_model
class(recommen_model)

#----------------------    Exploring the recommendation System model:   ---------------------------

model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

# Insights:
#------- here, we check with 20 top_items in Heatmap. 
#------- where we include row and column.
#------- black box gives us top most rating movies which we can recommend.

# Visualize sum of rows and columns with the similarity of the obejects above 0

sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("skyblue"), col = I("yellow")) + ggtitle("Distribution of the column count")

# Insights:
#--------- we took sum of rows and sum columns, we get int from 1 to 436, where we use bins = 30.
#--------- and in the heatmap highest distribution at the sum_col is 25 where there are 40+ movies.


#---------------    The number of items to recommend to each user   --------------
top_recommendations <- 10
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

#------------------- Insights:
#-------  Here, we get our top users and the total count of top users are 305.
#-------   who are comes in top 10 recommendation predictive model.


# now, let's check our first recommendation users:
#--------------   Recommendation for the first user   ----------------

user1 <- predicted_recommendations@items[[1]]
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10) {
  movies_user2[index] <- as.character(subset(movies_data,
                                             movies_data$movieId == movies_user1[index])$title)
}
movies_user2

# ----------    Insights:   --------------
# we already predicted our top 10 users in predicted_recommendation 
# from there we get movie_user1 and movie_users2.
# and from movies_data we get movieId which gives us top users with its movieId.
# and from the movieId we can recommended top movies to them. which are as follow..
# [1]"Interview with the Vampire: The Vampire Chronicles (1994)"
# [2] "North by Northwest (1959)"                                
# [3] "Mary Poppins (1964)"                                      
# [4] "Cool Hand Luke (1967)"                                    
# [5] "Butch Cassidy and the Sundance Kid (1969)"                
# [6] "Star Trek: First Contact (1996)"                          
# [7] "Full Monty, The (1997)"                                   
# [8] "Gattaca (1997)"                                           
# [9] "Untouchables, The (1987)"                                 
# [10] "Shakespeare in Love (1998)"                               

#------------   Matrix with the recommendations for each user   -------------

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x) { as.integer(colnames(movie_rating)[x]) })
dim(recommendation_matrix)
recommendation_matrix[, 1:4]

# save table in local device:

write.table(recommendation_matrix, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\recommendation_matrix.csv", row.names = F, sep=",")


#--- In "recommendation_matrix" table we get matrix from our top 10 users. 
#--- here, our predicted method is "IBCF".
#--- from the predictive analysis we used "topNList", "ratings", "ratingMatrix", "traindata"..
#--- as we can see all this data information from the "recommendation_model" search button.



# after finding the matrix let's distribute the data.
#-----------   Distribution of the Number of Items for IBCF    ------------

number_of_items <- factor(table(recommendation_matrix))
chart_title <- "Distribution of the Number of Items for IBFC"
qplot(number_of_items, fill=I("skyblue"), col=I("yellow")) + 
  ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 5)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)

for(i in 1:5) {
  table_top[i,1] <- as.character(subset(movies_data,
                                        movies_data$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie Title", "No. of Items")
head(table_top)

# save table in local device:

write.table(table_top, file = "C:\\Users\\amita\\Downloads\\FreeLance\\T@nvi-recommender-latest\\Exported_table\\table_top.csv", row.names = F, sep=",")



#------  Insights:
#--- from the "recommendation_matrix" we get the number_of_items and, 
#--- we sort them as descending order.
#--- after sorting the matrix we create "number_of_items_top" where we get the top matrix.
#--- from the "number_of_items_top" we visualize(qplot) the matrix on the bar chart. 
#--- and at last, from the matrix and coding we get our final top list of the "Movie" with movieId.
#--- here is the top movie list:

#                                                  Movie Title     No. of Items
# 1207                            To Kill a Mockingbird (1962)           33
# 111                                       Taxi Driver (1976)           29
# 908                                North by Northwest (1959)           28
# 912                                        Casablanca (1942)           27
# 541                                      Blade Runner (1982)           26


























 

