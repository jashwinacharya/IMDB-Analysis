#IMDB Movie Analysis

dataset <- read.csv("movie_metadata.csv", na.strings = "")
backup <- dataset
options(max.print = 5000)
#---------------------------------------------------------Cleaning Data---------------------------------------------------------------

#Finding out which rows have missing values

is.na(dataset$color)
is.na(dataset$director_name)
is.na(dataset$num_critic_for_reviews)
is.na(dataset$duration)
is.na(dataset$director_facebook_likes)
is.na(dataset$actor_3_facebook_likes)
is.na(dataset$actor_2_name)
is.na(dataset$actor_1_facebook_likes)
is.na(dataset$gross)
is.na(dataset$genres)
is.na(dataset$actor_1_name)
is.na(dataset$movie_title)
is.na(dataset$num_voted_users)
is.na(dataset$cast_total_facebook_likes)
is.na(dataset$actor_3_name)
is.na(dataset$facenumber_in_poster)
is.na(dataset$plot_keywords)
is.na(dataset$movie_imdb_link)
is.na(dataset$num_user_for_reviews)
is.na(dataset$language)
is.na(dataset$country)
is.na(dataset$content_rating)
is.na(dataset$budget)
is.na(dataset$title_year)
is.na(dataset$actor_2_facebook_likes)
is.na(dataset$imdb_score)
is.na(dataset$aspect_ratio)
is.na(dataset$movie_facebook_likes)

#Removing the missing data

dataset <- dataset[!is.na(dataset$color),]
dataset <- dataset[!is.na(dataset$director_name),]
dataset <- dataset[!is.na(dataset$actor_3_name),]
dataset <- dataset[!is.na(dataset$plot_keywords),]
dataset <- dataset[!is.na(dataset$movie_imdb_link),]
dataset <- dataset[!is.na(dataset$language),]
dataset <- dataset[!is.na(dataset$country),]
dataset <- dataset[!is.na(dataset$content_rating),]

#Resetting data frame index

rownames(dataset) <- 1:nrow(dataset)

#Dealing with missing numeric data using the median imputation method
unique(dataset$content_rating)

#Taking care of missing values in the Critics Review (3rd) column

median_critic_review_PG <- median(dataset[dataset$content_rating == "PG-13", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "PG-13", "num_critic_for_reviews"] <- median_critic_review_PG
  
median_critic_review_G <- median(dataset[dataset$content_rating == "G", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "G", "num_critic_for_reviews"] <- median_critic_review_G

median_critic_review_Unrated <- median(dataset[dataset$content_rating == "Unrated", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "Unrated", "num_critic_for_reviews"] <- median_critic_review_Unrated

median_critic_review_R <- median(dataset[dataset$content_rating == "R", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "R", "num_critic_for_reviews"] <- median_critic_review_R

median_critic_review_TVPG <- median(dataset[dataset$content_rating == "TV-PG", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "TV-PG", "num_critic_for_reviews"] <- median_critic_review_TVPG

median_critic_review_NR <- median(dataset[dataset$content_rating == "Not Rated", "num_critic_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_critic_for_reviews) & dataset$content_rating == "Not Rated", "num_critic_for_reviews"] <- median_critic_review_NR

#Taking care of missing values in the Duration (4th) Column

median_duration_R <- median(dataset[dataset$content_rating == "R", "duration"], na.rm = TRUE)
dataset[is.na(dataset$duration) & dataset$content_rating == "R", "duration"] <- median_duration_R

#Taking care of missing values in the Gross Column

median_gross_PG <- median(dataset[dataset$content_rating == "PG-13", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "PG-13", "gross"] <- median_gross_PG

median_gross_G <- median(dataset[dataset$content_rating == "G", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "G", "gross"] <- median_gross_G

median_gross_R <- median(dataset[dataset$content_rating == "R", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "R", "gross"] <- median_gross_R

median_gross_NR <- median(dataset[dataset$content_rating == "Not Rated", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "Not Rated", "gross"] <- median_gross_NR

median_gross_P <- median(dataset[dataset$content_rating == "PG", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "PG", "gross"] <- median_gross_P

median_gross_UR <- median(dataset[dataset$content_rating == "Unrated", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "Unrated", "gross"] <- median_gross_UR

median_gross_A <- median(dataset[dataset$content_rating == "Approved", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "Approved", "gross"] <- median_gross_A

median_gross_GP <- median(dataset[dataset$content_rating == "GP", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "GP", "gross"] <- median_gross_GP

median_gross_Passed <- median(dataset[dataset$content_rating == "Passed", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "Passed", "gross"] <- median_gross_Passed

median_gross_X <- median(dataset[dataset$content_rating == "X", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "X", "gross"] <- median_gross_X

median_gross_NC17 <- median(dataset[dataset$content_rating == "NC-17", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "NC-17", "gross"] <- median_gross_PG

median_gross_M <- median(dataset[dataset$content_rating == "M", "gross"], na.rm = TRUE)
dataset[is.na(dataset$gross) & dataset$content_rating == "M", "gross"] <- median_gross_M

#TV-14, TV-PG, TV-G, dont have any gross values.

#Taking care of missing values in the face number poster (13) column

median_facenumber_PG <- median(dataset[dataset$content_rating == "PG-13", "facenumber_in_poster"], na.rm = TRUE)
dataset[is.na(dataset$facenumber_in_poster) & dataset$content_rating == "PG-13", "facenumber_in_poster"] <- median_facenumber_PG

median_facenumber_R <- median(dataset[dataset$content_rating == "R", "facenumber_in_poster"], na.rm = TRUE)
dataset[is.na(dataset$facenumber_in_poster) & dataset$content_rating == "R", "facenumber_in_poster"] <- median_facenumber_R

median_facenumber_P <- median(dataset[dataset$content_rating == "PG", "facenumber_in_poster"], na.rm = TRUE)
dataset[is.na(dataset$facenumber_in_poster) & dataset$content_rating == "PG", "facenumber_in_poster"] <- median_facenumber_P

#Taking care of missing values in the number of user reviews (19th) column

median_UserReview_R <- median(dataset[dataset$content_rating == "R", "num_user_for_reviews"], na.rm = TRUE)
dataset[is.na(dataset$num_user_for_reviews) & dataset$content_rating == "R", "num_user_for_reviews"] <-median_UserReview_R  

#Taking care of missing values in budget (23rd) column

median_budget_PG <- median(dataset[dataset$content_rating == "PG-13", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "PG-13", "budget"] <- median_budget_PG

median_budget_G <- median(dataset[dataset$content_rating == "G", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "G", "budget"] <- median_budget_G

median_budget_R <- median(dataset[dataset$content_rating == "R", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "R", "budget"] <- median_budget_R

median_budget_NR <- median(dataset[dataset$content_rating == "Not Rated", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "Not Rated", "budget"] <- median_budget_NR

median_budget_P <- median(dataset[dataset$content_rating == "PG", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "PG", "budget"] <- median_budget_P

median_budget_UR <- median(dataset[dataset$content_rating == "Unrated", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "Unrated", "budget"] <- median_budget_UR

median_budget_A <- median(dataset[dataset$content_rating == "Approved", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "Approved", "budget"] <- median_budget_A

median_budget_GP <- median(dataset[dataset$content_rating == "GP", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "GP", "budget"] <- median_budget_GP

median_budget_TV14 <- median(dataset[dataset$content_rating == "TV-14", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "TV-14", "budget"] <- median_budget_TV14

median_budget_M <- median(dataset[dataset$content_rating == "M", "budget"], na.rm = TRUE)
dataset[is.na(dataset$budget) & dataset$content_rating == "M", "budget"] <- median_budget_M

#Taking care of missing aspect ratio columns

median_aspect_PG <- median(dataset[dataset$content_rating == "PG-13", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "PG-13", "aspect_ratio"] <- median_aspect_PG

median_aspect_G <- median(dataset[dataset$content_rating == "G", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "G", "aspect_ratio"] <- median_aspect_G

median_aspect_R <- median(dataset[dataset$content_rating == "R", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "R", "aspect_ratio"] <- median_aspect_R

median_aspect_NR <- median(dataset[dataset$content_rating == "Not Rated", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "Not Rated", "aspect_ratio"] <- median_aspect_NR

median_aspect_P <- median(dataset[dataset$content_rating == "PG", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "PG", "aspect_ratio"] <- median_aspect_P

median_aspect_UR <- median(dataset[dataset$content_rating == "Unrated", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "Unrated", "aspect_ratio"] <- median_aspect_UR

median_aspect_GP <- median(dataset[dataset$content_rating == "GP", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "GP", "aspect_ratio"] <- median_aspect_GP

median_aspect_TVPG <- median(dataset[dataset$content_rating == "TV-PG", "aspect_ratio"], na.rm = TRUE)
dataset[is.na(dataset$aspect_ratio) & dataset$content_rating == "TV-PG", "aspect_ratio"] <- median_aspect_TVPG

#---------------------------Visualizing some data-------------------------------------------


