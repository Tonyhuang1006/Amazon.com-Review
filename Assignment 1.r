# BAN6065 Individual Assignment 1: Building Perceptual Map Using Amazon.com Review Data 
# Name:Yihua Huang
# Student ID: 06610003
# Section#: Forest




#*********************************************************************************#
# STEP 0: INSTALL AND IMPORT NECESSARY PACKAGES

# Package for accessing and working with json files
#install.packages("jsonlite")
# Package to zip/unzip files
#install.packages("R.utils")
# Assume you have had the following packages installed
# install.packages("tidyverse")
# install.packages("ggplot2")

#import necessary libraries
library(jsonlite)
library(R.utils)
library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)

#*********************************************************************************#
# STEP 1: LOAD/READ IN JSON FILES INTO A MERGED COMPLETE DATAFRAME

# Set the working directory
# Currently the working directory is set to C:/R. Change it if you put the data in a different directory.
setwd("C:/R")

# Read in the review dataset
# Due to the data size, the review data file is zippped, in the .gz format.
# gunzip("All_Beauty.json.gz") # You can use the "gunzip" function to unzip the review data file if you want to, but it may not be necessary.
review.df <- stream_in(file("All_Beauty.json/All_Beauty.json"))
#review_appliance <- stream_in(file("Appliances.json/Appliances.json"))


# Is there any data cleaning you could do on review.df?
review.df <- review.df %>% 
  clean_names() 

review.df <- review.df[,c("asin","review_text")]



#write.csv(review.df, file="AllBeautyReviews.csv") # Not necessary, but you can save it to .csv if you want to.


# Read in the meta dataset in the same way as the review dataset
# Metadata include product information, such as brand name, color, size, package type, etc.
# The meta dataset and the review dataset can be linked using ASIN (Amazon Standard Identification Number)
meta.df <- stream_in(file("meta_All_Beauty.json/meta_All_Beauty.json"))

# Is there any data cleaning you need to do on meta.df?
meta.df <- meta.df %>% clean_names() %>% distinct(asin, .keep_all= TRUE)
meta.df <- meta.df[,c("asin","brand")] # We just need each product's ASIN and brand information.


# Merge datasets by ASIN 
data <- merge(meta.df, review.df, by="asin")


#*********************************************************************************#
#STEP 2: OBTAIN THE LIST OF BRANDS TO BUILD PERCEPTUAL MAP

# Some data cleaning may be needed, dependent of the product category, e.g., dropping observations without brand information, combining the same brands with various name variances, etc.
# Note what's needed to do is dependent of the product category 
data$brand <- str_replace_all(data$brand, "Oral B", "Oral-B")
data$brand <- str_replace_all(data$brand, "Philips Norelco", "Norelco")
data <- data[,!is.na(data$brand)] # drop observations without brand information

# Find the top 20 brands in terms of total number of reviews
brands <- data %>%
  select(brand) %>%
  group_by(brand) %>%
  count()%>%
  arrange(desc(n)) %>%
  na.omit() %>%
  ungroup() %>%
  top_n(20, wt = n)

top_20_brands <- brands$brand
top_10_brands <- brands$brand[10: 20]


  
#*********************************************************************************#
#STEP 3: CONSTRUCT THE SIMILARITY MATRIX

# Construct the similarity matrix using the concept of Co-occurrence
# Calculate Co-occurrence using self-defined function
cooccurrence <- computeCooccurrence(data, top_20_brands)

#write_csv(cooccurrence, "cooccurrence.csv") # Save the similarity matrix

# Construct the similarity matrix using the concept of Lift
# Calculate Lift using self-defined function
lift <- computeLift(data, top_20_brands)
#lift_app <- computeLift(data_appliance, top_20_appliance)
lift_10 <- computeLift(data, top_10_brands)
#write_csv(lift, "lift.csv") # Save the similarity matrix


#*********************************************************************************#
#STEP 4: CONVERT SIMILARITY MATRIX TO DISSIMILARITY MATRIX
dissLift <- convertSim(lift)
dissLift_10 <- convertSim(lift_10)
discooc <- convertSim(cooccurrence)
#diss_app <- convertSim(lift_app)

#*********************************************************************************#
#STEP 5: BUILD PERCEPTUAL MAP

# get the MDS plot using the self-defined function
mds <- mds_plot(dissLift)
mds$mds_plot

mds_cooc <- mds_plot(discooc)
mds_cooc$mds_plot

mds_10 <- mds_plot(dissLift_10)
mds_10$mds_plot



#*********************************************************************************#
# FUNCTION DEFINITIONS

# Define function "computeCooccurrence"
computeCooccurrence <- function(data, brands){
    
  # Initialize a matrix to store the cooccurrence results
  coocc <- data.frame(matrix(NA, nrow=length(brands), ncol=length(brands)))
  rownames(coocc) <- brands
  colnames(coocc) <- brands
  
  # Initialize an array to store the co-occurrences
  arr_coocc <- c()
  
  for (i in seq_along(brands)) {
    
    # In the outer loop, get the occurrences of brand i.
    b1 <- brands[i]
    # Detect the occurrences of b1 in review text
    flag_1_rev <- str_detect(data$reviewText, b1)
    # Detect whether the associated brand of the review is b1
	# We assume any nrand i reviews is a metion of the brand, even the brand name is not explicitly mentioned in review text.
    flag_1_brand <- str_detect(data$brand, b1)
    
    # However, the str_detect function will return NA if the string is empty
    # Set NAs in the `flag`, a boolean array, to False
    flag_1_rev[is.na(flag_1_rev)] <- F
    flag_1_brand[is.na(flag_1_brand)] <- F
    
    # Combine the two flags with 'OR' condition
	# As such, flag_1 flags all reviews that "mentions" brand i.
    flag_1 <- flag_1_rev | flag_1_brand
    
    for (j in seq_along(brands)) {
      # In the inner loop, first get the occurrences of brand j.
	  # Then get the co-occurrences of brand i and brand j, and append it to the end of arr_coocc
	  
	  # First, get the occurrences of brand j
      b2 <- brands[j]
      # Detect the occurrences of b2 in review text
      flag_2_rev <- str_detect(data$reviewText, b2)
      # Detect whether the review is under the product of b2
      flag_2_brand <- str_detect(data$brand, b2)
      
      # However, the str_detect function will return NA if the string is empty
      # Replace NA with FALSE
      flag_2_rev[is.na(flag_2_rev)] <- F
      flag_2_brand[is.na(flag_2_brand)] <- F
      
      # Combine the flags of b2 with 'OR' condition
	  # As such, flag_2 flags all reviews that "mentions" brand j.
      flag_2 <- flag_2_rev | flag_2_brand

      # get the index of co-occurrences of b1 and b2 with "AND" condition
      arr_coocc <- c(arr_coocc, sum(flag_2 & flag_1))
      
    }
  }

  # Map individual elements in an array to cells in a matrix
  # Loop over each cell of `coocc` table by rows and columns 
  # Iterate over rows, indexed by i
  for (i in 1:nrow(coocc)){
    # Loop over columns, indexed by j
    for (j in 1:ncol(coocc)) {
      if (i == j) {
        next
      } else {
        coocc[i, j] = arr_coocc[(i-1)*nrow(coocc)+j]
      }     
    }
  }
  
  return(coocc)
}


# Define function "computeLift"
computeLift <- function(data, brands){
    
  # Initialize a matrix to store the cooccurrence results
  lift <- data.frame(matrix(NA, nrow=length(brands), ncol=length(brands)))
  rownames(lift) <- brands
  colnames(lift) <- brands
  
  # Initialize an array to store the occurrences, with the i th element being the occurrences of brand i
  arr_occ <- c()
  
  # Initialize an array to store the co-occurrences, with the i*j th element being the co-occurrences of brand i and brand j
  arr_coocc <- c()
  
  for (i in seq_along(brands)) {
    
    # In the outer loop, get the occurrences of brand i, and append it to the end of arr_occ
    b1 <- brands[i]
    # Detect the occurrences of b1 in review text
    flag_1_rev <- str_detect(data$reviewText, b1)
    # Detect whether the associated brand of the review is b1
	# We assume any nrand i reviews is a metion of the brand, even the brand name is not explicitly mentioned in review text.
    flag_1_brand <- str_detect(data$brand, b1)
    
    # However, the str_detect function will return NA if the string is empty
    # Set NAs in the `flag`, a boolean array, to False
    flag_1_rev[is.na(flag_1_rev)] <- F
    flag_1_brand[is.na(flag_1_brand)] <- F
    
    # Combine the two flags with 'OR' condition
	# As such, flag_1 flags all reviews that "mentions" brand i.
    flag_1 <- flag_1_rev | flag_1_brand
      
    arr_occ <- c(arr_occ, sum(flag_1))
    	
    for (j in seq_along(brands)){
      # In the inner loop, first get the occurrences of brand j.
	  # Then get the co-occurrences of brand i and brand j, and append it to the end of arr_coocc
	  
	  # First, get the occurrences of brand j
      b2 <- brands[j]
      # Detect the occurrences of b2 in review text
      flag_2_rev <- str_detect(data$reviewText, b2)
      # Detect whether the review is under the product of b2
      flag_2_brand <- str_detect(data$brand, b2)
      
      # However, the str_detect function will return NA if the string is empty
      # Replace NA with FALSE
      flag_2_rev[is.na(flag_2_rev)] <- F
      flag_2_brand[is.na(flag_2_brand)] <- F
      
      # Combine the flags of b2 with 'OR' condition
	  # As such, flag_2 flags all reviews that "mentions" brand j.
      flag_2 <- flag_2_rev | flag_2_brand
      
      # get the index of co-occurrences of with "AND" condition
      arr_coocc <- c(arr_coocc, sum(flag_2 & flag_1))
      
    }
  }
  
  # calculate lift #  
  # Lift is a metric defined as the ratio of the actual co-occurrence of two terms to the frequency with which we would expect to see them together.
  # The lift between terms A and B can be calculated as:
  # Lift(A,B)= (P(A,B))/(P(A)×P(B))
  # where P(X) is the probability of occurrence of term X in a given review, and P(X, Y) is the probability that both X and Y appear in a given review
  
  # loop over each cell of `lift` table by rows and columns 
  # iterate over rows, indexed by i
  for (i in 1:nrow(lift)){
    # loop over columns, indexed by j
    for (j in 1:ncol(lift)) {
      # IF:
      #  (1) the cell indexed is on the diagonal line, OR
      #  (2) P(A) is zero, OR
      #  (3) P(B) is zero
      # THEN skip the loop because the denominator will be zero
      if (i == j | arr_occ[i] == 0 | arr_occ[j] == 0) {
        next
      # Calculate lift with P(A, B)/(P(A)*P(B))
      } else {
		PAB <- arr_coocc[(i-1)*nrow(coocc)+j]/nrow(arr_coocc)
		PA <- arr_occ[i]/nrow(arr_occ)
		PB <- arr_occ[j]/nrow(arr_occ)
        lift[i, j] = PAB / (PA * PB)
      }
      
    }
  }
  return(lift)
}


# Define function "convertSim"
convertSim <- function(SimilarityMatrix) {
		
	# Initialize the dissimilarity matrix
	DissMat <- data.frame(matrix(NA, nrow=nrow(SimilarityMatrix), ncol=ncol(SimilarityMatrix)))

	# Use a nested loop to iterate over the similarity matrix to change extremely small values to our lower bound
	# we believe that the 0.00001 is small enough and the 9999999 is large enough to represent the ultimate dissimilarity
	for (i in 1:nrow(SimilarityMatrix)) {
		for (j in 1:ncol(SimilarityMatrix)) {
			if(i == j){
			# Assign zeros to the diagonal line--the dissimilarity of the pair of same brand is zero
			DissMat[i, j] <- 0
			next
			} else if (is.na(SimilarityMatrix[i, j])){
				# if the cell is NA, the pair of brands has never been mentioned together, so very dissimilar to each other.
				# For this case, assign lower bound to SimilarityMatrix and 9999999 to DissMat
				DissMat[i, j] <- 9999999
				next
			} else if (abs(SimilarityMatrix[i, j]) <= 0.00001) {
				# if the cell is smaller than the lower bound, assign lower bound to it and 9999999 to DissMat
				DissMat[i, j] <- 9999999
				next
			}
		}
	}

	# Calculate dissimilarity of other cells
	# There are a number of approaches to convert similarity to dissimilarity.
	# In the following, the dissimilarity is calculated using reciprocal
	for (i in 1:nrow(SimilarityMatrix)) {
	  for (j in 1:ncol(SimilarityMatrix)) {
		if (is.na(DissMat[i, j])) {
		  DissMat[i, j] <- 1/SimilarityMatrix[i, j]
		}
	  }
	}

	# assign indexes to the dissimilarity matrix
	rownames(DissMat) <- rownames(SimilarityMatrix)
	colnames(DissMat) <- colnames(SimilarityMatrix)

	return(DissMat)
}
	

# Define function "mds_plot"
mds_plot <- function(DissimilarityMatrix, title = "MDS Perceptual Map Based on Amazon.com Review Data", 
                     c1 = "Coordinate 1", c2 = "Coordinate 2") {
					 
	# get the 2-dimensional MDS scale
	DissMatrix.mds <- cmdscale(DissimilarityMatrix, eig=TRUE, k=2)
	
	# DissMatrix.mds is a list
	result <- data.frame(DissMatrix.mds$points)
	colnames(result) = c("Coordinate1", "Coordinate2")
	
	# Plot solution
	p <- ggplot(data = result, aes(x= Coordinate1, y = Coordinate2)) +
	  geom_text(label = rownames(result)) +
	  ggtitle(title) +
	  labs(x = c1, y = c2)

	return(list("mds_plot" = p, "DissLift" = DissimilarityMatrix))
}
