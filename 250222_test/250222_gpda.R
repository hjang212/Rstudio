getwd()
setwd('C:/Users/hjjan/OneDrive/Documents/Projects/R/250222_gpa2')
install.packages("tidyverse")
library(tidyverse)



---




---
# Changing your data
# https://d3c33hcgiwev3.cloudfront.net/osHCTXtSSd2Bwk17Ukndiw_4c290cdedaab4ba0ae55358416b13ef1_Lesson3_Change.Rmd?Expires=1740873600&Signature=GCnTusJHgIQz3MSAbewp3dawjZ65C15izMQITgtMPtM1w8GasTq6VfwXaWNF0yQ5XzRbyEAVf1qLxngIKQC~GyAnM~mSN~5Q6NT7oNYH7fDop5CGxafAeae7ZC0UtipOnLGKXVLmm8FbEjP6yFHB4FsTVjbF5o-x7ebRN3fULj8_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A
 
# Step 1: Load packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
library(tidyverse)
library(skimr)
library(janitor)

# Step 2: Import data
x <- read_csv('./250222_test/hotel_bookings.csv')

# Step 3-1: Getting to know your data
# summary functions to get to know your data
str(x)
head(x)
glimpse(x)
skim_without_charts(x)
colnames(x)

# Quiz
# 32 colums
# arrival_date_month is character type data

# Step 3-2: Manipulating your data
y <- x %>% 
  arrange(-lead_time) # sorts desc order, stays in console. largest = 737
head(y)  
# You can also find out the max and min lead times without sorting using arrange() function. 
# max() and min() functions:
max(y$lead_time)
min(y$lead_time)
# Remember to specify which dataset and which column using the $ symbol between their names. Try running:
min(lead_time)
mean(x$lead_time)
## Practice quiz: What is the average lead time? -> 104.0114(D)

# You were able to report to your boss what the average lead time before booking is, but now they want to know what the average lead time before booking is for just city hotels. They want to focus the promotion they are running by targeting major cities.

xy <- filter(x, x$hotel=='City Hotel')
glimpse(x)
xy <- filter(x, x$hotel=='City Hotel')
head(xy)
mean(xy$lead_time)

# Your boss wants to know a lot more information about city hotels, including the maximum and minimum lead time. They are also interested in how they are different from resort hotels
# You don't want to run each line of code over and over again, so you decide to use the groupby and summarize functions.
# You can also use the pipe operator to make code easier to follow. You will store the new dataset in a data frame named 'hotel_summary':
hotel_summary <-
  x %>%
  group_by(hotel) %>%
  summarise(average_lead_time=mean(lead_time),
            min_lead_time=min(lead_time),
            max_lead_time=max(lead_time)
            )
head(hotel_summary)


## Activity Wrap up
# Being able to manipulate data is a key skill for working in R. After this activity, you should be more familiar with functions that allow you do change your data
# such as arrange(), group_by(), and filter(). You also have some experience using statistical summaries to make insights into your data. You can continue
# to practice these skills by modifying the code chunks in the rmd file, or use this code as a starting point in your own project console. As you practice,
# consider how performing tasks is similar and different in R compared to other tools you have learned throughout this program.





---
# Transform data

# pivot_longer() 
# pivot_wider()

# Resources: 
#  https://scc.ms.unimelb.edu.au/resources-list/simple-r-scripts-for-analysis/r-scripts, plotting
#  https://tidyr.tidyverse.org/articles/pivot.html, pivoting
#  https://rladiessydney.org/courses/ryouwithme/02-cleanitup-5/ , cleanitup wide to long to wide to pivot
  

---
# Cleaning data
library(tidyverse)
library(skimr)
library(janitor)

# Import data
bookings_df = read_csv('./250222_test/hotel_bookings.csv')

# Know your data
head(bookings_df)
glimpse(bookings_df)
str(bookings_df)
colnames(bookings_df)
skim_without_charts(bookings_df) # skimr package function

# Clean your data
trimmed_df <- bookings_df %>%
  select('hotel', 'is_canceled', 'lead_time') 
glimpse(trimmed_df)

View(trimmed_df)

trimmed_df <- bookings_df %>%
  select(hotel, is_canceled, lead_time) %>%
  rename(hotel_type = hotel)

# Another common taks is to either split or combine data in different columns. You can combine the arrival month and year into one column using the unite function
example_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>%
  unite(arrival_month_year, c(arrival_date_month, arrival_date_year), sep = ' ')


# Another way of doing things
example_df <- bookings_df %>%
  mutate(guests = adults + children + babies)
?mutate
head(example_df)
glimpse(bookings_df)


# Summarize
example_df <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled),
              avg_lead_time = mean(lead_time)) 
head(example_df)


---
# Organize your data
library(tidyverse)
# sort by arrange and group_by
penguins %>% arrange(bill_length_mm) # sorts in ascending order by default, stays in console
penguins %>% arrange(-bill_length_mm) # sorts in descending order by default, stays in console
penguins2 <- penguins %>% arrange(-bill_length_mm) # saves the data frame
penguins %>% group_by(island) %>% drop_na() %>% summarize(mean_bill_length_mm = mean(bill_length_mm)) # Can also sort by data using group_by function.
penguins %>% group_by(island) %>% drop_na() %>% summarize(max_bill_length_mm = max(bill_length_mm)) 
penguins %>% group_by(species, island) %>% drop_na() %>% summarize(max_b1 = max(bill_length_mm), mean_b1 = mean(bill_length_mm))

# filter results with filter()
penguins %>% filter(species == 'Adelie')


---
# Cleaning up with the basics
rm(bookings_df)
rm(new_df)
install.packages("here")
install.packages("skimr")
install.packages("janitor") #clean names function in janitor package will automatically make sure that the column names are unique and consistent
library(here)
library(skimr)
library(janitor) 
library(dplyr)
install.packages("palmerpenguins")
library(palmerpenguins)

# cleaning functions
skim_without_charts(penguins)

penguins %>% # pipe operator transforms the data
  select(-species)

penguins %>% 
  rename(island_new=island) # change the column name to island_new

rename_with(penguins, toupper) # automatically make our column names uppercase
rename_with(penguins, tolower) # automatically make our column names lowercase

clean_names(penguins) # this makes sure that there are characters, numbers, and underscores in the name



---
# Importing and working with data
# 2025-02-23
install.packages("tidyverse")
library(tidyverse)
bookings_df <- read_csv('./250222_test/hotel_bookings.csv')
glimpse(bookings_df)
View(bookings_df)
head(bookings_df)
str(bookings_df)
colnames(bookings_df)
new_df <- select(bookings_df, 'adr', 'adults')
glimpse(new_df)

  
---
# Tibble, 250223
# https://rstudio-education.github.io/tidyverse-cookbook/tidy.html#
# https://tibble.tidyverse.org/
  
install.packages("tidyverse")
library(tidyverse)
rm(rank)
data(diamonds)
diamonds
glimpse(diamonds)
View(diamonds)
diamonds_tibble <- as_tibble(diamonds) # Create a tibble
glimpse(diamonds_tibble)
diamonds_tibble




---
# from https://d3c33hcgiwev3.cloudfront.net/RSYCoU88Q66mAqFPPNOuKQ_4d3ab0b8fa0547f6996a66ab05babcf1_Lesson2_Dataframe.Rmd?Expires=1740441600&Signature=PlK15-BnFVmmJDVBqTFbgmm0ljTxJVuK2~mwajKFn-aw0qUD5BepptX-GH6imY1khnKqmbVmb07r0RgkTVNmYlZi13CnUlOJPZBAthGi90pwNU-qnISCrTfzhJ4n-dBKfH19XDoRK2BW2W2zdmKZFcfVVZlVYrKAVd84U1D18D0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A

# Step 1: Load packages
install.packages(tidyverse)
library(tidyverse)  

# Step 2: Create Data Frame 
names <- c('Hyunwuk', 'Hyunjun', 'Hanjun', 'Sungwon')
age <- c(42L, 37L, 37L, 47L) # integer
age <- c(42, 37, 37, 47) # make sure you are inputting numeric value (num) = int(L) + double(0.00)
str(age)

people <- data.frame(names, age)


# Inspect the data frame
head(people)
str(people)
glimpse(people)
colnames(people)

mutate(people,age_in_20 = age + 20)


fruit <- c('Orange', 'Lemon', 'Apple', 'Grape', 'Strawberry')
rank <- c(2,5,4,3,1)
fruit_ranks <- data.frame(fruit, rank)
glimpse(fruit_ranks)
rm(favorite_fruit)

---
# Google Professional Data Analyst
mdy("January 20th, 2023")
library(tidyverse)
library(lubridate)

# Types 
# In R, there are three types of data that refer to an instant in time:
# - A date ("2016-08-16")
# - A time within a day ("20:11:59 UTC")
# - And a date-time. This is a date plus a time ("2018-03-31 18:15:48 UTC")

today() #date
now() #date-time

ymd("2023-01-20")
# code output:
# [1] "2023-01-20"

mdy ("January 20th, 2023")
# [1] "2023-01-20"

dmy("20-Jan-2021")
# code output:
# [1] "2021-01-20"

ymd(20210120)
# code output:
# [1] "2021-01-20"

ymd_hms("2021-01-20 20:11:59")
# code output:
# [1] "2021-01-20 20:11:59 UTC"
mdy_hm("01/20/2021 08:01")
# code output:
# [1] "2021-01-20 08:01:00 UTC"

as_date(now())
# code output:
# [1] "2021-01-20"



# Vectors and lists
c(2.5, 48.5, 101.5)
c(1L, 5L, 15L) # L is followed by number 
c("Sara" , "Lisa" , "Anna")
c(TRUE, FALSE, TRUE)
z <- c(4:10)
z

# Determine vector properties
typeof(c("a" , "b")) #character
typeof(c(1L , 3L)) #integer
x <- c(2L, 5L, 11L) 
is.integer(x)

y <- c(TRUE, TRUE, FALSE)
is.character(y)

# Vector length
x <- c(33.5, 57.75, 120.05)
length(x)

# Name vectors
x <- c(1, 3, 5)
names(x) <- c("a", "b", "c")
x

# Extract a subset of a vector
x <- c(1, 3, 5)
names(x) <- c("a", "b", "c")
x
x[2]

# Alternatively, vector element 2 is named b, so you can call it with 
# the code x["b"]:
x <- c(1, 3, 5)
names(x) <- c("a", "b", "c")
x
x["b"]

# Lists
list("a", 1L, 1.5, TRUE)
list(list(list(1 , 3, 5)))
str(list("a", 1L, 1.5, TRUE))


z <- list(list(list(1 , 3, 5)))
str(z)

list('Chicago' = 1, 'New York' = 2, 'Los Angeles' = 3)

# Matrices
matrix(c(3:8), nrow = 2)
matrix(c(3:8), ncol = 2)

# Operators and calculations
quarter_1_sales <- 35657.98 
quarter_2_sales <- 43810.55 
midyear_sales <- quarter_1_sales + quarter_2_sales



---
# from https://ourcodingclub.github.io/tutorials/intro-to-r/
  
install.packages("dplyr")
library('dplyr')
getwd()
setwd('C:/Users/hjjan/OneDrive/Documents/Projects/R/250222_gpa')

edidiv <- read.csv('./edidiv.csv')
head(edidiv) # Displays the first few rows
tail(edidiv) # Displays the last rows

str(edidiv) # Displays structures

head(edidiv$taxonGroup) # Displays the first few rows of this column only
class(edidiv$taxonGroup) # Tells you what type of variable we're dealing with: it's character now but we want it to be a factor

edidiv$taxonGroup <- as.factor(edidiv$taxonGroup) # Transforms $taxonGroup rows as factor or category

dim(edidiv) # Displays number of rows and columns
summary(edidiv) # Gives you a summary of the data
unique(edidiv$taxonGroup) # Gives you a list of unique values of $taxonGroup
unique(edidiv$taxonName)


# Calculate species richness
# Create a graph showing how many species were recorded in each taxonomic group

Beetle <- filter(edidiv, taxonGroup == 'Beetle') # The first argument of the function is the data frame, the second argument is the condition you want to filter on. Because we only want the beetles here, we say: the variable taxonGroup MUST BE EXACTLY (==) Bettle - drop everything else from the dataset. (R is case-sensitive so its important to watch your spelling! "bettle" or "Beetles" would not have worked here.)
summary(Beetle)
Bird <- filter(edidiv, taxonGroup == 'Bird')
Butterfly <- filter(edidiv, taxonGroup == 'Butterfly')
Dragonfly <- filter(edidiv, taxonGroup == 'Dragonfly')
Flowering.Plants <- filter(edidiv, taxonGroup == 'Flowering.Plants')
Fungus <- filter(edidiv, taxonGroup == 'Fungus')
Hymenopteran <- filter(edidiv, taxonGroup == 'Hymenopteran')
Lichen <- filter(edidiv, taxonGroup == 'Lichen')
Liverwort <- filter(edidiv, taxonGroup == 'Liverwort')
Mollusc <- filter(edidiv, taxonGroup == 'Mollusc')
Mammal <- filter(edidiv, taxonGroup == 'Mammal')


rm(Bettle) # Delete a df
gc() # Perform garbage collection


# the number of different species in each group.Similar to count
a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mollusc$taxonName))
k <- length(unique(Mammal$taxonName))



# Now that we have species richness for each taxon, we can combine all those values in a vector. A vector is another type of R object that stores values. As opposed to a data frame, which has two dimensions (rows and columns), a vector only has one. When you call a column of data frame like we did earlier, you are essentially producing a vector - but you can also crreate them from scrach.
# We do this using the c() function (c stands for concatenate, or chain if that makes it easier to remember). We can also add labels with the names() function, so that the values are not coming out of the blue.

biodiv <- c(a,b,c,d,e,f,g,h,i,j,k) # We are concatenating (chaining) together all the values; pay attention to the object names you have calculated and their order
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

# We can now visualize species richness with barplot() function. Plots appear in the bottom right window in RStudio.
barplot(biodiv)

# There are no axis titles, not all column labels are visible, and the value for plant species (n=521) exceeds the highest value on the y axis, so we need to extend it. The great thing about R is that you don't need to come up with all the code on your own - you can use the help() function and see what arguments you need to add in. Look through the helpoutput, what code do you need to add in?
help("barplot")
help(par)

png("barplot.png", width=1600, height=600)  # look up the help for this function: you can customize the size and resolution of the image
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.0, cex.axis=1.5, cex.lab=1.5)
dev.off()
# The cex code increases the font size when greater than one (and decreases it when less than one). 



# In the last section we created vectors, i.e. a series of values, each with a label. This object type is suitable 
# when dealing with just one set of values. Often, however, you will have more than one variable and have multiple 
# data types - e.g. some continuous, some categorical. In those cases, we use data frame objects. Data frames are 
# tables of values: they have a two-dimensional structure with rows and columns, where each column can have a 
# different data type. For instance, a column called “Wingspan” would have numeric values measured on different 
# birds (21.3, 182.1, 25.1, 8.9), and a column “Species” would have character values of with the names of the species
# (“House sparrow”, “Golden eagle”, “Eurasian kingfisher”, “Ruby-throated hummingbird”) Another possible data format is
# a matrix - a matrix can have several rows of data as well (e.g. you can combine vectors into a matrix), but the 
# variables must be all of the same type. For instance they are all numerical and are the same length in terms of the 
# number of rows.


# A note on good housekeeping: ALWAYS keep a copy of your raw data as you first collected it. The beauty of 
# manipulating a file in an R script is that the modifications live on the script, not in the data. For 
# Photoshop-savvy people, it’s like adding layers to an image: you’re not altering the original photo, just 
# creating new things on top of it. That said, if you wrote a long piece of code to tidy up a large dataset 
# and get it ready to analyse, you may not want to re-run the whole script every time you need to access the clean 
# data. It’s therefore a good idea to save your shiny new object as a new csv file that you can load, ready-to-go, 
# with just one command. We will now create a data frame with our species richness data, and then save it using 
# write.csv().

# We will use the data.frame() function, but first we will create an object that contains the names of all the taxa 
# (one column) and another object with all the values for the species richness of each taxon (another column).

# Creating an object called "taxa" that contains all the taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
# Turning this object into a factor, i.e. a categorical variable
taxa_f <- factor(taxa) # Categorize taxa

taxa
taxa_f

# Combining all the values for the number of species in an object called richness
richness <- c(a,b,c,d,e,f,g,h,i,j,k)

richness

# Creating the data frame from the two vectors
biodata <- data.frame(taxa_f, richness)

biodata

# Saving the file
write.csv(biodata, file="biodata.csv")  # it will be saved in your working directory

# If we want to create and save a barplot using the data frame, we need to slightly change the code - because data 
# frames can contain multiple variables, we need to tell R exactly which one we want it to plot. Like before, 
# we can specify columns from a data frame using $:

png("barplot2.png", width=1600, height=600)
barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of species", ylim=c(0,600))
dev.off()
