# Load libraries and set options
library(tidyverse)
library(pdftools)
library(dslabs)

options(digits = 3)

# Load in and open PDF of mortality data for Puerto Rico
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package = "dslabs")

system2("open", args = fn)

# Read in the PDF and store it in an object, then examine the object
txt <- pdf_text(fn)

class(txt)
length(txt)
head(txt)

# Extract page 9 of PDF and store each line in a different entry of a new object, examine new object
x <- txt[9] %>% str_split("\n")

head(x)
class(x)
length(x)

# Create a new object containing the first entry of object x, examine new object
s <- x[[1]]

class(s)
head(s)
length(s)

# Trim s and assign result back to s
s <- str_trim(s)

# Find the last character of element 1 of s
s[1]

# Store the index of the table header string in an object
header_index <- s %>% 
  str_which("^[A-Z]{3}\\s+2015")

# Extract the month and column names from the header row and store them in separate objects
header <- s[header_index]

header <- header %>%
  str_split("\\s+", simplify = TRUE)

month <- header[,1]

header <- header[,2:5]

# Store the index of the "Total" row at the end of the page
tail_index <- s %>% 
  str_which("^Total")
tail_index

# Create an object to store the count of numbers in each row
n <- s %>%
  str_count("\\d+")

# Find the number of rows with a single number in them
sum(n == 1)

# Remove unneeded entries from s
s <- s[(header_index+1):(tail_index-1)] # remove the header and all entries before, tail and all entries after

s <- s %>% # remove entries with only one number in them
  str_remove("^\\d+$") %>%
  str_subset(".+")

length(s) # calculate the number of entries now in s

# Remove all text that isn't a digit or a space
s <- s %>%
  str_remove_all("[^\\d\\s]")

# Convert s into a data matrix with just the days and death count data
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

# Add column names to the matrix and add a column with the month
column_names <- c("DAY", header)

tab <- s %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(column_names) %>%
  mutate_all(parse_number) %>%
  mutate(MONTH = month, .before = 1)

# Convert tab into tidy format
tab_tidy <- tab %>% 
  gather(year, deaths, -DAY, -MONTH) %>%
  mutate(deaths = as.numeric(deaths))
