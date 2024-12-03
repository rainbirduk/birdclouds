### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Set up the environment ######################################################
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# remove objects in the environment
rm(list = ls()) 

# install/load the required packages
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, readxl, lubridate, stringdist,
               wordcloud, wordcloud2, imgpalr) 
