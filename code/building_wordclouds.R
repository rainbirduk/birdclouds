### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### BUILDING WORDCLOUDS ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Set-up ####################################################################

# run the environment set-up and data import steps
source("code/env_setup.R")
source("code/data_cleaning.r")

### Create the wordcloud inputs ###############################################

# set the filters
SITE <- "Fishers Green"
YEAR <- 2019

# create a summary dataset
wc_input <- final_data %>%  
  {if (exists("SITE")) filter(., site_name == SITE) else .} %>%
  {if (exists("YEAR")) filter(., year == YEAR) else .} %>%
  group_by(species) %>%
  summarise(count = sum(count)) %>%
  setNames(colnames(wordcloud2::demoFreq))

# remove filters
rm(list = c("SITE", "YEAR"))


### Experiment with colour palette tools... ###################################

### ### ... using imgpalr #######

pal <- image_pal("palette_references/kelp.jpg", 
                 n = 4, plot = T)


### Experiment with making wordclouds... ######################################

### ### ... using wordcloud #######

# produce the word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = wc_input$word, freq = wc_input$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, scale=c(2, .8),
          colors=brewer.pal(8, "Dark2"))


### ### ... using wordcloud2 ######

# produce wordcloud2 output
wordcloud2(data = wc_input)



