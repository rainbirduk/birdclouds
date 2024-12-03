# run the cleaning scrip so far
source("data_cleaning.r")

# extract the unique resulting cleaned names
reduced_names <- 
  clean_names_df %>%
  select(site_cleaner) %>%
  arrange() %>% 
  unique()

# view the output
print(reduced_names, n = nrow(reduced_names))

# stringdist: https://www.youtube.com/watch?v=Tq-B95qbVXg
# fuzzy string matching: https://www.youtube.com/watch?v=RibBUG9Zuvs
