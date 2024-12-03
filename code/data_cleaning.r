### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### DATA IMPORT AND CLEANING ## ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 1. Read in all the components of the spreadsheet and bind them together #####
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# set the filename
filename <- "data/Bird Count 2019-23.xls"

# find the number of tabs
n_tabs <- length(excel_sheets(filename))

# extract the tab names (years)
tab_names <- excel_sheets(filename)

# initialize an empty list to store the tabs
tab_list <- list()

# Loop to create multiple data frames and store them in the list
for (i in 1:n_tabs) {

  # assign the year for the data from the sheet tab name
  tab_year <- tab_names[i]
  
  # upload the whole tab for year xxxx and remove any entirely NA rows
  xls_tab <- 
    read_xls(filename, sheet = tab_year) %>% 
    filter(if_any(everything(), ~ !is.na(.)))
  
  # extract the visit title and visit date information
  visit_titles <- c(as.character(xls_tab[2,])) %>% .[!is.na(.)]
  visit_dates <- c(as.character(xls_tab[3,])) %>% .[!is.na(.)]
  visit_title_dates <- data.frame(visit_dates, visit_titles)
  
  # extract the species counts and add the titles back in as column headers
  tab_counts <- xls_tab[2:nrow(xls_tab),] %>% filter(!is.na(.[[1]]))
  colnames(tab_counts) <- c("species", visit_titles)
  
  # pivot to long format
  tab_counts_long <- tab_counts %>%
    pivot_longer(!species, names_to = "visit_title", values_to = "count")
  
  # add back in the visit date and the year from the tab name
  tab_counts_long <- 
    tab_counts_long %>% 
    left_join(visit_title_dates, by = c("visit_title" = "visit_titles")) %>%
    rename(date = visit_dates) %>%
    mutate(year = as.integer(tab_year))
  
  # add the data frame to the list
  tab_list[[i]] <- tab_counts_long
  
  # rename the list element with the year
  names(tab_list)[i] <- tab_year
}

# bind all data frames in the list into one dataframe
combined_data <- bind_rows(tab_list)

# remove all intermediate objects
rm(list=setdiff(ls(), "combined_data"))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2. Cleaning steps for the combined data  ####################################
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# 2.1 clean the count column ####
combined_data <- 
  combined_data %>% 
  mutate(count = str_replace(count, "ü", "-1")) %>% # replace the "ü" with -1 (presence)
  mutate(count = str_replace(count, "[^0-9\\-]", "")) %>% # remove any other non-numerics
  mutate(count = as.integer(count)) %>% # convert to integer (check for NA coercion warning)
  filter(!is.na(count)) # remove NA rows (zero counts)


# 2.2 format the dates consistently ####

# isolate and format the decimal dates
dec_dates <- 
  combined_data %>%
  filter(str_detect(date, "\\.")) %>%
  mutate(date = dmy(date))

# isolate and format the numerical dates
num_dates <- 
  combined_data %>%
  filter(!str_detect(date, "\\.")) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))  

# rebind back into the combined data  
combined_data <- 
  bind_rows(dec_dates, num_dates)

# extract the month to a separate column
combined_data <-
  combined_data %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE))

# remove the intermediate date tibbles
rm(list = ls(pattern = "_dates"))


# 2.3 Cleaning the species list ####

combined_data <-
  combined_data %>%
  filter(species != "no of species") %>% # removing the "no of species" rows
  mutate(species = str_replace_all(species, "\\s+$", "")) # remove any trailing whitespaces


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 3. Establishing standardised site names  ####################################
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Fuzzy string matching: https://www.youtube.com/watch?v=txHkNr29w20

# 3.1 Clean the visit_title column to make fuzzy matching more consistent
combined_data <-
  combined_data %>%
  mutate(clean_visit_title = visit_title) %>%
  mutate(clean_visit_title = str_replace_all(clean_visit_title, "\\s*/\\s*", "/")) %>% # remove whitespace either side of the "/"
  mutate(clean_visit_title = str_replace_all(clean_visit_title, "\\([^()]*\\)", "")) %>% # remove any text in parentheses (xxx)
  mutate(clean_visit_title = str_replace_all(clean_visit_title, "[[:punct:]&&[^/]]", "")) %>% # remove all puncuation except for the "/"
  mutate(clean_visit_title = str_replace_all(clean_visit_title, "\\s+", " ")) %>% # replace repeated whitespace with one instance
  mutate(clean_visit_title = str_replace_all(clean_visit_title, "\\s+$", "")) # remove any trailing whitespaces

# 3.2 Apply the string matching algorithm ####

# extract the unique cleaned names
clean_visit_titles <- 
  combined_data %>%
  pull(clean_visit_title) %>% 
  unique() %>%
  sort()

# choose the standard site names file
std_names_file <- "data/standard_site_names.csv"

# import the standard site names csv file and extract the only column as a character vector
std_names <- read_csv(std_names_file, col_names = FALSE, show_col_types = FALSE)$X1

# use amatch to return a dataframe of Jaro-Winkler matches
matched_names <- 
  data.frame(clean_visit_title = clean_visit_titles, 
             site_name = std_names[amatch(clean_visit_titles, std_names,
                                         method = "lcs", maxDist = 30)]) 

# 3.3 Manual corrections for problem sites
# apply a manual correction to Connaught Water
# (temporary patch pending more refined fuzzy string matching)
matched_names <-
  matched_names %>%
  mutate(site_name = if_else(grepl("Connaught", clean_visit_title),
                             "Chingford Plains/Connaught Water", site_name))


# 3.3 USER CHECK ####

# arrange the data frame for easy reading
matched_names <- 
  matched_names %>%
  select(site_name, clean_visit_title) %>%
  arrange(site_name)

# Convert data frame to string
df_string <- capture.output(print(matched_names))

# Text string to be printed
text_string <- "\nPlease check the data above. Do all of the visit titles match the appropriate site names?"

# Combine text string and data frame string
cat(paste(c(df_string, text_string), collapse = "\n"))


# 3.4 Add the standardised site names to the combined data ####
combined_data <-
  combined_data %>%
  left_join(matched_names, by = "clean_visit_title") %>% # join in the standardised names
  select(-clean_visit_title) # remove the intermediate clean names column


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 4. Establishing and saving the final data set ###############################
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# arrange the data by date and re-order the columns
final_data <-
  combined_data %>%
  arrange(date) %>%
  select(date, year, month, site_name, count, species, visit_title, everything())

# review the final data
final_data

# remove everything except the final, clean data set
rm(list=setdiff(ls(), "final_data"))



