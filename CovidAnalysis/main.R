# Install packages
library(httr)
library(rvest)
library(gtools)

get_wiki_covid19_page <- function() {
  #Covid 19 url param
  wiki_url_param_value <- "Template:COVID-19_testing_by_country"
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
  response <- GET(wiki_base_url, query=list(title=wiki_url_param_value))
  return (response)
}

extract_covid19_test_data_frame <- function(response) {
  html <- read_html(response)
  covid_table_html_node <- html_nodes(html, "table")[2]
  covid_html_table_data_frame <- as.data.frame(html_table(covid_table_html_node))
  return(covid_html_table_data_frame)
}

preprocess_covid_data_frame <- function(data_frame) {
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}

get_subset_data_task_4 <- function(dataframe){
  countrys_subset <- dataframe$country[5:10]
  confirmed_subset <- dataframe$confirmed[5:10]
  return(list(countrys_subset, confirmed_subset))
}

calculate_worldwide_data_task_5 <- function(dataframe){
  total_confirmed_cases <- 0
  total_tested_cases <- 0
  
  for(case in dataframe$confirmed){
    total_confirmed_cases <- total_confirmed_cases + case
  }
  
  for (test_case in dataframe['confirmed.tested.ratio']){
    c <- test_case
    total_tested_cases <- total_tested_cases + test_case
  }

 return(list(total_confirmed_cases, total_tested_cases))
}

get_a_country_list_which_reported_task_6 <- function(dataframe){
  countrys <- dataframe$country
  a_to_z_sort <- mixedsort(countrys)
  z_to_a_sort <- mixedsort(countrys, decreasing = TRUE)
  return(z_to_a_sort)
}

identity_countries_names_task_7 <- function(dataframe){
  data <- dataframe
  data %>% filter(!grepl("United.+", data))
  return (data)
}

pick_two_countries_task_8 <- function(dataframe){
  country <- dataframe$country
  confirmed <- dataframe$confirmed
  confirmed_population_ratio <- dataframe['confirmed.population.ratio']
  subset <- list(country, confirmed, confirmed_population_ratio)[1:10]
  return (subset)
}

compare_which_one_selected_countries_task_9 <- function(dataframe){
  confirmed_cases <- dataframe['confirmed.population.ratio']
  largest_ratio <- 0
  
  for(case in confirmed_cases){
    if(largest_ratio<case){
      larget_ratio <- case
    }
  }
  
  return (largest_ratio)
}

find_countries_task_10 <- function(dataframe){
  less_than_1_perc_list <- list()
  confirmed_cases <- dataframe['confirmed.population.ratio']
  
  for(case_ratio in confirmed_cases){
    if(case_ratio< 0.01){
      less_than_1_perc_list[[length(less_than_1_perc_list)+1]] = case
    }
  }
  
  return(less_than_1_perc_list)
}

# Get data from wiki site and export as csv file
response <- get_wiki_covid19_page()
data_frame <- extract_covid19_test_data_frame(response)
processed_data <- preprocess_covid_data_frame(data_frame)
write.csv(processed_data, "/Users/kailynwilliams/R-practice/CovidAnalysis/covid_data.csv")

#download csv
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="/Users/kailynwilliams/R-practice/CovidAnalysis/aws_covid_data.csv")
covid_data_frame_csv <- read.csv("aws_covid_data.csv", header=TRUE, sep=",")

#Task assignments
task4_data <- get_subset_data_task_4(covid_data_frame_csv)
task5_data <- calculate_worldwide_data_task_5(covid_data_frame_csv)
task6_data <- get_a_country_list_which_reported_task_6(covid_data_frame_csv)
task7_data <- identity_countries_names_task_7(covid_data_frame_csv)
task8_data <- pick_two_countries_task_8(covid_data_frame_csv)
task9_data <- compare_which_one_selected_countries_task_9(covid_data_frame_csv)
task10_data <- find_countries_task_10(covid_data_frame_csv)
