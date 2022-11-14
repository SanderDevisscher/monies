update_level <- function(){
  
  
  library(googlesheets4)
  library(tidyverse)
  
  bo_email <- Sys.getenv("bo_email")
  gs4_auth(email = bo_email)
  
  Data <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", 
                     sheet = "Mazout", 
                     range = "C7:E1000")
  
  today <- Sys.Date()
  
  Data_subset <- Data %>% 
    filter(!is.na(Over)) %>% 
    mutate(dagen_geleden = difftime(Datum, today, "days"))
  
  Update_value <- Data_subset %>% 
    filter(dagen_geleden == max(dagen_geleden)) %>% 
    select(Over)
  
  sheet_write(Update_value, sheet = "Update_Level", ss = "1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI")
  
}