library(tidyverse)
library(googlesheets4)

bo_email <- Sys.getenv("bo_email")
gs4_auth(email = bo_email)

Annual_Use <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", sheet = "Pivot Table 3", range = "A1:B10")

current_year <- format(Sys.Date(), "%Y")

current_usage <- Annual_Use$`SUM of Verbruikt`[Annual_Use$Jaar == current_year]
today <- Sys.Date()
new_date <- today
final_date <- paste0(current_year, "-12-31")
usage_sim <- current_usage

Verbruik_sim <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", sheet = "Pivot Table 2", range = "J:P", col_types = "Tnnnnnn")

verbruik_simulatie <- data.frame(1,2)
verbruik_simulatie <- verbruik_simulatie %>% 
  mutate(X1 = as.character(X1))

colnames(verbruik_simulatie) <- c("datum", "usage_sim")

while(new_date <= final_date){
  new_date <- new_date + 1
  jaar <- format(new_date, "%Y")
  Verbruik_sim <- Verbruik_sim %>% 
    mutate(Maand = format(Dag, "%m"),
           Dag2 = format(Dag, "%d"),
           Datum = as.Date(paste0(jaar, "-", Maand, "-", Dag2))) 
  Verbruik_sim$Datum <- strptime(as.character(Verbruik_sim$Datum), "%Y-%m-%d", tz = "UTC")
  Verbruik_sim$Datum <- as.Date(paste0(jaar,"-",format(Verbruik_sim$Datum, "%m"),"-",format(Verbruik_sim$Datum, "%d")))
  verbruik <- Verbruik_sim$Verbruik[Verbruik_sim$Datum == new_date]
  verbruik <- verbruik[1]
  usage_sim <- usage_sim + verbruik
  verbruik_simulatie <- verbruik_simulatie %>% 
    filter(usage_sim != 2) 
  
  new_date2 <- as.character(new_date)
  
  verbruik_simulatie <- verbruik_simulatie %>% 
    add_row(datum = new_date2, usage_sim = usage_sim) 
}

sheet_name <- paste0("mazout_verbruik_sim_", current_year)

sheet_write(verbruik_simulatie, sheet = sheet_name, ss = "1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI")
