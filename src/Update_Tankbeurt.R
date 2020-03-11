library(tidyverse)
library(googlesheets)
library(googlesheets4)

Data <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", sheet = "Mazout", range = "V10:V11")
avg <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", sheet = "Mazout", range = "V2:V3")


current_level <- Data$...1[1]
current_date <- strptime(as.character(Sys.Date()), "%Y-%m-%d", tz = "UTC")
current_date <- current_date[[1]]

avg <- nth(avg, 1)

i <- 1
log <- data.frame(1:10)

colnames(log) <- "datum"

repeat{
  Verbruik_sim <- read_sheet("1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI", sheet = "Pivot Table 2", range = "J:P", col_types = "Tnnnnnn")
  
  #Verbruik_sim <- Verbruik_sim %>% 
   # mutate(Maand = format(Dag, "%m"),
    #       Dag2 = format(Dag, "%d"),
     #      Datum = as.Date(paste0(format(Sys.Date(), "%Y"), "-", Maand, "-", Dag2))) 
  
  #Verbruik_sim$Datum <- strptime(as.character(Verbruik_sim$Datum), "%Y-%m-%d", tz = "UTC")
  
  #summary(Verbruik_sim$Datum)
  
  new_level <- current_level
  new_date <- current_date + 24*3600
 
  threshold <- 100
  
  while(new_level > threshold){
    jaar <- format(new_date, "%Y")
    Verbruik_sim <- Verbruik_sim %>% 
      mutate(Maand = format(Dag, "%m"),
             Dag2 = format(Dag, "%d"),
             Datum = as.Date(paste0(jaar, "-", Maand, "-", Dag2))) 
    Verbruik_sim$Datum <- strptime(as.character(Verbruik_sim$Datum), "%Y-%m-%d", tz = "UTC")
    verbruik <- Verbruik_sim$Verbruik[Verbruik_sim$Datum == new_date]
    verbruik <- verbruik[1]
    
    if(is.na(verbruik)){
      verbruik <- avg
    }
    
    print(verbruik)
    new_level <- new_level - verbruik
    new_date <- new_date + 24*3600
    print(new_level)
    print(new_date)
  }
  
  log$datum[i] <- new_date[[1]]
  
  
  new_date <- as.data.frame(new_date)
  #Trigger randomisation in gsheets
  sheets_write(new_date, sheet = "Update_Mazout", ss = "1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI")
  i  <- i + 1
  
  if (i == 11){
    print("repeat loop ends");
    break
  }
}

log$datum2 <- as.Date.POSIXct(log$datum, tz = "UTC", origin = "1900-01-01")
#Average of 10 iterations
log2 <- log %>% 
  summarise(mean = mean(datum2))
sheets_write(log2, sheet = "Update_Mazout", ss = "1YLYWYwPsXXAeTEFz1Mpi2tV6J13sXUveIIKY8HBDncI")

