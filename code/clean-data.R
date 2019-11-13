library(tidyverse)

#--> Set this name!!!
# Set your working directory to be in one of three folders (v1-..., v2-... etc.)
# Then you can run this code. IT takes the simulation ouputs over a range of PERC 
# values and composes them into one data frame
save_name <- "data-v1.csv"

# -------------------------------------------------------------------------


#--> Warmup period in simulation (preset)
cutoff <- 100 * 60 * 24 # hours * mins / hr (parameter from simulation)

#--> Iterate thru these in a moment
folder_names <- c("Prob_PERC_out_00", "Prob_PERC_out_25", "Prob_PERC_out_50", 
                 "Prob_PERC_out_75", "Prob_PERC_out_100")


#--> Funtion to munge an individual simulation run
cleanData <- function(path, folder_name, cutoff){
  
  home <- getwd()
  setwd(folder_name)
  
  folder_split <- str_split(folder_name, "_", simplify=TRUE)
  prob_PERC_out <- folder_split[ncol(folder_split)]
  
  df <- read_csv("output_data.csv", col_names = TRUE) %>%
    select(-one_of(c("ResidualBoard", "X12"))) %>% # drop unwanted variables
    filter(is_Admit == 1) %>% # only keep the people that went thru the system
    filter(is_Ped == 0) %>% # no kids
    filter(ArrivalTime > cutoff) %>% # people arrived after warm-up period
    mutate(ESI = as.factor(ESI)) %>% 
    mutate(LOS = WaitTime + ServiceTime + RegularBoard) # compute total length of stay
  
  df$prob_PERC_out <- as.numeric(rep(prob_PERC_out, nrow(df)))
  setwd(home)
  
  return(df)
}

#--> Function to loop and compose several sim runs
composeData <- function(path, folder_names, cutoff){
  df <- cleanData(path, folder_names[1], cutoff)
  
  for (ff in 2:length(folder_names)){
    try(
      df <- rbind(df, cleanData(path, folder_names[ff], cutoff))  
    )
  }
  
  return(df)
}


df <- composeData(path, folder_names, cutoff)

#--> Write this bad boy to a CSV of his own
write_csv(df, save_name)
