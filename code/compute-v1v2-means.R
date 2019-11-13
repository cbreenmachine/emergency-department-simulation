library(tidyverse)
library(readxl)

# Given a set of input parameters (computed based on historical 2012 data), we want
# to find a new set of parameters that stratify non-PPE, PPE-no CT, and PPE-CT patients'
# mean service times. For more background, refer to paper linked in Repo.
# In essence, this works thru the algebra.

ct_scan_length <- 60 # how long the physician thinks a CT scan take (mins)
data_name <- "v2" # swap between v1/v2

orig.df <- read_excel("input-times.xlsx", col_names = TRUE) %>%
  filter(str_detect(type, "mu_1")) %>% #the second parameters are not important
  mutate(mean = exp(mu + sigma*sigma/2))

df <- read_excel(paste0(data_name,"-input.xlsx"), col_names = TRUE) %>%
  spread(key=group, value=probability)

comp.df <- left_join(orig.df, df) %>%
  mutate(normal = mean,
         ppe = normal - ct_scan_length*(ct / (ct + ppe)), # conditional probability
         # P(CT|PPE) = P(CT) / (P(CT and PPE) + P(No CT and PPE))
         ct = ppe + ct_scan_length) %>%
  mutate(mu_normal = log(normal) - (sigma^2)/2,
         mu_ppe = log(ppe) - (sigma^2)/2,
         mu_ct = log(ct) - (sigma^2)/2)

mu.df <- comp.df %>%
  select(-c(ct, normal, ppe, mu, sigma, mean))

mu.df

write.csv(mu.df, file = paste0(data_name, "-parameters.csv"))
