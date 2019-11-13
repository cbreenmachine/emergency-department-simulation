library(tidyverse)
library(readxl)


# Given a set of input parameters (computed based on historical 2012 data), we want
# to find a new set of parameters that stratify non-PPE, PPE-no CT, and PPE-CT patients'
# mean service times. For more background, refer to paper linked in Repo.
# In essence, this works thru the algebra.

df <- read_excel("v3-input.xlsx", col_names = TRUE)

df <- df %>%
  group_by(ESI) %>%
  mutate(mean_time = sum(probability*time)) %>%
  mutate(diff = (mean_time - time))

diff.df <- df %>%
  select(group, diff) %>%
  spread(key=group, value=diff)

orig.df <- read_excel("input-times.xlsx", col_names=TRUE) %>%
  filter(str_detect(type, "mu_1")) %>%
  mutate(mean = exp(mu + sigma*sigma/2))

 comp.df <- left_join(orig.df, diff.df) %>%
   mutate(mu_normal = log(mean - normal) - (sigma^2)/2,
          mu_ppe = log(mean - ppe) - (sigma^2)/2,
          mu_ct = log(mean - ct) - (sigma^2)/2)

 
mu.df <- comp.df %>%
  select(-c(ct, normal, ppe, mu, sigma, mean))

write.csv(mu.df, "v3-parameters.csv")

