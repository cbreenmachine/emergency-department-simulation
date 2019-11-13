library(tidyverse)
library(ggthemes)


data_name <- "v3" # must match what folder you're in
save_folder <- "../../final-graphs/"
bs = 12 # base font size
max_x <- 1000 # x scale for distributions
w <- 9 # width of saved figure in inches
h <- 5 # height

df <- read_csv(paste0("data-", data_name, ".csv")) %>%
  mutate(ESI = as.factor(paste0("ESI-", ESI)))

#--> Distributions
ground_truth <- df %>%
  filter(prob_PERC_out == 0 | prob_PERC_out == 100) %>%
  mutate(prob_PERC_out = as.factor(prob_PERC_out)) %>%
  ggplot(aes(x=LOS, y=..density.., fill=prob_PERC_out)) +
  geom_histogram(position="identity", alpha = .4) +
  facet_wrap(.~ESI) +
  xlim(c(0, max_x)) +
  theme_clean(base_size = bs) +
  theme(legend.position = c(0.8, 0.2)) +
  scale_fill_fivethirtyeight() +
  labs(title = "Length of stay with and without implementing PERC") +
  labs(x = "Length of stay (mins)",
       y = "Density",
       fill = "P(PPE patient PERCs out)*",
       caption = expression(italic("*Proportion of PPE patients who are filtered out by PERC rule (i.e. do not receive CT scan)"))) 
  

ggsave(paste0(save_folder, data_name, "-zero-to-100.png"), ground_truth, width=w, height = h)



#--> Compute confidence intervals (95%)
alpha <- .025

#--> Mean length of stay by ESI
grp_ci <- df %>%
  group_by(ESI, prob_PERC_out) %>%
  summarise(my_mean = mean(LOS),
            lower = my_mean + qnorm(alpha)*sd(LOS)/sqrt(n()),
            upper = my_mean + qnorm(1-alpha)*sd(LOS)/sqrt(n()))


#--> Overal mean length of stay
tot_ci <- df %>%
  group_by(prob_PERC_out) %>%
  summarise(my_mean = mean(LOS),
            lower = my_mean + qnorm(alpha)*sd(LOS)/sqrt(n()),
            upper = my_mean + qnorm(1-alpha)*sd(LOS)/sqrt(n()))

#--> Plot overall means
overall_ci <- tot_ci %>%
  ggplot(aes(x=prob_PERC_out, y=my_mean)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymax=upper,ymin=lower)) +
  theme_clean(base_size = bs) +
  scale_x_continuous(breaks = seq(0, 100, by = 25)) +
  ylab("Average length of stay (mins)*") +
  xlab('Proportion that PERC out**') +
  labs(title = "Average length of stay by ESI classification", 
       subtitle = "Over a range of PERC success rates", 
       caption = expression(italic("*Error bars show the 95% confidence interval around the mean          **PPE patients who are filtered out by PERC rule (i.e. do not receive CT scan)")))

ggsave(paste0(save_folder, data_name, "-overall-means.png"), overall_ci, width=w, height = h)

#--> plot by groups
group_ci <- grp_ci %>%
  ggplot(aes(x=prob_PERC_out, y=my_mean)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymax=upper,ymin=lower)) +
  facet_wrap(.~ESI, scales="free_y") +
  theme_clean(base_size = bs) +
  scale_x_continuous(breaks = seq(0, 100, by = 25)) +
  ylab("Average length of stay (mins)*") +
  xlab('Proportion that PERC out**') +
  labs(title = "Average length of stay by ESI classification", 
       subtitle = "Over a range of PERC success rates", 
       caption = expression(italic("*Error bars show the 95% confidence interval around the mean          **PPE patients who are filtered out by PERC rule (i.e. do not receive CT scan)")))

ggsave(paste0(save_folder, data_name,"-esi-means.png"), group_ci, width=w, height = h)