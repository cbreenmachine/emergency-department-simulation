library(tidyverse)
library(ggthemes)

# Makes a few simple histograms based on historical data

w <- 6
h <- 5


#--> Data from Chen 2018 and Riederer 2016
ESI <- c("ESI-1", "ESI-2","ESI-3","ESI-4","ESI-5")
wait_time <- c(24.4, 24.4, 57.3, 59.2, 59.2)
serve_time <- c(96.4, 392.6, 255.8, 135, 71.8)
los <- c(267.4, 599.3, 420.3, 221.1, 142.1)

df <- data.frame(ESI, wait_time, serve_time, los)

p1 <- df %>%
  ggplot(aes(ESI, wait_time)) +
  geom_col(fill="dodgerblue4") +
  theme_clean() +
  geom_text(aes(label = wait_time), vjust = -0.5) +
  labs(x="ESI", 
       y="Average wait time (mins)",
       title="Average wait time by ESI (adults)")
ggsave("wait-mean.png", p1, width=w, height = h)

p2 <- df %>%
  ggplot(aes(ESI, serve_time)) +
  geom_col(fill="dodgerblue4") +
  theme_clean() +
  geom_text(aes(label = serve_time), vjust = -0.5) +
  labs(x="ESI", 
       y="Average service time (mins)",
       title="Average service time by ESI (adults)")

ggsave("serve-mean.png", p2, width=w, height = h)

p3 <- df %>%
  ggplot(aes(ESI, los)) +
  geom_col(fill="dodgerblue4") +
  theme_clean() +
  geom_text(aes(label = los), vjust = -0.5) +
  labs(x="ESI", 
       y="Average LOS (mins)",
       title="Average LOS by ESI (adults)")

ggsave("los-mean.png", p3, width=w, height = h)