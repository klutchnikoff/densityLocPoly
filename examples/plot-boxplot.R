library(tidyverse)

risk <- read_csv("data/risk.csv")
k0 <- 2.1

find_best_h <- risk |>
  group_by(h, method) |>
  summarise(mse = mean(value)) |>
  group_by(method) |>
  summarise(h = h[which.min(mse)])

bplot <- semi_join(risk, find_best_h, by = c("method", 'h'))

## Dessins

bplot |>
  filter(k==k0) |>
  ggplot() +
    aes(x = method, y = value, fill = method) +
    geom_boxplot(position = position_dodge(), outlier.size = -1, outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 14)) +
    xlab("") +
    facet_wrap(~n, nrow = 1) +
    scale_fill_grey(start = 0.5, end = 0.9) +
    theme_classic() +
    theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line.y = element_blank())


