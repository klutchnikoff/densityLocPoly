library(tidyverse)

##
## Init
##

risk <- read_csv("data/risk_poly.csv")
k0 <- 2.1

gather_LP <- TRUE

##
## start
##

find_best_h <- risk  |>
  group_by(h, method) |>
  summarise(mse = mean(value)) |>
  group_by(method) |>
  summarise(h = h[which.min(mse)])

bplot <- semi_join(risk, find_best_h, by = c("method", 'h')) |>
  filter(k == k0) |>
  select(rep, n, method, value)

# best LP
if (gather_LP) {
  bplot <- bplot |>
    pivot_wider(names_from = method, values_from = value) |>
    (\(.){
      mutate(., LP = purrr::pmap_dbl(
        select(., contains("LP")),
        pmin, na.rm=TRUE))
      })() |>
    select(n, SPARR, LP) |>
    pivot_longer(-n, names_to = "method")
}

ylimit <- bplot |>
  group_by(n, method) |>
  summarise(l = quantile(value, 0.75) + 1.59 * IQR(value))
ylimit <- max(ylimit$l)

pdf(file = "img/boxplot-poly-k21.pdf", height = 5, width = 8)
bplot |> ggplot() +
  aes(x = method, y = value, fill = method) +
  geom_boxplot(position = position_dodge(), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, ylimit)) +
  xlab("") +
  facet_wrap(~n, nrow = 1) +
  scale_fill_grey(start = 0.5, end = 0.9) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.line.y = element_blank()
  )
dev.off()

