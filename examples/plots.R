library(tidyverse)

lp0 <- readRDS("data/rlp0-k2.1-Nobs2000.rds")
lp1 <- readRDS("data/rlp1-k2.1-Nobs2000.rds")
lp2 <- readRDS("data/rlp2-k2.1-Nobs2000.rds")
sparr <- readRDS("data/rsparr-k2.1-Nobs2000.rds")

find_best <- function(x){
  x |>
    group_by(h) |>
    summarise(rmse = mean(value)) |>
    arrange(rmse) |>
    slice(1)
}

best_lp <- bind_rows(
  list(
    lp0 = find_best(lp0),
    lp1 = find_best(lp1)
    #lp2 = find_best(lp2)
    ),
  .id = "which") |>
  arrange(rmse) |>
  slice(1)

best_sparr <- find_best(sparr)

lp_opt <- get(best_lp$which) |>
  filter(h == best_lp$h) |>
  select(risk = value)

sparr_opt <- sparr |>
  filter(h == best_sparr$h) |>
  select(risk = value) |>
  mutate(risk = as.vector(risk))

bplot <- bind_rows(
  list(LP = lp_opt,
       SPARR = sparr_opt),
  .id = "Method"
)

pdf("img/boxplot-k21-Nobs2000.pdf", height = 8, width = 8)
boxplot(risk ~ Method, data = bplot,
        main = "k = 2.1 and N = 2000")
dev.off()

