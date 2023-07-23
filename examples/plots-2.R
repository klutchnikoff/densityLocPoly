library(tidyverse)

Nobs <- c(200, 500, 1000, 2000)
KK <- c(1, 1.6, 2.1)

lp_opt <- tibble(k = numeric(), n = numeric(), risk = numeric())
sparr_opt <- tibble(k = numeric(), n = numeric(), risk = numeric())

for (k in KK) {
  for (n in Nobs) {
    lp0 <- readRDS(str_c("data/rlp0-k", k, "-Nobs", n,".rds"))
    lp1 <- readRDS(str_c("data/rlp1-k", k, "-Nobs", n,".rds"))
    lp2 <- readRDS(str_c("data/rlp2-k", k, "-Nobs", n,".rds"))
    sparr <- readRDS(str_c("data/rsparr-k", k, "-Nobs", n,".rds"))

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

    lp_opt_tmp <- get(best_lp$which) |>
      filter(h == best_lp$h) |>
      select(risk = value)
    lp_opt <- bind_rows(
      lp_opt,
      tibble(k = k, n = n, risk = lp_opt_tmp$risk))

    sparr_opt_tmp <- sparr |>
      filter(h == best_sparr$h) |>
      select(risk = value) |>
      mutate(risk = as.vector(risk))
    sparr_opt <- bind_rows(
      sparr_opt,
      tibble(k = k, n = n, risk = sparr_opt_tmp$risk))
  }
}

bplot <- bind_rows(
  list(LP = lp_opt,
    SPARR = sparr_opt),
    .id = "Method"
)
pdf("img/boxplot-k21.pdf", height = 8, width = 8)
bplot |>
  filter( k==2.1 ) |>
  ggplot() +
    aes(x = Method, y = risk, fill = Method) +
    geom_boxplot(position = position_dodge()) +
    facet_wrap(~n)
dev.off()

