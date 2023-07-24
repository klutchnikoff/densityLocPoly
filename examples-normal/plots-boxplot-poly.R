library(tidyverse)

Nobs <- c(200, 500, 1000, 2000)
KK <- c(1, 1.6, 2.1)

lp_opt <- tibble(k = numeric(), n = numeric(), risk = numeric())
sparr_opt <- tibble(k = numeric(), n = numeric(), risk = numeric())

for (k in KK) {
  for (n in Nobs) {
    lp0 <- readRDS(str_c("data/rlp0-k", k, "-Nobs", n,"--norm.rds"))
    lp1 <- readRDS(str_c("data/rlp1-k", k, "-Nobs", n,"--norm.rds"))
    lp2 <- readRDS(str_c("data/rlp2-k", k, "-Nobs", n,"--norm.rds"))
    sparr <- readRDS(str_c("data/rsparr-k", k, "-Nobs", n,"--norm.rds"))

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
        lp1 = find_best(lp1),
        lp2 = find_best(lp2)
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

## Dessins

pdf("img/boxplot-k2.1--norm.pdf", height = 4, width = 8)
n_names <- as_labeller(
  c(`200` = "n = 200",
    `500` = "n = 500",
    `1000` = "n = 1000",
    `2000` = "n = 2000"))
bplot |>
  filter(k==2.1) |>
  ggplot() +
    aes(x = Method, y = risk, fill = Method) +
    geom_boxplot(position = position_dodge(), outlier.size = -1, outlier.alpha = 0.5) +
    scale_y_continuous(limits = c(0, 4)) + # 1=> 5, 1.6 => 10
    xlab("") +
    facet_wrap(~n, nrow = 1, labeller = n_names) +
    scale_fill_grey(start = 0.5, end = 0.9) +
    theme_classic() +
    theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.line.y = element_blank())
dev.off()

