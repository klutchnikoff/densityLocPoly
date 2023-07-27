library(tidyverse)

##
## This file was used to generate the boxplots
## obtained in the paper
## [2023] K. Bertin, N. Klutchnikoff and F. Ouimet
## A NEW ADAPTIVE LOCAL POLYNOMIAL DENSITY ESTIMATION PROCEDURE ON COMPLICATED DOMAINS
## ArXiv:
##

##
## Init
##

gather_LP <- TRUE # if TRUE we plot the boxplot of LP = min(LP0, LP1)

for(k0 in c(1, 2.1)){
  for(type in c("poly", "norm")){
    risk <- read_csv(str_c("data/risk_", type, ".csv"))

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

    pdf(
      file = str_c("img/boxplot-", type, "-k", 10*k0, ".pdf"),
      height = 4, width = 8
    )
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
  }
}


