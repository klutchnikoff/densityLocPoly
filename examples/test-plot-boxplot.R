library(tidyverse)
library(densityLocPoly)
library(spatstat)
library(patchwork)

gather_LP <- TRUE # if TRUE we plot the boxplot of LP = min(LP0, LP1)
ggg <- list()

ii <- 0
for (k0 in c(1, 2.1)) {
  for (type in c("poly", "norm")) {

    ii <- ii+1

    risk <- get(str_glue("risk_{type}")) |>
      filter(method %in% c("SPARR", "LP0", "LP1", "LP2")) |>
      filter(h <= 0.6)

    find_best_h <- risk |>
      group_by(n, h, method) |>
      summarise(mse = mean(value)) |>
      group_by(n, method, .drop = TRUE) |>
      summarise(h = h[which.min(mse)], mse = min(mse))

    if (gather_LP) {
      a <- find_best_h |>
        group_by(n) |>
        filter(str_detect(method, "^LP")) |>
        filter(mse == min(mse))
      b <- find_best_h |>
        filter(str_detect(method, "^SPARR"))
      find_best_h <- bind_rows(a, b)
    }

    find_best_h <- find_best_h |> select(n, method, h)

    bplot <- semi_join(risk, find_best_h, by = c("method", "h")) |>
      filter(k == k0) |>
      select(rep, n, method, value)

    if (gather_LP){
      bplot <- bplot |> mutate(method = str_replace(method, "LP[0123456789]*", "LP"))
    }

    ylimit <- bplot |>
      group_by(n, method) |>
      summarise(l = quantile(value, 0.75) + 1.59 * IQR(value))
    ylimit <- max(ylimit$l)

    ggg[[ii]] <-
      ggplot(bplot) +
        aes(x = method, y = value, fill = method) +
        geom_boxplot(
          position = position_dodge(),
          outlier.shape = NA
        ) +
        coord_cartesian(ylim = c(0, ylimit)) +
        stat_summary(fun = "mean") +
        xlab("") +
        facet_wrap(~n, nrow = 1) +
        scale_fill_grey(start = 0.5, end = 0.9) +
        theme_classic() +
        theme(
          legend.position = "none",
          axis.title.y = element_blank(),
          axis.line.y = element_blank()
        )
    pdf(file = str_glue("boxplot-{type}-k{k0}.pdf"), height = 4, width = 8)
    print(ggg[[ii]])
    dev.off()
  }
}

(ggg[[1]] + ggg[[2]]) / (ggg[[3]] + ggg[[4]])



