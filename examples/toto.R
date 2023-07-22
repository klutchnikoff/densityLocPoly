library(tidyverse)

RMSE_lp0 <- r_lp0 |> group_by(h) |> summarise(rmse = mean(value))
plot(RMSE_lp0$h, RMSE_lp0$rmse, ylim = c(0, 50))

RMSE_lp1 <- r_lp1 |> group_by(h) |> summarise(rmse = mean(value))
plot(RMSE_lp1$h, RMSE_lp1$rmse, ylim = c(0, 50))

RMSE_lp2 <- r_lp2 |> group_by(h) |> summarise(rmse = mean(value))
plot(RMSE_lp2$h, RMSE_lp2$rmse, ylim = c(0, 50))

RMSE_sparr <- r_sparr |> group_by(h) |> summarise(rmse = mean(value))
plot(RMSE_sparr$h, RMSE_sparr$rmse, ylim = c(0, 50))
