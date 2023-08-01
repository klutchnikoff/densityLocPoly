library(densityLocPoly)


risk_P <- simulate_risk_polysec(
  NN = c(200, 500, 1000, 2000),
  RR = 1:5000,
  HH = seq(0.01, 0.6, by = 0.02),
  KK = c(1, 2.1),
  density_type = "f_poly"
)
save(risk_P, file = "../risk_poly_definitif-5000")


risk_N <- simulate_risk_polysec(
  NN = c(200, 500, 1000, 2000),
  RR = 1:5000,
  HH = seq(0.01, 0.6, by = 0.02),
  KK = c(1, 2.1),
  density_type = "f_norm"
)
save(risk_N, file = "../risk_norm_definitif-5000")
