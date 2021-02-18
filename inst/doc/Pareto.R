## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(Pareto)
x <- c(1:10) * 1000
pPareto(x, 1000, 2)
plot(pPareto(1:5000, 1000, 2), xlab = "x", ylab = "CDF(x)")
dPareto(x, 1000, 2)
plot(dPareto(1:5000, 1000, 2), xlab = "x", ylab = "PDF(x)")

## -----------------------------------------------------------------------------
qPareto(0:10 / 10, 1000, 2)

## -----------------------------------------------------------------------------
rPareto(20, 1000, 2)

## -----------------------------------------------------------------------------
Pareto_Layer_Mean(4000, 1000, 2, t = 500)

## -----------------------------------------------------------------------------
Pareto_Layer_Var(4000, 1000, 2, t = 500)

## -----------------------------------------------------------------------------
Pareto_Extrapolation(4000, 1000, 5000, 5000, 2) * 500
Pareto_Extrapolation(4000, 1000, 5000, 5000, 2, ExpLoss_1 = 500)

## -----------------------------------------------------------------------------
Pareto_Find_Alpha_btw_Layers(4000, 1000, 500, 5000, 5000, 62.5)

## -----------------------------------------------------------------------------
Pareto_Find_Alpha_btw_FQ_Layer(500, 2.5, 4000, 1000, 500)

## -----------------------------------------------------------------------------
Pareto_Layer_Mean(4000, 1000, 2, t = 500) * 2.5

## -----------------------------------------------------------------------------
Pareto_Find_Alpha_btw_Layers(30, 10, 26.66, 60, 40, 15.95)

## -----------------------------------------------------------------------------
26.66 / Pareto_Layer_Mean(30, 10, 1.086263)

## -----------------------------------------------------------------------------
t_1 <- 1000
f_1 <- 2
t_2 <- 4000
(f_2 <- f_1 * (t_1 / t_2)^2.5)

## -----------------------------------------------------------------------------
Pareto_Find_Alpha_btw_FQs(t_1, f_1, t_2, f_2)

## -----------------------------------------------------------------------------
losses <- rPareto(1000, t = 1000, alpha = 2)
Pareto_ML_Estimator_Alpha(losses, t = 1000)

## -----------------------------------------------------------------------------
losses_1 <- rPareto(5000, t = 1000, alpha = 2)
losses_2 <- rPareto(5000, t = 1000, alpha = 2)
reported <- losses_2 > 3000
losses_2 <- losses_2[reported]
losses <- c(losses_1, losses_2)
Pareto_ML_Estimator_Alpha(losses, t = 1000)

## -----------------------------------------------------------------------------
reporting_thresholds_1 <- rep(1000, length(losses_1))
reporting_thresholds_2 <- rep(3000, length(losses_2))
reporting_thresholds <- c(reporting_thresholds_1, reporting_thresholds_2)
Pareto_ML_Estimator_Alpha(losses, t = 1000, reporting_thresholds = reporting_thresholds)

## -----------------------------------------------------------------------------
limits <- sample(c(5000, 10000), length(losses), replace = T)
censored <- losses > limits
losses[censored] <- limits[censored]
reported <- losses > reporting_thresholds
losses <- losses[reported]
reporting_thresholds <- reporting_thresholds[reported]
Pareto_ML_Estimator_Alpha(losses, t = 1000, reporting_thresholds = reporting_thresholds)

## -----------------------------------------------------------------------------
Pareto_ML_Estimator_Alpha(losses, t = 1000, reporting_thresholds = reporting_thresholds, 
                          is.censored = censored)

## -----------------------------------------------------------------------------
x <- c(1:10) * 1000
t <- c(1000, 2000, 3000, 4000)
alpha <- c(2, 1, 3, 20)
pPiecewisePareto(x, t, alpha)
plot(pPiecewisePareto(1:5000, t, alpha), xlab = "x", ylab = "CDF(x)")

## -----------------------------------------------------------------------------
dPiecewisePareto(x, t, alpha)
plot(dPiecewisePareto(1:5000, t, alpha), xlab = "x", ylab = "PDF(x)")

## -----------------------------------------------------------------------------
rPiecewisePareto(20, t, alpha)

## -----------------------------------------------------------------------------
PiecewisePareto_Layer_Mean(4000, 1000, t, alpha)

## -----------------------------------------------------------------------------
PiecewisePareto_Layer_Var(4000, 1000, t, alpha)

## -----------------------------------------------------------------------------
losses <- rPiecewisePareto(10000, t = c(1000, 2000, 3000), alpha = c(1, 2, 3))
PiecewisePareto_ML_Estimator_Alpha(losses, c(1000, 2000, 3000))

## -----------------------------------------------------------------------------
losses_1 <- rPiecewisePareto(5000, t = c(1000, 2000, 3000), alpha = c(1, 2, 3))
losses_2 <- rPiecewisePareto(5000, t = c(1000, 2000, 3000), alpha = c(1, 2, 3))
reported <- losses_2 > 3000
losses_2 <- losses_2[reported]
losses <- c(losses_1, losses_2)
PiecewisePareto_ML_Estimator_Alpha(losses, c(1000, 2000, 3000))

reporting_thresholds_1 <- rep(1000, length(losses_1))
reporting_thresholds_2 <- rep(3000, length(losses_2))
reporting_thresholds <- c(reporting_thresholds_1, reporting_thresholds_2)
PiecewisePareto_ML_Estimator_Alpha(losses, c(1000, 2000, 3000), 
                                   reporting_thresholds = reporting_thresholds)

limits <- sample(c(2500, 5000, 10000), length(losses), replace = T)
censored <- losses > limits
losses[censored] <- limits[censored]
reported <- losses > reporting_thresholds
losses <- losses[reported]
reporting_thresholds <- reporting_thresholds[reported]
censored <- censored[reported]
PiecewisePareto_ML_Estimator_Alpha(losses, c(1000, 2000, 3000), 
                                   reporting_thresholds = reporting_thresholds)
PiecewisePareto_ML_Estimator_Alpha(losses, c(1000, 2000, 3000), 
                                   reporting_thresholds = reporting_thresholds, 
                                   is.censored = censored)

## -----------------------------------------------------------------------------
attachment_points <- c(1000, 1500, 2000, 2500, 3000)
exp_losses <- c(100, 90, 50, 40, 100)
fit <- PiecewisePareto_Match_Layer_Losses(attachment_points, exp_losses)
fit

## -----------------------------------------------------------------------------
c(PiecewisePareto_Layer_Mean(500, 1000, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 1500, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 2000, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 2500, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(Inf, 3000, fit$t, fit$alpha) * fit$FQ)

## -----------------------------------------------------------------------------
covers <- c(diff(attachment_points), Inf)
Layer_Mean(fit, covers, attachment_points)

## -----------------------------------------------------------------------------
  covers <- c(1000, 1000, 1000)
  att_points <- c(1000, 2000, 5000)
  exp_losses <- c(100, 50, 10)
  thresholds <- c(4000, 10000)
  fqs <- c(0.04, 0.005)
  fit <- Fit_References(covers, att_points, exp_losses, thresholds, fqs)
  Layer_Mean(fit, covers, att_points)
  Excess_Frequency(fit, thresholds)

## -----------------------------------------------------------------------------
return_periods <- c(1, 5, 10, 20, 50, 100)
amounts <- c(1000, 4000, 7000, 10000, 13000, 14000)
fit <- Fit_PML_Curve(return_periods, amounts)
1 / Excess_Frequency(fit, amounts)

## -----------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
PPPM

## -----------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
Layer_Mean(PPPM, 4000, 1000)
Layer_Sd(PPPM, 4000, 1000)
Layer_Var(PPPM, 4000, 1000)

## -----------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
thresholds <- c(0, 1000, 2000, 5000, 10000, Inf)
Excess_Frequency(PPPM, thresholds)

## -----------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
Simulate_Losses(PPPM, 10)

## -----------------------------------------------------------------------------
x <- c(1:10) * 1000
pGenPareto(x, t = 1000, alpha_ini = 1, alpha_tail = 2)
plot(pGenPareto(1:5000, 1000, 1, 2), xlab = "x", ylab = "CDF(x)")
dGenPareto(x, t = 1000, alpha_ini = 1, alpha_tail = 2)
plot(dGenPareto(1:5000, 1000, 1, 2), xlab = "x", ylab = "PDF(x)")

## -----------------------------------------------------------------------------
qGenPareto(0:10 / 10, 1000, 1, 2)

## -----------------------------------------------------------------------------
rGenPareto(20, 1000, 1, 2)

## -----------------------------------------------------------------------------
GenPareto_Layer_Mean(4000, 1000, t = 500, alpha_ini = 1, alpha_tail = 2)

## -----------------------------------------------------------------------------
GenPareto_Layer_Var(4000, 1000, t = 500, alpha_ini = 1, alpha_tail = 2)

## -----------------------------------------------------------------------------
losses <- rGenPareto(10000, t = 1000, alpha_ini = 1, alpha_tail = 2)
GenPareto_ML_Estimator_Alpha(losses, 1000)

## -----------------------------------------------------------------------------
losses_1 <- rGenPareto(5000, t = 1000, alpha_ini = 1, alpha_tail = 2)
losses_2 <- rGenPareto(5000, t = 1000, alpha_ini = 1, alpha_tail = 2)
reported <- losses_2 > 3000
losses_2 <- losses_2[reported]
losses <- c(losses_1, losses_2)
GenPareto_ML_Estimator_Alpha(losses, 1000)

reporting_thresholds_1 <- rep(1000, length(losses_1))
reporting_thresholds_2 <- rep(3000, length(losses_2))
reporting_thresholds <- c(reporting_thresholds_1, reporting_thresholds_2)
GenPareto_ML_Estimator_Alpha(losses, 1000, 
                             reporting_thresholds = reporting_thresholds)

limits <- sample(c(2500, 5000, 10000), length(losses), replace = T)
censored <- losses > limits
losses[censored] <- limits[censored]
reported <- losses > reporting_thresholds
losses <- losses[reported]
reporting_thresholds <- reporting_thresholds[reported]
censored <- censored[reported]
GenPareto_ML_Estimator_Alpha(losses, 1000, 
                             reporting_thresholds = reporting_thresholds)
GenPareto_ML_Estimator_Alpha(losses, 1000, 
                             reporting_thresholds = reporting_thresholds, 
                             is.censored = censored)

## -----------------------------------------------------------------------------
PGPM <- PGP_Model(FQ = 2, t = 1000, alpha_ini = 1, alpha_tail = 2, 
                  truncation = 10000, dispersion = 1.5)
PGPM

## -----------------------------------------------------------------------------
PGPM <- PGP_Model(FQ = 2, t = 1000, alpha_ini = 1, alpha_tail = 2, 
                  truncation = 10000, dispersion = 1.5)
Layer_Mean(PGPM, 4000, 1000)
Layer_Sd(PGPM, 4000, 1000)
Layer_Var(PGPM, 4000, 1000)
thresholds <- c(0, 1000, 2000, 5000, 10000, Inf)
Excess_Frequency(PGPM, thresholds)
Simulate_Losses(PGPM, 10)

