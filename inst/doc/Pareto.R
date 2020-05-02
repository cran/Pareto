## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(Pareto)
x <- c(1:10) * 1000
pPareto(x, 1000, 2)
plot(pPareto(1:5000, 1000, 2), xlab = "x", ylab = "CDF(x)")
dPareto(x, 1000, 2)
plot(dPareto(1:5000, 1000, 2), xlab = "x", ylab = "PDF(x)")

## ------------------------------------------------------------------------
qPareto(0:10 / 10, 1000, 2)

## ------------------------------------------------------------------------
rPareto(20, 1000, 2)

## ------------------------------------------------------------------------
Pareto_Layer_Mean(4000, 1000, 2, t = 500)

## ------------------------------------------------------------------------
library(Pareto)
Pareto_Layer_Var(4000, 1000, 2, t = 500)

## ------------------------------------------------------------------------
Pareto_Extrapolation(4000, 1000, 5000, 5000, 2) * 500
Pareto_Extrapolation(4000, 1000, 5000, 5000, 2, ExpLoss_1 = 500)

## ------------------------------------------------------------------------
Pareto_Find_Alpha_btw_Layers(4000, 1000, 500, 5000, 5000, 62.5)

## ------------------------------------------------------------------------
Pareto_Find_Alpha_btw_FQ_Layer(500, 2.5, 4000, 1000, 500)

## ------------------------------------------------------------------------
Pareto_Layer_Mean(4000, 1000, 2, t = 500) * 2.5

## ------------------------------------------------------------------------
Pareto_Find_Alpha_btw_Layers(30, 10, 26.66, 60, 40, 15.95)

## ------------------------------------------------------------------------
26.66 / Pareto_Layer_Mean(30, 10, 1.086263)

## ------------------------------------------------------------------------
t_1 <- 1000
f_1 <- 2
t_2 <- 4000
(f_2 <- f_1 * (t_1 / t_2)^2.5)

## ------------------------------------------------------------------------
Pareto_Find_Alpha_btw_FQs(t_1, f_1, t_2, f_2)

## ------------------------------------------------------------------------
losses <- rPareto(1000, t = 100, alpha = 2)
Pareto_ML_Estimator_Alpha(losses, t = 100)

## ------------------------------------------------------------------------
x <- c(1:10) * 1000
t <- c(1000, 2000, 3000, 4000)
alpha <- c(2, 1, 3, 20)
pPiecewisePareto(x, t, alpha)
plot(pPiecewisePareto(1:5000, t, alpha), xlab = "x", ylab = "CDF(x)")

## ------------------------------------------------------------------------
dPiecewisePareto(x, t, alpha)
plot(dPiecewisePareto(1:5000, t, alpha), xlab = "x", ylab = "PDF(x)")

## ------------------------------------------------------------------------
rPiecewisePareto(20, t, alpha)

## ------------------------------------------------------------------------
PiecewisePareto_Layer_Mean(4000, 1000, t, alpha)

## ------------------------------------------------------------------------
PiecewisePareto_Layer_Var(4000, 1000, t, alpha)

## ------------------------------------------------------------------------
losses <- rPiecewisePareto(10000, t = c(100,200,300), alpha = c(1,2,3))
PiecewisePareto_ML_Estimator_Alpha(losses, c(100,200,300))

## ------------------------------------------------------------------------
attachment_points <- c(1000, 1500, 2000, 2500, 3000)
exp_losses <- c(100, 90, 50, 40, 100)
fit <- PiecewisePareto_Match_Layer_Losses(attachment_points, exp_losses)
fit

## ------------------------------------------------------------------------
c(PiecewisePareto_Layer_Mean(500, 1000, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 1500, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 2000, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(500, 2500, fit$t, fit$alpha) * fit$FQ,
  PiecewisePareto_Layer_Mean(Inf, 3000, fit$t, fit$alpha) * fit$FQ)

## ------------------------------------------------------------------------
covers <- c(diff(attachment_points), Inf)
PPP_Model_Exp_Layer_Loss(covers, attachment_points, fit)

## ------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
PPPM

## ------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
PPP_Model_Exp_Layer_Loss(4000, 1000, PPPM)
PPP_Model_Layer_Sd(4000, 1000, PPPM)
PPP_Model_Layer_Var(4000, 1000, PPPM)

## ------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
thresholds <- c(0, 1000, 2000, 5000, 10000, Inf)
PPP_Model_Excess_Frequency(thresholds, PPPM)

## ------------------------------------------------------------------------
PPPM <- PPP_Model(FQ = 2, t = c(1000, 2000), alpha = c(1, 2), 
                  truncation = 10000, truncation_type = "wd", dispersion = 1.5)
PPP_Model_Simulate(10, PPPM)

