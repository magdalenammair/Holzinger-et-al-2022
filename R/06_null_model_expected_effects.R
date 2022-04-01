# load functions ------------
source(file = "R/functions.R")


# import results ------------
survival <- readRDS("Results/results_survival.rds")
repro <- readRDS("Results/repro.rds")
weight.par <- readRDS("Results/weight.par.RDS")
ros <- readRDS("Results/ros.RDS")


## 1. survival ----------

### 1A week 4: ----------
summary(survival$w4.result)

# multiplicative effect at 0.5 %v/v
surv.4w.0.5 <- multiplicative(survival$w4.result, c(2,4,6,8), link = "logit") [[2]]

# multiplicative effect at 2 %v/v
surv.4w.2 <- multiplicative(survival$w4.result, c(3, 5, 7, 9), link = "logit") [[2]]

### 1B week 8: ---------
summary(survival$w8.result)

# multiplicative effect at 0.5 %v/v
surv.8w.0.5 <- multiplicative(survival$w8.result, c(2,4,6,8), link = "logit") [[2]]

# multiplicative effect at 2 %v/v
surv.8w.2 <- multiplicative(survival$w8.result, c(3, 5, 7, 9), link = "logit") [[2]]

## 2. reproduction ----------



### 2A cocoons: --------
summary(repro$coc_result)

# dominance effect at 0.5 %v/v: proportional to control mean
coc.0.5 <- exp(repro$coc_result$fit$par[1] + repro$coc_result$fit$par[2])

# dominance effect at 2 %v/v: proportional to control mean
coc.2 <- exp(repro$coc_result$fit$par[1] + repro$coc_result$fit$par[3])

### 2B juveniles: --------
summary(repro$juv_result)

# dominance effect at 0.5 %v/v: proportional to control mean
juv.0.5 <- exp(repro$juv_result$fit$par[1] + repro$juv_result$fit$par[2])

# dominance effect at 2 %v/v: proportional to control mean
juv.2 <- exp(repro$juv_result$fit$par[1] + repro$juv_result$fit$par[3])

## 3. Weight---------

weight.par

# additive effect at 0.5 %v/v
weight.0.5 <- additive(weight.par, c(14, 16, 18, 20), link = "identity", control = 13)[[1]]
# this is the expected difference to the control treatment slope (i.e. weight gain) 
# predict weight after 4 and 8 weeks:
weight.0.5.4w <- (weight.par$fit$par[1] + weight.par$fit$par[11]) + 4 * weight.0.5 
weight.0.5.8w <- (weight.par$fit$par[1] + weight.par$fit$par[11]) + 8 * weight.0.5 
# (mix2 intercept) + 4 * expected slope 


# additive effect at 2 %v/v
weight.2 <- additive(weight.par, c(15, 17, 19, 21), link = "identity", control = 13)[[1]]
# this is the expected difference to the control treatment slope (i.e. weight gain) 
# predict weight after 4 and 8 weeks:
weight.2.4w <- (weight.par$fit$par[1] + weight.par$fit$par[11]) + 4 * weight.2 
weight.2.8w <- (weight.par$fit$par[1] + weight.par$fit$par[11]) + 8 * weight.2 

## 4. ROS ---------

### 4A CAT ------
summary(ros$CAT)

CAT.0.5 <- additive(ros$CAT, c(2, 3, 5, 7), link = ros$CAT$modelInfo$family$link)
CAT.0.5 <- CAT.0.5$additive.effect
CAT.2 <- additive(ros$CAT, c(4, 6, 8), link = ros$CAT$modelInfo$family$link)
CAT.2 <- CAT.2$additive.effect

### 4B GST ------
summary(ros$GST)

GST.0.5 <- additive(ros$GST, c(2, 3, 5, 7), link = ros$GST$modelInfo$family$link)
GST.0.5 <- GST.0.5$additive.effect
GST.2 <- additive(ros$GST, c(4, 6, 8), link = ros$GST$modelInfo$family$link)
GST.2 <- GST.2$additive.effect

### 4C LPO ------
summary(ros$LPO)

LPO.0.5 <- additive(ros$LPO, c(2, 3, 5, 7), link = ros$LPO$modelInfo$family$link)
LPO.0.5 <- LPO.0.5$additive.effect
LPO.2 <- additive(ros$LPO, c(4, 6, 8), link = ros$LPO$modelInfo$family$link)
LPO.2 <- LPO.2$additive.effect

# save results--------
saveRDS(data.frame(surv.4w.0.5, surv.4w.2, surv.8w.0.5, surv.8w.2, 
                   coc.0.5, coc.2, juv.0.5, juv.2, 
                   weight.0.5.4w, weight.0.5.8w, weight.2.4w, weight.2.8w,
                   CAT.0.5, CAT.2, GST.0.5, GST.2, LPO.0.5, LPO.2), file = "Results/expected.RDS")



