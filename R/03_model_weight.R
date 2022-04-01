# Load oackages: 
library(metafor)

# Import data:
weight_agg <- read.csv2("Data/weight_agg.csv", stringsAsFactors = TRUE)
#str(weight_agg)


# change week to class numeric 
weight_agg$week <- as.numeric(weight_agg$week)

# calculate variance from standard deviation
weight_agg$var <- weight_agg$sd^2

# create factor weektreat
weight_agg$weektreat = paste(weight_agg$week, weight_agg$treatment)

# order treatment levels
levels(weight_agg$treatment)
weight_agg$treatment <- factor(weight_agg$treatment, levels = c("control__0", 
                                                      "brake_abrasion__0.5", "brake_abrasion__2",
                                                      "exhaust_particles__0.5", "exhaust_particles__2", 
                                                      "MP_fibers__0.5", "MP_fibers__2", 
                                                      "MP_particles__0.5", "MP_particles__2", 
                                                      "mix__0.5", "mix__2", "mix__8"))


# Fit model with interaction of treatment and week to (1) test for differences in weights between pollution and control treatments before exposure, 
# and (2) test whether weight gain (i.e., change in weight over time, the slope) differs between different pollution treatments and control treatment

weight.over.time <- rma.mv(yi = mean, V = var, mods = ~ treatment * week, data = weight_agg)
summary(weight.over.time)

# save results:
saveRDS(weight.over.time, "Results/weight.RDS")

# save parameter estimates in structure readable by function additive():
weight.par <- list(fit = list(par= weight.over.time$beta))
saveRDS(weight.par, "Results/weight.par.RDS")

### END ------

