# load packages
library(glmmTMB)
library(DHARMa)

# import data
surv <- read.csv2("Data/survival.csv", stringsAsFactors = TRUE)
str(surv)

# check treatment levels and sort:
levels(surv$treatment)
surv$treatment <- factor(surv$treatment, levels = c("control__0", "brake_abrasion__0.5", "brake_abrasion__2",
                                                      "exhaust_particles__0.5", "exhaust_particles__2", 
                                                      "MP_fibers__0.5", "MP_fibers__2", 
                                                      "MP_particles__0.5", "MP_particles__2", 
                                                      "mix__0.5", "mix__2", "mix__8"))

# check data
with(surv,table(Status, treatment, Week))
# Week4: MP fibers 2, mix 2: all zeros (all alive)
# Week 8: mix 8: all 1 (all dead)

# add one surviving individual, i.e. one "0" (i.e. alive) value to the week8 mix__8 treatment.  
# And add 1 dead individual for week 4 MP fibers 2 and mix 2.

# Model week 4 and week 8 separately: 

# Week 4:
week4 <- surv[surv$Week == 4,]

add4 <- data.frame(
  Week = 4,
  Status = c(1,1),
  GlassID = "dummy",
  treatment = c("MP_fibers__2", "mix__2")
)
week4.add <- rbind(week4, add4)

# Fit model:
w4.fit <- glmmTMB(Status ~  treatment + (1|GlassID), data = week4.add, family = binomial) 

# Check residuals:
res <- simulateResiduals(w4.fit)
plot(res, asFactor = T)
testDispersion(res)

# Regression table output:
summary(w4.fit)


# Week 8:
week8 <- surv[surv$Week == 8,]

add8 <- data.frame(
  Week = 8,
  Status = 0,
  GlassID = "dummy",
  treatment = "mix__8"
)
week8.add <- rbind(week8, add8)

# Fit model:
w8.fit <- glmmTMB(Status ~  treatment + (1|GlassID), data = week8.add, family = binomial) 

# Check residuals:
res <- simulateResiduals(w8.fit)
testDispersion(res)
plot(res, asFactor=T)

# Regression table output:
summary(w8.fit)


# save model outputs to file:
saveRDS(list(w4.result = w4.fit, w8.result = w8.fit), "Results/results_survival.RDS")

### END -----------


