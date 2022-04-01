# Load packages:
library(DHARMa)
library(glmmTMB)
library(dplyr)

# import data:
repro <- read.csv2("Data/reproduction.csv", stringsAsFactors = TRUE)
str(repro)

# check treatment levels and sort:
levels(repro$treatment)
repro$treatment <- factor(repro$treatment, levels = c("control_0", "brake_abrasion_0.5", "brake_abrasion_2",
                                                  "exhaust_particles_0.5", "exhaust_particles_2", "MP_fibers_0.5", "MP_fibers_2", 
                                                  "MP_particles_0.5", "MP_particles_2", "mix_0.5", "mix_2", "mix_8"))

# Number of cocoons:
# check data
repro %>% group_by(treatment) %>% summarise(sum = sum(cocoons))

# no cocoons in brake_abrasion_0.5 and brake_abrasion_2. Add one dummy cocoon each:
dummy = data.frame(
  Pollutant = NA,
  Concentration = NA,
  Glass = NA,
  juvenile = NA,
  cocoons = 1,
  treatment = c("brake_abrasion_0.5", "brake_abrasion_2")
)
repro.added <- rbind(repro, dummy)


# Fit model:
count_coc <- glmmTMB(cocoons ~ treatment, repro.added, family = nbinom2)

# Check residuals:
res = simulateResiduals(count_coc)
testDispersion(res)
plot(res, asFactor = T) # There are within-group deviations from uniformity, but this is due to the all-zero group predictions

# Regression table output:
summary(count_coc)


# Number of juveniles:
# Check data
repro %>% group_by(treatment) %>% summarise(sum = sum(juvenile))

# Five treatments without juveniles (all zero counts). Add one dummy juvenile each:
dummy2 = data.frame(
  Pollutant = NA,
  Concentration = NA,
  Glass = NA,
  juvenile = 1,
  cocoons = NA,
  treatment = c("brake_abrasion_0.5", "brake_abrasion_2", "exhaust_particles_0.5", "MP_particles_0.5", "mix_8")
)
repro.added2 <- rbind(repro, dummy2)

# Fit model:
count_juv <- glmmTMB(juvenile ~ treatment, repro.added2, family = nbinom2)

# Check residuals:
res = simulateResiduals(count_juv)
testDispersion(res)
plot(res, asFactor = T)

# Regression table output:
summary(count_juv)

# save results:
saveRDS(list(coc_result = count_coc, juv_result = count_juv), "Results/repro.RDS")

### END -------------


