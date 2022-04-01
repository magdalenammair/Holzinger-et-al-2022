#### Data aggregation for analysis with metafor package:

library(dplyr)

# Import data:
weight <- read.csv2("Data/weight.csv", stringsAsFactors = TRUE)
#str(weight)

# change week to factor:
weight$week <- as.factor(weight$week)

levels(weight$treatment)

#reorder factor levels:
weight$treatment <- factor(weight$treatment, 
                           levels = c("control_0", 
                                      "break_abrasion_0.5", "brake_abrasion_2", 
                                      "exhaust_particles_0.5", "exhaust_particles_2", 
                                      "MP_fibers_0.5", "MP_fibers_2",
                                      "MP_particles_0.5", "MP_particles_2", 
                                      "mix_0.5", "mix_2", "mix_8"))


# calculate mean weight (+- standard deviation) per replicate per treatment per week
weight_agg <- weight %>%
  group_by(weekGlassID) %>%
  summarise(mean = mean(weight, na.rm = TRUE), sd = sd(weight, na.rm = TRUE)) 

weight_agg <- as.data.frame(weight_agg)

# recover factors week, pollutant, concentration and treatment:

weight_agg$week = NA
weight_agg$Nr = NA
weight_agg$pollutant = NA
weight_agg$concentration = NA


for (i in 1:nrow(weight_agg)) {
  weight_agg$week[i] <- strsplit(as.character(weight_agg$weekGlassID[i]), split = "__")[[1]][1]
  weight_agg$Nr[i] <- strsplit(as.character(weight_agg$weekGlassID[i]), split = "__")[[1]][2]
  weight_agg$pollutant[i] <- strsplit(as.character(weight_agg$weekGlassID[i]), split = "__")[[1]][3]
  weight_agg$concentration[i] <- strsplit(as.character(weight_agg$weekGlassID[i]), split = "__")[[1]][4]
}

weight_agg$treatment <- paste(weight_agg$pollutant, weight_agg$concentration, sep = "__")
weight_agg$GlassID <- paste(weight_agg$treatment, weight_agg$Nr, sep = "__")

str(weight_agg)

write.csv2(weight_agg, "Data/weight_agg.csv", row.names = FALSE)


#### END --------------
