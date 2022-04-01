# Load packages:
library(glmmTMB)
library(DHARMa)

# import data
ros <- read.csv2("Data/ros.csv", stringsAsFactors = TRUE)
str(ros)

# sort treatment levels
levels(ros$treatment)
ros$treatment <- factor(ros$treatment, levels = c("control__0", 
                                          "break_abrasion__0.5", 
                                          "exhaust_particles__0.5", "exhaust_particles__2",
                                          "MP_fibers__0.5", "MP_fibers__2", 
                                          "MP_particles__0.5", "MP_particles__2", 
                                          "mix__0.5", "mix__2"))

# CAT ---------------------------------------------------------------------

# Fit model:use Gamma with log-link (right skew and non-negative continuous values)
CAT <- glmmTMB(CAT ~ treatment + (1|GlassID), ros, family = Gamma(link = "log")) # identity link does not converge

# Check residuals:
res = simulateResiduals(CAT)
testDispersion(res)
plot(res) 

# Regression table output:
summary(CAT)

# GST ---------------------------------------------------------------------

# Fit model:
GST <- glmmTMB(GST ~ treatment + (1|GlassID), ros, family = gaussian) 

# Check residuals:
res = simulateResiduals(GST)
testDispersion(res)
plot(res) #this looks good

# Regression table output:
summary(GST)


# LPO ---------------------------------------------------------------------

# Fit model:
LPO <- glmmTMB(LPO ~ treatment + (1|GlassID), ros, family = gaussian)

# Check residuals:
res = simulateResiduals(LPO)
testDispersion(res)
plot(res) 

# Regression table output
summary(LPO)


# Save results:
saveRDS(list(CAT = CAT, GST = GST, LPO = LPO), "Results/ROS.RDS")

### END ------




