# load packages
library(dplyr)
library(glmmTMB)

# set palette
colors.Holzinger <- c("#CC6677", "#882255","#eeeeee", "#eeeeee", "#eeeeee", "#44AA99", "#117733","#bbbbbb", "#666666", "#333333", "#DDCC77", "#999933", "#88CCEE", "#6699CC")
scales::show_col(colors.Holzinger)
palette(colors.Holzinger)

# Import data
surv <- read.csv2("Data/survival.csv", stringsAsFactors = TRUE)

# Import regression model output
surv.results <- readRDS("Results/results_survival.RDS")

# Import expected multiple stressor effects
expected <- readRDS("Results/expected.RDS")

# check treatment levels and sort:
levels(surv$treatment)
surv$treatment <- factor(surv$treatment, levels = c("control__0", "brake_abrasion__0.5", "brake_abrasion__2",
                                                    "exhaust_particles__0.5", "exhaust_particles__2", 
                                                    "MP_fibers__0.5", "MP_fibers__2", 
                                                    "MP_particles__0.5", "MP_particles__2", 
                                                    "mix__0.5", "mix__2", "mix__8"))



# calculate proportion of survived per GlassID per treatment per week:
surv.prop <- surv %>% 
  group_by(Week, treatment, GlassID, .add = TRUE) %>% 
  summarise(sum = sum(Status), n = n()) %>%
  mutate(prop.alive = 1 - sum/n)
surv.prop <- as.data.frame(surv.prop)

#saveRDS(surv.prop, "Data/surv.proportions.RDS")

#create dummy treatments as place holders for plotting:
dummies <- data.frame(
  Week = c("4","4","8","8"),
  treatment = c("d1", "d2", "d1","d2"),
  GlassID = NA,
  sum = NA,
  n = NA,
  prop.alive = NA
)

plot.prop = rbind(dummies, surv.prop)

# put dummies to correct place:
plot.prop$treatment <- factor(plot.prop$treatment, levels = c("control__0", "d1",
                                                    "brake_abrasion__0.5", "brake_abrasion__2",
                                                    "exhaust_particles__0.5", "exhaust_particles__2", 
                                                    "MP_fibers__0.5", "MP_fibers__2", 
                                                    "MP_particles__0.5", "MP_particles__2", 
                                                    "d2",
                                                    "mix__0.5", "mix__2", "mix__8"))
# order data frame according to treatment levels:
plot.prop <- plot.prop[order(plot.prop$treatment),]

# get y-axis positions of asterisks
max <- surv.prop %>% group_by(Week, treatment) %>% summarise(max = max(prop.alive))
max <- as.data.frame(max)
max4 <- max[max$Week == 4,]
max8 <- max[max$Week == 8,]

# get positions of significant results:
summary(surv.results$w4.result)
summary(surv.results$w8.result)
y.sig4 = max4$max[c(2,3,12)]
y.sig8 = max8$max[c(2,3,12)]


### plot
#png("Plots/plot_survival.png", height = 15, width = 25, units = "cm", res = 500)
tiff("Plots/plot_survival.tiff", height = 15, width = 25, units = "cm", res = 500)

opar <- par(oma = c(5,1,1,1), mfrow = c(1,2), xpd = TRUE, bty = "l")

boxplot(prop.alive ~ treatment, data = plot.prop[plot.prop$Week == 4,], las = 2, 
        xlab = "", ylab = "Proportion alive after 4 weeks",
        names = rep("",14), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.prop$treatment))),
        ylim = c(0, 1), pch = 20)
mtext(c("Concentration", "0", "", rep(c("0.5","2"), 4), "","0.5","2","8"),at = c(-2, seq(1, 14, 1)), 
      side = 1, line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Carbon black", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 2, las = 2)
arrows(c(2, 11), -15, c(2, 11), 1.02, angle = 0, lwd = 1)
text(x = c(3, 4, 14), y = y.sig4, "***", pos = 3, cex = 0.7)
mtext("(A)", side = 3, at = 0.7, line = 1)
points(13, expected$surv.4w.0.5, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$surv.4w.2, pch = 17, cex = 1.7, col = "darkred")


boxplot(prop.alive ~ treatment, data = plot.prop[plot.prop$Week == 8,], las = 2, 
        xlab = "", ylab = "Proportion alive after 8 weeks",
        names = rep("",14), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.prop$treatment))),
        ylim = c(0, 1), pch = 20)
mtext(c("0", "", rep(c("0.5","2"), 4), "","0.5","2","8", "Vol%"),at = c(seq(1, 14, 1), 16), 
      side = 1, line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Carbon black", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 2, las = 2)
arrows(c(2, 11), -15, c(2, 11), 1.02, angle = 0, lwd = 1)
text(x = c(3, 4, 14), y = y.sig8, "***", pos = 3, cex = 0.7)
mtext("(B)", side = 3, at = 0.7, line = 1)
points(13, expected$surv.8w.0.5, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$surv.8w.2, pch = 17, cex = 1.7, col = "darkred")

par(opar)

dev.off()



### END -----------


