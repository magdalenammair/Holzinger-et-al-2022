# Load packages:
library(dplyr)

# import data
weight_agg <- read.csv2("Data/weight_agg.csv", stringsAsFactors = TRUE)
str(weight_agg)

# import expected multiple stressor effects:
expected <- readRDS("Results/expected.RDS")

# check treatment levels and sort:
levels(weight_agg$treatment)
weight_agg$treatment <- factor(weight_agg$treatment, levels = c("control__0", 
                                                      "brake_abrasion__0.5", "brake_abrasion__2",
                                                      "exhaust_particles__0.5", "exhaust_particles__2", 
                                                      "MP_fibers__0.5", "MP_fibers__2", 
                                                      "MP_particles__0.5", "MP_particles__2", "mix__0.5", "mix__2", "mix__8"))

# set palette
colors.Holzinger <- c("#CC6677", "#882255","#eeeeee", "#eeeeee", "#eeeeee", "#44AA99", "#117733","#bbbbbb", "#666666", "#333333", "#DDCC77", "#999933", "#88CCEE", "#6699CC")
scales::show_col(colors.Holzinger)
palette(colors.Holzinger)

#create dummy treatments as place holders for plotting:
dummies <- data.frame(
  weekGlassID = NA,
  mean = NA,
  sd = NA,
  week = NA,
  Nr = NA,
  pollutant = NA,
  concentration = NA,
  treatment = c("d1", "d2"),
  GlassID = NA
)
plot.weight = rbind(dummies, weight_agg)

# put dummies to correct place:
plot.weight$treatment <- factor(plot.weight$treatment, levels = c("control__0", "d1",
                                                                "brake_abrasion__0.5", "brake_abrasion__2",
                                                                "exhaust_particles__0.5", "exhaust_particles__2", 
                                                                "MP_fibers__0.5", "MP_fibers__2", 
                                                                "MP_particles__0.5", "MP_particles__2", 
                                                                "d2",
                                                                "mix__0.5", "mix__2", "mix__8"))

# in "brake_abrasion__2", week 4 and week8, there are only 1 and 2 datapoints (1 point with SD one without):
# should be displayed as single datapoints, not as box:

# save data points in separate object:
points.BA2.week4 <- plot.weight$mean[plot.weight$treatment == "brake_abrasion__2" & 
                                       plot.weight$week == 4 &
                                       !is.na(plot.weight$mean)]
sd.BA.week4 <- plot.weight$sd[plot.weight$treatment == "brake_abrasion__2" & 
                                  plot.weight$week == 4 &
                                  !is.na(plot.weight$mean)]

points.BA2.week8 <- plot.weight$mean[plot.weight$treatment == "brake_abrasion__2" & 
                                       plot.weight$week == 8 &
                                       !is.na(plot.weight$mean)]

# change values to NA in plot.weight dataset to avoid plotting as boxplot:
plot.weight$mean[plot.weight$treatment == "brake_abrasion__2" & 
                   plot.weight$week == 4] = NA

plot.weight$mean[plot.weight$treatment == "brake_abrasion__2" & 
                   plot.weight$week == 8] = NA

# get positions for asterisks:
x.sig4 = c(3, 4, 13, 14)
y.sig4 = weight_agg[weight_agg$week == 4,] %>% group_by(treatment) %>% summarise(max = max(mean, na.rm = TRUE))
y.sig4 = y.sig4[c(2,3,11,12),]
x.sig8 = c(3, 13)
y.sig8 = weight_agg[weight_agg$week == 8,] %>% group_by(treatment) %>% summarise(max = max(mean, na.rm = TRUE))
y.sig8 = y.sig8[c(2, 11),]

# Plot ---------

png("Plots/plot_weight.png", width = 25, height = 12, units = "cm", res = 1000)

par(oma = c(7,1,1,1), mfrow = c(1,3), bty = "l", xpd = TRUE)

boxplot(mean ~ treatment, data = plot.weight[plot.weight$week == 0,], las = 2, 
        xlab = "", ylab = "Mean weight before exposure (g)",
        names = rep("",14), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.weight$treatment))),
        ylim = c(0, 0.9), pch = 20)
mtext(c("Concentration", "0", "", rep(c("0.5","2"), 4), "","0.5","2","8"),at = c(-2, seq(1, 14, 1)), 
      side = 1, line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 2, las = 2)
arrows(c(2, 11), -15, c(2, 11), 0.9, angle = 0, lwd = 1)
mtext("(A)", side = 3, at = 0.7, line = 1)
arrows(0, mean(weight_agg$mean[weight_agg$week == 0]), 16, mean(weight_agg$mean[weight_agg$week == 0]),
       angle = 0, lwd = 1.3, col = "grey70", lty = 2)


boxplot(mean ~ treatment, data = plot.weight[plot.weight$week == 4,], las = 2, 
        xlab = "", ylab = "Mean weight after 4 weeks (g)",
        names = rep("",14), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.weight$treatment))),
        ylim = c(0, 0.9), pch = 20)
points(x = c(4, 4), y = points.BA2.week4, pch = 20)
arrows(4, points.BA2.week4[2] + sd.BA.week4[2], 4, points.BA2.week4[2] - sd.BA.week4[2], angle = 0)# sd of one of the data points
mtext(c("0", "", rep(c("0.5","2"), 4), "","0.5","2","8"),at = c(seq(1, 14, 1)), side = 1, #
      line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 2, las = 2)
arrows(c(2, 11), -15, c(2, 11), 0.9, angle = 0, lwd = 1)
mtext("(B)", side = 3, at = 0.7, line = 1)
arrows(-6, mean(weight_agg$mean[weight_agg$week == 0]), 16, mean(weight_agg$mean[weight_agg$week == 0]),
       angle = 0, lwd = 1.3, col = "grey70", lty = 2)
text(x = x.sig4, y = y.sig4$max, "***", pos = 3, cex = 1.7)
points(13, expected$weight.0.5.4w, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$weight.2.4w, pch = 17, cex = 1.7, col = "darkred")


boxplot(mean ~ treatment, data = plot.weight[plot.weight$week == 8,], las = 2, 
        xlab = "", ylab = "Mean weight after 8 weeks (g)",
        names = rep("",14), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.weight$treatment))),
        ylim = c(0, 0.9), pch = 20)
points(x = 4, y = points.BA2.week8, pch = 20)
mtext(c("0", "", rep(c("0.5","2"), 4), "","0.5","2","8", "Vol%"),at = c(seq(1, 14, 1), 16), 
      side = 1, line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 2, las = 2)
arrows(c(2, 11), -15, c(2, 11), 0.9, angle = 0, lwd = 1)
mtext("(C)", side = 3, at = 0.7, line = 1)
arrows(-6, mean(weight_agg$mean[weight_agg$week == 0]), 16, mean(weight_agg$mean[weight_agg$week == 0]),
       angle = 0, lwd = 1.3, col = "grey70", lty = 2)
text(14, 0.32, "mean", col = "grey70", pos = 4, cex = 0.8)
text(14, 0.25, "before", col = "grey70", pos = 4, cex = 0.8)
text(14, 0.21, "exposure", col = "grey70", pos = 4, cex = 0.8)
text(x = x.sig8, y = y.sig8$max, "***", pos = 3, cex = 1.7)
points(13, expected$weight.0.5.8w, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$weight.2.8w, pch = 17, cex = 1.7, col = "darkred")

dev.off()

### END -----
