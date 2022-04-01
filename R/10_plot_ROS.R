# Load packages
library(dplyr)

# Import data:
ros <- read.csv2("Data/ros.csv", stringsAsFactors = TRUE)
#str(ros)

# Import regression model output:
results.ros <- readRDS("Results/ROS.RDS")

# Import expected multiple stressor effects: 
expected <- readRDS("Results/expected.RDS")


# set palette
colors.Holzinger.reduced <- c("#CC6677", "#eeeeee", "#eeeeee", "#eeeeee",  "#44AA99", "#117733", "#bbbbbb", "#666666", "#DDCC77", "#999933", "#88CCEE", "#6699CC")
scales::show_col(colors.Holzinger.reduced)
palette(colors.Holzinger.reduced)

# Remove unnecessary columns:
ros <- ros[,-c(1:3,7,8)]
str(ros)

# Create dummy treatments as place holders for plotting:
dummies <- data.frame(
  CAT = NA,
  GST = NA,
  LPO = NA,
  treatment = as.factor(c("d1", "d2"))
)
plot.ROS = rbind(dummies, ros)

# Put dummies to correct place:
plot.ROS$treatment <- factor(plot.ROS$treatment, levels = c("control__0", "d1",
                                                              "break_abrasion__0.5",
                                                              "exhaust_particles__0.5", "exhaust_particles__2", 
                                                              "MP_fibers__0.5", "MP_fibers__2", 
                                                              "MP_particles__0.5", "MP_particles__2", 
                                                              "d2",
                                                              "mix__0.5", "mix__2"))

# Get positions for asterisks:
summary(results.ros$CAT)
x.sigCAT <- c(3, 4, 9, 11, 12) # "**", "*", "*", "**", "*" (starts with 3, because vertical line takes column2 in the plot)
summary(results.ros$GST) 
x.sigGST <- c(5, 11) #"*", "*"
summary(results.ros$LPO)
x.sigLPO <- c(6, 8) # "*","**"


y.sig = plot.ROS[-c(1:2),] %>% group_by(treatment) %>% summarise(maxCAT = max(CAT, na.rm = T), maxGST = max(GST, na.rm = T), maxLPO = max(LPO, na.rm = T))
y.sigCAT <- y.sig$maxCAT[c(2, 3, 8, 9, 10)]
y.sigGST <- y.sig$maxGST[c(4,9)]
y.sigLPO <- y.sig$maxLPO[c(4, 7)]

# horizontal lines at mean control:
lines = plot.ROS[plot.ROS$treatment == "control__0",] %>% summarise(meanCAT = mean(CAT, na.rm = T), meanGST = mean(GST, na.rm = T), meanLPO = mean(LPO, na.rm = T))


#plot----------------
png("Plots/plot_ROS.png", width = 25, height = 12, units = "cm", res = 1000)

op <- par(oma = c(7,1.5,1,1), mfrow = c(1, 3), bty = "l", xpd = TRUE)

boxplot(CAT ~ treatment, data = plot.ROS, las = 2,
        xlab = "", ylab = "CAT activity (U/mg protein)",
        names = rep("",12), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.ROS$treatment))),
        ylim = c(0, 35), pch = 20)
mtext(c("Concentration", "0", "", "0.5", rep(c("0.5","2"), 3), "","0.5","2", "8"),at = c(-2, seq(1, 13, 1)), side = 1, line = 0, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3, 4.5, 6.5, 8.5, 12), 
      side = 1, line = 1.5, las = 2)
arrows(c(2, 10), -20, c(2, 10), 35, angle = 0, lwd = 1.3)
text(x = x.sigCAT, y = y.sigCAT, c("**", "*", "*", "**", "*"), pos = 3, cex = 1.7)
mtext("(A)", side = 3, at = 0.7, line = 1)
arrows(0, lines$meanCAT, 13, lines$meanCAT,  angle = 0, lwd = 1.3, col = "grey70", lty = 2)
points(12, expected$CAT.0.5, pch = 17, cex = 1.7, col = "darkred")
points(13, expected$CAT.2, pch = 17, cex = 1.7, col = "darkred")


boxplot(GST ~ treatment, data = plot.ROS, las = 2,
        xlab = "", ylab = "GST activity (nmol/min/mg protein)",
        names = rep("",12), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.ROS$treatment))),
        ylim = c(-60, 130), pch = 20)
mtext(c("0", "", "0.5", rep(c("0.5","2"), 3), "","0.5","2", "8"),at = c(seq(1, 13, 1)), side = 1, 
      line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3, 4.5, 6.5, 8.5, 12), 
      side = 1, line = 2, las = 2)
arrows(c(2, 10), -200, c(2, 10), 130, angle = 0, lwd = 1)
text(x = x.sigGST, y = y.sigGST, "*", pos = 3, cex = 1.7)
mtext("(B)", side = 3, at = 0.7, line = 1)
arrows(0, lines$meanGST, 13, lines$meanGST,  angle = 0, lwd = 1.3, col = "grey70", lty = 2)
points(12, expected$GST.0.5, pch = 17, cex = 1.7, col = "darkred")
points(13, expected$GST.2, pch = 17, cex = 1.7, col = "darkred")

boxplot(LPO ~ treatment, data = plot.ROS, las = 2,
        xlab = "", ylab = "MDA content (U/mg protein)",
        names = rep("",12), xaxt = "n",
        col = as.numeric(as.factor(levels(plot.ROS$treatment))),
        ylim = c(0, 120), pch = 20)
mtext(c("0", "", "0.5", rep(c("0.5","2"), 3), "","0.5","2","8", "Vol%"),at = c(seq(1, 13, 1), 14), 
      side = 1, line = 0.5, cex = 0.7)
mtext(c("Control", "Brake dust", "Soot", "MP fibers", "MP fragments", "Mix"), at = c(1, 3, 4.5, 6.5, 8.5, 12), 
      side = 1, line = 2, las = 2)
arrows(c(2, 10), -100, c(2, 10), 120, angle = 0, lwd = 1)
text(x = x.sigLPO, y = y.sigLPO, c("*", "**"), pos = 3, cex = 1.7)
mtext("(C)", side = 3, at = 0.7, line = 1)
arrows(0, lines$meanLPO, 13, lines$meanLPO,  angle = 0, lwd = 1.3, col = "grey70", lty = 2)
points(12, expected$LPO.0.5, pch = 17, cex = 1.7, col = "darkred")
points(13, expected$LPO.2, pch = 17, cex = 1.7, col = "darkred")
par(op)

dev.off()

#### END--------

