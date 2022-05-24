# Import data
repro <- read.csv2("Data/reproduction.csv", stringsAsFactors = TRUE)
str(repro)

# Import model results:
results_repro <- readRDS("Results/repro.RDS")

# Import expected multiple stressor effects
expected <- readRDS("Results/expected.RDS")


# Check treatment levels and sort:
levels(repro$treatment)
repro$treatment <- factor(repro$treatment, levels = c("control_0", "brake_abrasion_0.5", "brake_abrasion_2",
                                                      "exhaust_particles_0.5", "exhaust_particles_2", "MP_fibers_0.5", "MP_fibers_2", 
                                                      "MP_particles_0.5", "MP_particles_2", "mix_0.5", "mix_2", "mix_8"))

# set palette
colors.Holzinger <- c("#CC6677", "#882255","#eeeeee", "#eeeeee", "#eeeeee", "#44AA99", "#117733","#bbbbbb", "#666666", "#333333", "#DDCC77", "#999933", "#88CCEE", "#6699CC")
scales::show_col(colors.Holzinger)
palette(colors.Holzinger)


#create dummy treatments as place holders for plotting:
dummies <- data.frame(
  Pollutant = NA,
  Concentration = NA,
  Glass = NA,
  juvenile = NA,
  cocoons = NA,
  treatment = c("d1", "d2")
)
plot.repro = rbind(dummies, repro)

# put dummies to correct place:
plot.repro$treatment <- factor(plot.repro$treatment, levels = c("control_0", "d1",
                                                              "brake_abrasion_0.5", "brake_abrasion_2",
                                                              "exhaust_particles_0.5", "exhaust_particles_2", 
                                                              "MP_fibers_0.5", "MP_fibers_2", 
                                                              "MP_particles_0.5", "MP_particles_2", 
                                                              "d2",
                                                              "mix_0.5", "mix_2", "mix_8"))

# get positions of significant results:
summary(results_repro$coc_result)
summary(results_repro$juv_result)

# get positions for asterisks:
x.sig = c(3, 4, 5, 6, 7, 9, 12, 13, 14)
y.sig = repro %>% group_by(treatment) %>% summarise(coc = max(cocoons), juv = max(juvenile))
y.sig = y.sig[c(2,3,4,5,6,8,10,11,12),]

# horizontal lines at mean control:
lines = repro[repro$treatment == "control_0",] %>% 
  summarise(meanjuv = mean(juvenile, na.rm = T), meancoc = mean(cocoons, na.rm = T))

# plot -------

#png("Plots/plot_repro.png", width = 25, height = 15, units = "cm", res = 500)
tiff("Plots/plot_repro.tiff", height = 15, width = 25, units = "cm", res = 500)
opar <- par(oma = c(5,1,1,1), mfrow = c(1,2), bty = "l", xpd = TRUE)

boxplot(cocoons ~treatment, data = plot.repro, las = 2, xlab = "", ylab = "Number of coccoons", 
        names = rep("",14), xaxt = "n", pch = 20,
        col = as.numeric(as.factor(levels(plot.repro$treatment))))
mtext(c("Concentration", "0", "", rep(c("0.5","2"), 4), "","0.5","2","8"),at = c(-2.5, seq(1, 14, 1)), side = 1, line = 0, cex = 0.8)
mtext(c("Control", "Brake dust", "Carbon black", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 1.5, las = 2)
arrows(c(2, 11), -15, c(2, 11), 52, angle = 0, lwd = 1.3)
text(x = x.sig, y = y.sig$coc, "***", pos = 3)
mtext("(A)", side = 3, at = 0.7, line = 1)
arrows(0, lines$meancoc, 14, lines$meancoc,  angle = 0, lwd = 1.3, col = "grey70", lty = 2)
points(13, expected$coc.0.5, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$coc.2, pch = 17, cex = 1.7, col = "darkred")

boxplot(juvenile ~treatment, data = plot.repro, las = 2, xlab = "", ylab = "Number of juveniles", 
        names = rep("",14), xaxt = "n", pch = 20,
        col = as.numeric(as.factor(levels(plot.repro$treatment))))
mtext(c("0", "", rep(c("0.5","2"), 4), "","0.5","2","8", "Vol%"),at = c(seq(1, 14, 1), 16), side = 1, line = 0, cex = 0.8)
mtext(c("Control", "Brake dust", "Carbon black", "MP fibers", "MP fragments", "Mix"), at = c(1, 3.5, 5.5, 7.5, 9.5, 13), 
      side = 1, line = 1.5, las = 2)
arrows(c(2, 11), -15, c(2, 11), 52, angle = 0, lwd = 1.3)
text(x = x.sig, y = y.sig$juv, "***", pos = 3)
mtext("(B)", side = 3, at = 0.7, line = 1)
arrows(0, lines$meanjuv, 14, lines$meanjuv,  angle = 0, lwd = 1.3, col = "grey70", lty = 2)
points(13, expected$juv.0.5, pch = 17, cex = 1.7, col = "darkred")
points(14, expected$juv.2, pch = 17, cex = 1.7, col = "darkred")

par(opar)

dev.off()
### END--------