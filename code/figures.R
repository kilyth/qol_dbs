#######################################################
### Deep Brain Stimulation: STN vs GPi.
### Figures
### 2023 Katrin Petermann
#######################################################

## colors
## blue, green, red, orange, yellow
cols <- c("#125773", "#368F86", "#F0523F", "#F59E3C", "#F5CF67")


source("./code/geom_flat_violin.r")

######################################
## Figure 1: PDQ subscores
######################################

fig_pdq_subscores <- ggplot(dd_pdq39_change[dd_pdq39_change$target != "all", ], aes(x = subscore, y = score, fill = target))+
        annotate(geom = "rect", ymin = -20, ymax = 50, 
                 xmin = seq(0.5, 8.5, by = 2), 
                 xmax = seq(1.5, 10.5, by = 2),
                 fill = "#F5F5F5")+
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = 0.6)+
        geom_hline(aes(yintercept = 0), lty = 2)+
        scale_fill_manual(values = cols[c(1, 5)])+
        scale_x_discrete(guide = guide_axis(angle = 45))+
        scale_y_continuous(limits = c(-20, 50), breaks = seq(-20, 50, 10))+
        labs(fill = "", y = "Change from Baseline (%)", x = "")+
        theme_pubr()+
        theme(legend.position = "top")

pdf(file = "./figures/fig_pdq_subscores.pdf", width = 5, height = 5)
print(fig_pdq_subscores)
dev.off()

png(file = "./figures/fig_pdq_subscores.png", width = 5, height = 5, 
    units = "in", res = 600)
print(fig_pdq_subscores)
dev.off()

#######################################
### Figure 2: Change Model
#######################################


p1 <- ggplot(data = change_mod_plot, aes(y = coef, x = names, col = relimp))+
  annotate(geom = "rect", ymin = -9.5, ymax = 6.5,
           xmin = seq(0.5, 10.5, by = 2),
           xmax = seq(1.5, 12.5, by = 2),
           fill = "#F5F5F5")+
  geom_hline(yintercept = seq(-8, 6, by = 2), 
             color = "grey", linewidth = 0.05)+
  geom_hline(aes(yintercept = 0), color = "white")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_point(size = 2)+
  geom_segment(aes(x = names, xend = names,
                   y = lower, yend = upper),
               linewidth = 0.7)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  coord_cartesian(ylim = c(-9, 5))+
  labs(x = "", y = "\n Regression Coefficient")+
  scale_y_continuous(breaks = seq(-8, 6, by = 4))+
  scale_color_gradient2(low = cols[5], mid = cols[2], high = cols[1], midpoint = 20)+
  theme_pubr()+
  geom_text(aes(y = -7.5, x = names, label = stars))+
  theme(legend.position = "none")

p2 <- ggplot(data = change_mod_plot, aes(x = names , y = relimp, fill = relimp))+
  geom_hline(yintercept = seq(0, 40, by = 10), 
             color = "grey", linewidth = 0.05, alpha = 0.7)+
  geom_col()+
  scale_fill_gradient2(low = cols[5], mid = cols[2], high = cols[1], midpoint = 20)+
  labs(x = "", y = "Relative \n importance (%)")+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x = element_blank())

pdf(file = "./figures/fig_change_mod.pdf", width = 5, height = 7)
print(ggarrange(p2, p1, ncol = 1, heights = c(0.5, 1)))
dev.off()

png(file = "./figures/fig_change_mod.png", width = 5, height = 7, 
    units = "in", res = 600)
print(ggarrange(p2, p1, ncol = 1, heights = c(0.5, 1)))
dev.off()

#############################################
### Figure 3: PDQ, Postural Stability, MMS
#############################################

fig_pdq_change_all <- ggplot(data = dd_all_wide, aes(y = pdq39_improvement_abs, x = stim_target, fill = stim_target))+
  stat_compare_means()+
  geom_hline(aes(yintercept = 0), col = cols[3], linetype = 2, linewidth = 0.5) +
  geom_boxjitter(jitter.shape = 21, jitter.color = NA,
                 outlier.colour = "transparent", width = 0.4,
                 jitter.params = list(width = 0.05, height = 0.05),
                 jitter.alpha = 0.6, jitter.size = 2,
                 errorbar.draw = T,
                 errorbar.length = 0.2, alpha = 0.8)+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   width = 0.4)+
  scale_y_continuous(limits = c(-35, 35), breaks = seq(-30, 30, len = 5))+
  scale_fill_manual(values = cols[c(1, 5)])+
  labs(x = "", y = "Improvement in \n PDQ-39 SI", fill = "Target")+
  theme_pubr(legend = "none")

fig_pdq_change <- ggplot(data = dd_match, aes(y = pdq39_improvement_abs, x = stim_target, fill = stim_target))+
  stat_compare_means()+
  geom_hline(aes(yintercept = 0), col = cols[3], linetype = 2, linewidth = 0.5) +
  geom_boxjitter(jitter.shape = 21, jitter.color = NA,
                 outlier.colour = "transparent", width = 0.4,
                 jitter.params = list(width = 0.05, height = 0.05),
                 jitter.alpha = 0.6, jitter.size = 2,
                 errorbar.draw = T,
                 errorbar.length = 0.2, alpha = 0.8)+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   width = 0.4)+
  scale_y_continuous(limits = c(-35, 35), breaks = seq(-30, 30, len = 5))+
  scale_fill_manual(values = cols[c(2, 5)])+
  labs(x = "", y = "Improvement in \n PDQ-39 SI", fill = "Target")+
  theme_pubr(legend = "none")

fig_stability_change <- ggplot(data = dd_match, aes(x = stim_target, y = stability_on_change, fill = stim_target))+
  stat_compare_means()+
  geom_hline(aes(yintercept = 0), col = cols[3], linetype = 2, linewidth = 0.5) +
  geom_boxjitter(jitter.shape = 21, jitter.color = NA,
                 outlier.colour = "transparent", width = 0.4,
                 jitter.params = list(width = 0.05, height = 0.05),
                 jitter.alpha = 0.6, jitter.size = 2,
                 errorbar.draw = T,
                 errorbar.length = 0.2, alpha = 0.8)+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   width = 0.4)+
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, len = 5))+
  scale_fill_manual(values = cols[c(2, 5)])+
  labs(x = "", y = "Improvement in \n Postural Stability", fill = "Target")+
  theme_pubr(legend = "none")

fig_mms_change <- ggplot(data = dd_match, aes(x = stim_target, y = mms_change, fill = stim_target))+
  stat_compare_means()+
  geom_hline(aes(yintercept = 0), col = cols[3], linetype = 2, linewidth = 0.5) +
  geom_boxjitter(jitter.shape = 21, jitter.color = NA,
                 outlier.colour = "transparent", width = 0.4,
                 jitter.params = list(width = 0.05, height = 0.05),
                 jitter.alpha = 0.6, jitter.size = 2,
                 errorbar.draw = T,
                 errorbar.length = 0.2, alpha = 0.8)+
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   width = 0.4)+
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, len = 5))+
  scale_fill_manual(values = cols[c(2, 5)])+
  labs(x = "", y = "Improvement in \n MMS", fill = "Target")+
  theme_pubr(legend = "none")

dd_legend <- data.frame(target = c("GPi",
                                   "STN subgroup",
                                   "STN whole cohort"))
fig_legend <- ggplot(data = dd_legend, aes(y = target, fill = target))+
  geom_point(aes(x = 1, y = -1), shape = 15, col = cols[5], size = 9)+
  geom_point(aes(x = 1, y = 0), shape = 15, col = cols[2], size = 9)+
  geom_point(aes(x = 1, y = 1), shape = 15, col = cols[1], size = 9)+
  annotate(geom = "text", x = c(1.5, 1.5, 1.5), y = c(-1, 0, 1), 
           label = c("GPi",
                     "STN subgroup",
                     "STN whole cohort"), 
           size = 4, hjust = 0)+
  lims(x = c(0, 4), y = c(-6, 6))+
  theme_void()+
  theme(legend.position = "n")  

pdf(file = "./figures/fig_matched_change.pdf", width = 12, height = 7)
print(ggarrange(ggarrange(fig_pdq_change_all, fig_pdq_change, fig_stability_change, fig_mms_change, 
                          nrow = 2, ncol = 2, labels = c("A", "B", "C", "D")),
                fig_legend, nrow = 1, ncol = 2, widths = c(1, 0.3), align = "v"))
dev.off()

png(file = "./figures/fig_matched_change.png", width = 12, height = 7, 
    units = "in", res = 600)
print(ggarrange(ggarrange(fig_pdq_change_all, fig_pdq_change, fig_stability_change, fig_mms_change, 
                          nrow = 2, ncol = 2, labels = c("A", "B", "C", "D")),
                fig_legend, nrow = 1, ncol = 2, widths = c(1, 0.3), align = "v"))
dev.off()

######################################################
### Supplementary Figure 1: % Change in all variables
######################################################

fig_perc_change_allvars <- ggplot(subset(dd_allpat_allvars_change, (perc_change >=-100) & (target != "all")), aes(x = scale, y = perc_change, col = target))+
  annotate(geom = "rect", ymin = -105, ymax = 105, 
           xmin = seq(0.5, 10.5, by = 2), 
           xmax = seq(1.5, 12.5, by = 2),
           fill = "#F5F5F5")+
  geom_bar(data = dd_allvars_change[dd_allvars_change$target != "all", ], aes(x = subscore, y = score, fill = target),
           position = "dodge",
           stat = "identity",
           width = 0.4)+
  geom_point(position = position_jitterdodge(
    jitter.width = 0.2,
    dodge.width = 1.2), size = 1.2, shape = 16, alpha = 0.5)+
  geom_hline(aes(yintercept = 0), lty = 2)+
  scale_color_manual(values = cols[c(1, 5)])+
  scale_fill_manual(values = cols[c(1, 5)])+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  coord_cartesian(ylim = c(-100, 100))+
  scale_y_continuous(breaks = seq(-100, 100, 20))+
  labs(fill = "", col = "", y = "Improvement (%)", x = "")+
  theme_pubr()+
  theme(legend.position = "right")

pdf(file = "./figures/fig_perc_change_allvars.pdf", width = 12, height = 5)
print(fig_perc_change_allvars)
dev.off()

png(file = "./figures/fig_perc_change_allvars.png", width = 12, height = 5, 
    units = "in", res = 600)
print(fig_perc_change_allvars)
dev.off()