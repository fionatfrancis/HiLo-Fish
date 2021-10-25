#Current Speed Boxplot (currently Fig 3 in ResultsSection.doc)
ggplot(Abiotic_byday, aes(x = Site, y = SpeedMax)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.95, hjust = 0.9),
        legend.position = "bottom") +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  labs(y = "Daily Max Current Speed (cm/sec)", x = "Site", fill = "Site Current Speed Category")

#Richness, Abundance, Biomass by Current Speed (currently Fig 4 in ResultsSection.doc)
(AbundCurr <- ggplot(transectdata, aes(y = Abundance, x = CurrentMax, col = Richness)) +
    theme_cowplot() +
    theme(panel.border = element_rect(color = "black"),
          strip.background = element_rect(colour = "black"),
          panel.spacing = unit(1.5, "lines"),
          legend.justification = "top") +
    facet_wrap(~ TransectDepth, labeller = labeller(TransectDepth = depthlabel)) +
    geom_point(size = 2) +
    labs(y = "Fish Abundance", x = "") +
    scale_color_distiller(palette = "Greys", direction = 1) +
    scale_x_continuous(breaks = seq(0, 120, 20)))
(BioCurr <- ggplot(transectdata, aes(y = Biomass_kg, x = CurrentMax)) +
    theme_cowplot() +
    theme(panel.border = element_rect(color = "black"),
          strip.background = element_rect(colour = "black"),
          panel.spacing = unit(1.5, "lines"),
          strip.text.x = element_blank()) +
    facet_wrap(~ TransectDepth) +
    geom_point(size = 2) +
    labs(y = "Fish Biomass (kg)", x = "Site Average Daily Maximum Current Speed (cm/sec)") +
    scale_color_distiller(palette = "Greys", direction = 1) +
    scale_x_continuous(breaks = seq(0, 120, 20)))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- get_legend(AbundCurr)
AbundCurr <- AbundCurr + theme(legend.position = "none")

(CurrPlots <- grid.arrange(AbundCurr, mylegend, BioCurr, ncol = 2, nrow = 2, heights = c(1,1), widths = c(1, 0.15)))
