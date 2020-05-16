matched_data <- readRDS("cache/matched_analyses.rds")
subgroup_names <- c("all", "low_dose", "poor_ttr", "volatile", "elderly", "valve")
matched_data[ , subgroup := factor(subgroup, levels = rev(subgroup_names))]
matched_data[ , Subgroup := factor(subgroup, levels = levels(subgroup), labels = pretty_names[levels(subgroup)])]

#---- Considerations before analysing ----
# We want to evaluate heterogeneity in the effect within groups of special interest
# These groups were chosen because they:
# - have a different prompt to switch, i.e. a low dose or a poor TTR
# - require special clinical consideration, because they have other options (e.g. the elderly of patients with MVR)

# In the first category, we want to compare the two specials groups with each other
# to see which strategy works best.
# In the second category, we compare the subgroups with the overall image
# to see whether they have the same results.

#---- Create plots for visual inspection ----
plot_subgroup <- function(Data, key, target_range) {
	Data <- Data %>%
		filter(key == !! key, target_range == !! target_range)
	all_data <- filter(Data, subgroup == "all", target_range == !! target_range)
	n_subgroup <- length(unique(Data$Subgroup))
	
	Plot <- ggplot(Data, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Subgroup, colour = factor(subgroup, rev(levels(subgroup))))) +
		# Show vertical line at x = 0
		geom_vline(xintercept = 0, linetype = "solid", colour = "#666666") +
		# Show actual points
		geom_point() +
		geom_errorbarh(height = 0.25) +
		# Indicate area of "all"
		geom_rect(ymin = 0 - 2.5, ymax = n_subgroup + 0.5 + 2.5,
				  fill = "grey", colour = "transparent", alpha = 0.2,
				  data = all_data) +
		geom_vline(aes(xintercept = estimate), linetype = "dashed", colour = "#333333",
				   data = all_data) +
		# Label favourable direction
		geom_text(x = 0, label = ifelse(key == "log_vgr", "Switch better ", "Switch worse "), y = n_subgroup + 0.5,
				  hjust = "right", vjust = "top", size = 2.5, colour = "black",
				  data = all_data) +
		geom_text(x = 0, label = ifelse(key == "log_vgr", " Switch worse", " Switch better"), y = n_subgroup + 0.5,
				  hjust = "left", vjust = "top", size = 2.5, colour = "black",
				  data = all_data) +
		# Other options
		facet_grid(vars(period_id), vars(Key)) +
		ggthemes::theme_few() +
		theme(legend.position = "none", title = element_text(size = 10), strip.text.x = element_text(hjust = 0)) +
		plot_colours() +
		labs(
			x = "Difference with matched controls, estimate (95% CI)",
			y = ""
		) +
		lims(x = c(
			min(Data$conf.low) - 0.1 * abs(min(Data$conf.low)),
			max(Data$conf.high) + 0.2 * abs(max(Data$conf.high))
		))
	
	if(key == "log_vgr") {
		Plot +
			theme(
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank()
			)
	} else {
		Plot +
			scale_x_continuous(labels = scales::percent) +
			theme(
				strip.text.y = element_blank()
			)
	}
}

library(patchwork)
plots <- map(unique(matched_data$target_range), function(target_range) {
	wrap_elements(grid::textGrob(x = 0, hjust = 0, gp = grid::gpar(cex = 1.2),
								 label = paste("Effect of a switch to phenprocoumon in subgroups, target range", target_range))) /
		(plot_subgroup(matched_data, "in_range", target_range) +
		 	plot_subgroup(matched_data, "log_vgr", target_range)) +
		plot_layout(height = unit(c(0.8, 1), c("cm", "null")))
})  
names(plots) <- gsub(" ", "", unique(matched_data$target_range))

iwalk(plots, function(x, tr) {
	ggsave(paste0("plots/subgroup_forest-", tr, ".png"), x, width = 5.2, height = 3, scale = 2)
	ggsave(paste0("plots/subgroup_forest-", tr, ".eps"), x, width = 5.2, height = 3, device = cairo_ps, scale = 2)
})
