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
plot_subgroup <- function(Data, key) {
	Data <- Data %>%
		filter(key == !! key)
	all_data <- filter(Data, subgroup == "all")
	
	Plot <- ggplot(Data, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = Subgroup, colour = factor(subgroup, rev(levels(subgroup))))) +
		# Show vertical line at x = 0
		geom_vline(xintercept = 0, linetype = "solid", colour = "#666666") +
		# Show actual points
		geom_point() +
		geom_errorbarh(height = 0.25) +
		# Indicate area of "all"
		geom_rect(ymin = 0 - 2.5, ymax = length(subgroup_names) + 0.5 + 2.5,
				  fill = "grey", colour = "transparent", alpha = 0.2,
				  data = all_data) +
		geom_vline(aes(xintercept = estimate), linetype = "dashed", colour = "#333333",
				   data = all_data) +
		# Label favourable direction
		geom_text(x = 0, label = ifelse(key == "log_vgr", "Switch better ", "Switch worse "), y = length(subgroup_names) + 0.5,
				  hjust = "right", vjust = "top", size = 2.5, colour = "black",
				  data = all_data) +
		geom_text(x = 0, label = ifelse(key == "log_vgr", " Switch worse", " Switch better"), y = length(subgroup_names) + 0.5,
				  hjust = "left", vjust = "top", size = 2.5, colour = "black",
				  data = all_data) +
		# Other options
		facet_grid(vars(period_id), vars(target_range)) +
		ggthemes::theme_few() +
		theme(legend.position = "none", title = element_text(size = 10)) +
		plot_colours() +
		labs(
			x = "Difference from matched non-switchers, estimate (95% CI)",
			y = "",
			title = paste(ifelse(key == "log_vgr", "INR variability", "TTR"), "in subgroups, compared with matched non-switching controls")
		) +
		lims(x = c(
			min(Data$conf.low) - 0.1 * abs(min(Data$conf.low)),
			max(Data$conf.high) + 0.2 * abs(max(Data$conf.high))
		))
	
	if(key == "log_vgr") {
		Plot
	} else {
		Plot + scale_x_continuous(labels = scales::percent)
	}
}

plots <- ggpubr::ggarrange(
	plot_subgroup(matched_data, "in_range"),
	plot_subgroup(matched_data, "log_vgr"),
	nrow = 2, ncol = 1
)
ggsave("plots/subgroup_forest.png", plots, width = 10, height = 8.5)
ggsave("plots/subgroup_forest.eps", plots, width = 10, height = 8.5, device = cairo_ps)

#---- Quantify heterogeneity ----
ma_data <- matched_data %>%
	rename(var = key) %>%
	select(target_range, subgroup, period_id, N, var, estimate, std.error) %>%
	filter( var %in% c("in_range", "log_vgr", "above_range", "below_range")) %>%
	arrange(target_range, desc(period_id), var)
setDT(ma_data)

ma_data[ subgroup %in% c("poor_ttr", "low_dose"), sg_type := "prompt"]
ma_data[ subgroup %in% c("valve", "elderly"),     sg_type := "clinical"]

do_meta <- function(Data, By) {
	Dat <- copy(Data)
	
	BY <- str_to_upper(By)
	walk(By, ~set(Dat, NULL, str_to_upper(.x), Dat[ , ...x]))
	
	x <- Dat[ ,
			  .(mobj = .(metafor::rma(yi = estimate, sei = std.error,
			  						weights = N, slab = paste(target_range, subgroup, period_id, var),
			  						data = .SD))),
			  by = BY]
	x[ , I2  := map_dbl(mobj, "I2")]
	x[ , QEp := map_dbl(mobj, "QEp")]
	x[ , est := map_dbl(mobj, "b", 1)]
	x[ , conf.low  := map_dbl(mobj, "ci.lb")]
	x[ , conf.high := map_dbl(mobj, "ci.ub")]
	colnames(x)[seq_len(length(By))] <- By
	copy(x)
}

meta_prompt <- do_meta( ma_data[sg_type == "prompt" & subgroup != "all"],
						c("target_range", "period_id", "var"))
meta_clinic <- do_meta( ma_data[sg_type == "clinical" | subgroup == "all"],
						c("target_range", "period_id", "var"))

list(
	prompt = meta_prompt,
	clinic = meta_clinic
) %>%
	saveRDS("cache/matched_heterogeneity.rds")
