# Mode <- "calc"
Mode <- commandArgs(TRUE)

if(Mode == "calc") {
	matched_ids <- readRDS("cache/matched_qoac.rds") %>%
		distinct(patient_id, switch_date, cc, target_range_lower, target_range_upper, target_range)
	setDT(matched_ids)
	matched_ids[ , merge_from := switch_date - 180 - 42 ]
	matched_ids[ , merge_to   := switch_date + 360 ]
	
	all_INRs <- readRDS("data/INR_data.rds")
	setDT(all_INRs, key = "patient_id")
	
	INRs <- all_INRs[ matched_ids,
				   .( patient_id, switch_date, cc,
				      target_range_lower, target_range_upper, target_range,
				      INR_date = as.integer(difftime(x.inr_date, switch_date, units = "days")), INR = inr ),
				   on = .(patient_id, inr_date >= merge_from, inr_date <= merge_to )]
	
	calc_qoac <- function(Data) {
		all_interpolated_inrs <- approx(Data$INR_date, Data$INR, seq(-180, 360)) %>%
			as.data.table()
		
		map_dfr(plot_times, function(i) {
			INR_date <- Data[INR_date <= i, .(max(INR_date))][[1]]
			int_inrs <- all_interpolated_inrs[between(x, INR_date - 180, INR_date), y]
			
			vgr <- Data[between(INR_date, i - 180, i), mean(diff(INR)^2 / as.numeric(diff(INR_date)))]
			
			tibble(
				cf_time     = i,
				from        = INR_date - 180,
				to          = INR_date,
				above_range = mean(int_inrs > Data[[1, "target_range_upper"]], na.rm = TRUE),
				below_range = mean(int_inrs < Data[[1, "target_range_lower"]], na.rm = TRUE),
				in_range    = 1 - above_range - below_range,
				vgr         = vgr
			)
		})
	}
	
	plot_times <- seq(0, 380, by = 10)
	
	qoac <- INRs[ , calc_qoac(.SD), by = c("patient_id", "target_range", "cc")]
	
	saveRDS(qoac, "cache/qoac_over_time.rds")
	
} else if(Mode == "plot") {
	
	plot_data <- readRDS("cache/qoac_over_time.rds") %>%
		left_join(
			readRDS("data/subgroups.rds")
		) %>%
		gather("key", "value", above_range, below_range, in_range, vgr) %>%
		mutate(CC = factor(pretty_names[cc], levels = pretty_names[c("case", "control")])) %>%
		group_by(subgroup, key, target_range, CC, cc, cf_time) %>%
		summarise(
			est  = median(value, na.rm = TRUE),
			low  = quantile(value, 0.25, na.rm = TRUE),
			high = quantile(value, 0.75, na.rm = TRUE)
		)
	plot_data$Key <- factor(pretty_names[plot_data$key], levels = pretty_names[c("in_range", "below_range", "above_range", "vgr")])
	
	check_periods <- data.frame(x = c(180, 360))
	
	#----- For ALL observations (i.e. subgroup == "all") ----
	ttr_plot <- plot_data %>%
		filter(key != "vgr", subgroup == "all") %>%
		ggplot(aes(x = cf_time, y = est, colour = CC, group = cc)) +
		geom_path() +
		geom_path(aes(y = low), linetype = "dotted") +
		geom_path(aes(y = high), linetype = "dotted") +
		facet_grid(vars(target_range), vars(Key)) +
		geom_vline(aes(xintercept = x), data = check_periods, linetype = "dashed", colour = "grey") +
		ggthemes::theme_few() +
		plot_colours() +
		theme(
			legend.title      = element_blank(),
			legend.background = element_rect(fill = "transparent"),
			legend.position   = c(0.11,0.96)
		) +
		scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
		labs(
			x = "Time (days)",
			y = "Proportion of time"
		)

	vgr_plot <- plot_data %>%
		filter(key == "vgr", subgroup == "all") %>%
		ggplot(aes(x = cf_time, y = est, colour = CC, group = cc)) +
		geom_path() +
		geom_path(aes(y = low), linetype = "dotted") +
		geom_path(aes(y = high), linetype = "dotted") +
		facet_grid(vars(target_range), vars(Key)) +
		geom_vline(aes(xintercept = x), data = check_periods, linetype = "dashed", colour = "grey") +
		ggthemes::theme_few() +
		plot_colours() +
		theme(
			legend.position = "none"
		) +
		scale_y_log10() +
		labs(
			x = "Time (days)",
			y = "INR variability"
		)
	
	combined_plot <- ggpubr::ggarrange(ttr_plot, vgr_plot, nrow = 1, widths = c(5,2))
	ggsave("plots/matched_development_all.png", combined_plot, width = 10, height = 6)
	ggsave("plots/matched_development_all.eps", combined_plot, width = 10, height = 6, device = cairo_ps)
	
	#---- For subgroups ----
	sg_ttr_plot <- plot_data %>%
		filter(key != "vgr", subgroup != "all") %>%
		ggplot(aes(x = cf_time, y = est, colour = subgroup, linetype = cc, group = paste(subgroup, CC))) +
		geom_path() +
		facet_grid(vars(target_range), vars(key)) +
		geom_vline(aes(xintercept = x), data = check_periods, linetype = "dashed", colour = "grey") +
		ggthemes::theme_few() +
		plot_colours() +
		scale_linetype_manual(values = c(case = "solid", control = "dashed"), guide = "none", name = "Subgroup") +
		theme(
			legend.background = element_rect(fill = "transparent"),
			legend.direction  = "vertical",
			legend.position   = c(0.08,0.90)
		) +
		scale_y_continuous(labels = scales::percent, lim = c(0,1)) +
		labs(
			x = "Time (days)",
			y = "Proportion of time"
		)
	
	sg_vgr_plot <- plot_data %>%
		filter(key == "vgr", subgroup != "all") %>%
		ggplot(aes(x = cf_time, y = est, colour = subgroup, linetype = cc, group = paste(subgroup, CC))) +
		geom_path() +
		facet_grid(vars(target_range), vars(key)) +
		geom_vline(aes(xintercept = x), data = check_periods, linetype = "dashed", colour = "grey") +
		ggthemes::theme_few() +
		plot_colours() +
		scale_linetype_manual(values = c(case = "solid", control = "dashed"), guide = "none") +
		theme(
			legend.position = "none"
		) +
		scale_y_log10() +
		labs(
			x = "Time (days)",
			y = "INR variability"
		)
	
	combined_sg_plot <- ggpubr::ggarrange(sg_ttr_plot, sg_vgr_plot, nrow = 1, widths = c(5,2))
	ggsave("plots/matched_development_subgroups.png", combined_sg_plot, width = 10, height = 6, scale = 1.5)
	ggsave("plots/matched_development_subgroups.eps", combined_sg_plot, width = 10, height = 6, scale = 1.5, device = cairo_ps)
}
