TR <- commandArgs(TRUE)

plot_data <- readRDS("cache/qoac_over_time.rds") %>%
	filter(target_range == gsub("-", " - ", TR)) %>%
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

ttr_plot <- plot_data %>%
	filter(key != "vgr", subgroup == "all") %>%
	ggplot(aes(x = cf_time, y = est, colour = CC, group = cc)) +
	geom_path() +
	geom_path(aes(y = low), linetype = "dotted") +
	geom_path(aes(y = high), linetype = "dotted") +
	facet_grid(cols = vars(Key)) +
	geom_vline(aes(xintercept = x), data = check_periods, linetype = "dashed", colour = "grey") +
	ggthemes::theme_few() +
	plot_colours() +
	theme(
		legend.title      = element_blank(),
		legend.background = element_rect(fill = "transparent"),
		legend.position   = c(0.11,0.88)
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
	facet_grid(cols = vars(Key)) +
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
ggsave(paste0("plots/qoac_over_time-", TR, ".png"), combined_plot, width = 7.5, height = 2.1, scale = 1.2)
ggsave(paste0("plots/qoac_over_time-", TR, ".eps"), combined_plot, width = 7.5, height = 2.1, device = cairo_ps, scale = 1.2)
