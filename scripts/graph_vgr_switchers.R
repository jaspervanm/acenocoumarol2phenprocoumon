qoac <- readRDS("cache/qoac.rds") %>%
	filter_tr() %>%
	melt_qoac("vgr") %>%
	filter(key == "vgr") %>%
	relevel_period()

plot_data <- full_join(
	# baseline
	qoac %>%
		filter(period_id == "acenocoumarol", cc == "case") %>%
		rename(baseline = value) %>%
		select(-period_id),
	# changes
	qoac %>%
		filter(period_id != "acenocoumarol", cc == "case")
) %>%
	group_by(target_range, key, period_id) %>%
	do({
		tibble(
			peso_id  = .$peso_id,
			baseline = .$baseline,
			i        = frank( . , baseline, ties.method = "random"),
			value    = .$value,
			Min      = pmin(baseline, value),
			Max      = pmax(baseline, value),
			increase = .$baseline < .$value
		)
	})

control_data <- qoac %>%
	filter(period_id == "acenocoumarol", cc == "control") %>%
	group_by(target_range) %>%
	summarise(
		pctl = quantile(value, c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE) %>%
			imap_dfr(~tibble(
				i = paste0("p", gsub("%", "", .y)),
				x = .x)
			) %>%
			list()
	) %>%
	unnest() %>%
	group_by(target_range) %>%
	spread(i, x)

plot_data %>%
	semi_join(control_data, by = "target_range") %>%
	ggplot() +
	geom_hline(aes(yintercept = p50, linetype = "median"), data = control_data) +
	geom_hline(aes(yintercept = p25, linetype = "p25 / p75"), data = control_data) +
	geom_hline(aes(yintercept = p75, linetype = "p25 / p75"), data = control_data) +
	geom_hline(aes(yintercept = p5, linetype = "p05 / p95"), data = control_data) +
	geom_hline(aes(yintercept = p95, linetype = "p05 / p95"), data = control_data) +
	geom_linerange(aes(x = i, ymin = Min, ymax = Max, colour = increase)) + 
	geom_point(aes(x = i, y = baseline)) +
	facet_grid(rows = vars(period_id), cols = vars(target_range),
			   scales = "free", space = "free_x") +
	scale_y_log10() +
	ggthemes::theme_few() +
	ggthemes::scale_color_few() +
	theme(axis.title.x = element_blank(),
		  axis.text.x  = element_blank(),
		  axis.ticks.x = element_blank()) +
	labs(y = "Variance Growth Rate")

ggsave("plots/vgr_switchers.png", width = 16, height = 10, dpi = 300)
