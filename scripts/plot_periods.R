df <- tribble(
	~from, ~to, ~id, ~drug, ~qoac,
	-350, -180, "switcher", "acenocoumarol", FALSE,
	-180,    0, "switcher", "acenocoumarol", TRUE,
	   0,  360, "switcher", "phenprocoumon", TRUE,
	 360,  420, "switcher", "phenprocoumon", FALSE,
	-280, -180, "non-switcher", "acenocoumarol", FALSE,
	-180, +360, "non-switcher", "acenocoumarol", TRUE,
	+360,  490, "non-switcher", "acenocoumarol", FALSE
)
df$drug <- factor(df$drug, c("phenprocoumon", "acenocoumarol"))

periods <- tribble(
	~from, ~to, ~name,
	-180, 0, "baseline",
	0, 180, "short-term",
	180, 360, "long-term"
)

Plot <- ggplot() +
	geom_linerange(aes(x = id, ymin = from, ymax = to, colour = drug, linetype = qoac), data = df) +
	geom_hline(yintercept = unique(c(periods$from, periods$to)), linetype = "dotted") +
	geom_hline(yintercept = 0) +
	coord_flip() +
	geom_text(aes(x = 2.5, y = (from + to) / 2, label = name), data = periods) +
	ggthemes::theme_few() +
	plot_colours(guide = FALSE) +
	labs(x = "", y = "Time since (virtual) switch, days") +
	scale_y_continuous(breaks = seq(-900, 900, by = 90)) +
	scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = FALSE)

ggsave("plots/periods.png", Plot, width = 7, height = 2.5)
ggsave("plots/periods.eps", Plot, width = 7, height = 2.5, device = cairo_ps)
