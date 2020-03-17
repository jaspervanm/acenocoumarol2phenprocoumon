source("renv/activate.R")
suppressPackageStartupMessages({
	suppressWarnings({
		library(stats)
		library(tidyverse)
		library(data.table)
		library(JasperTools)
	})
})

pretty_names <- c(
	case = "switchers",
	control = "non-switchers",
	in_range = "TTR",
	above_range = "above range",
	below_range = "below range",
	vgr = "INR variability",
	log_vgr = "INR variability*",
	mean_inr = "mean INR",
	dose = "dose",
	doseratio = "dose ratio",
	tbi = "mean number of days between INRs",
	all = "all patients",
	elderly = "the elderly",
	valve = "mechanical valve replacement",
	low_dose = "acenocoumarol dose <1.5mg",
	poor_ttr = "TTR <60%",
	volatile = "INR variability >75th percentile"
)
pretty_order <- set_names(seq_along(pretty_names), names(pretty_names))
plot_colours <- function(alpha = 1, ...) {
	discrete_scale("colour", "plos", scales::manual_pal(c("#f63c3d", "#32373c", "#00397e", "#17bafa", "#d2da20", "#006164")), ...)
}

And <- function(x) {
	if(length(x) == 1) {
		x
	} else {
		paste(paste(x[-length(x)], collapse =", "), x[length(x)], sep = " and ")
	}
}

give_patient_id <- function(Data) {
	within(Data, {
		patient_id <- paste0(substr(center, 1, 1), peso_id)
	})
}

percentilise <- function(case_values, control_values = case_values) {
	# controls <- control_values[!is.na(control_values)]
	# map_dbl(case_values, ~mean(.x >= controls)) * 100
	control_pctl <- quantile(control_values, (0:99)/100, na.rm = TRUE)
	map_dbl(case_values, ~sum(.x > control_pctl))
}

filter_tr <- function(data, r = c("2 - 3.5", "2 - 3", "2.5 - 3.5")) {
	if(!("target_range") %in% colnames(data)) {
		data$target_range <- paste(data$target_range_lower, data$target_range_upper, sep = " - ")
	}
	if(!is.data.table(data)) {
		setDT(data)
	}
	data[data$target_range %in% r, ]
}

melt_qoac <- function(data, extra_vars = NA, drop = TRUE) {
	vars <- na.omit(c("in_range", "above_range", "below_range", extra_vars))

	x <- gather(data, key, value, !!vars) %>%
		mutate(
			key = factor(key, vars, vars)
		)

	if(drop == TRUE) {
		i <- intersect(
			c("patient_id", "peso_id", "switch_date", "period_id", "key", "value", "target_range", "target_range_lower", "target_range_upper", "cc", "center"),
			colnames(x)
		)
		x[ , i ]
	} else {
		x
	}
}

diff_qoac <- function(Data, parameters = "value") {
	x <- select(Data, one_of( intersect(
		c("patient_id", "target_range", "subgroup", "center", "period_id", "key", "value", parameters),
		colnames(Data) ) )
	)

	if(parameters != "value") {
		x <- gather(x, key, value, !! parameters)
	}

	within(x, {
			value[key == "vgr"] <- log(value[key == "vgr"])
		}) %>%
		group_by(center, patient_id, key) %>%
		spread(period_id, value) %>%
		mutate(
			ds = `short-term` - acenocoumarol,
			dl = `long-term`  - acenocoumarol
		)
}

relevel_period <- function(data) {
	data$period_id <- factor(data$period_id, c("acenocoumarol", "short-term", "long-term"))
	data
}

summarise_qoac <- function(data) {
	# assumes a melt_qoac'ed data.frame that is relevantly grouped
	data %>%
		summarise(
			Mean = mean(value, na.rm = TRUE),
			Sd   = sd(value, na.rm = TRUE),
			Se   = Sd / sqrt(n()),
			conf.low = Mean + qt(0.025, n() - 1) * Se,
			conf.high = Mean + qt(0.975, n() - 1) * Se
		)
}

summarise_iqr_wilcox <- function(x) {

	setDT(x)
	x[ , digits := 0]
	x[key %in% c("dose", "mean_inr"), digits := 1]
	x[key == "vgr", digits := 2]

	tab <- expand.grid(
		target_range = unique(x$target_range),
		periods      = intersect(
			c("short-term", "long-term"),
			x$period_id
		),
		key          = unique(x$key),
		stringsAsFactors = FALSE
	)

	tab$p <- pmap_dbl( tab , function(...) {
		args <- list(...)

		y <- x %>%
			filter(target_range == !! args$target_range,
				   key == !! args$key,
				   period_id %in% c("acenocoumarol", args$periods)) %>%
			unnest(data) %>%
			select(patient_id, switch_date, period_id, value) %>%
			spread(period_id, value) %>%
			drop_na()

		wilcox.test(x = y$acenocoumarol, y = unlist(y[, args$periods]), paired = TRUE, exact = FALSE)$p.value
	})

	x$IQR <- map2_chr(x$data, x$digits,
					  ~JasperTools::median_iqr(.x$value, na.rm = TRUE, digits = .y) )

	full_join(x, tab, by = c(period_id = "periods", "key", "target_range")) %>%
		within({
			Print <- IQR
			Print[!is.na(p)] <- paste0(IQR[!is.na(p)], ", p = ", Hmisc::format.pval(p[!is.na(p)], digits = 3, eps = 0.001))
		})
}
