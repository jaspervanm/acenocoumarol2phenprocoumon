library(lme4)
library(lmerTest)
p_lmer <- possibly(lmerTest::lmer, NA)

mqoac <- readRDS("cache/matched_qoac.rds") %>%
	select(-timespan, -nINR) %>%
	filter_tr() %>%
	inner_join(
		readRDS("data/all_doses.rds") %>%
			gather(period_id, dose, contains("dos_")) %>%
			mutate(
				period_id = gsub("dos_", "", period_id),
				dose     = log(dose)
				# later on: log(X) - log(Y) = log(X / Y)
			)
	) %>%
	mutate(
		period_id    = factor(period_id, c("acenocoumarol", "short-term", "long-term")),
		cc           = factor(cc, levels = c("control", "case")), # control first, is the reference here
		log_vgr      = log(vgr),
		log_tbi      = log(tbi)
	) %>%
	within({
		log_tbi[period_id == "acenocoumarol"] <- 0
		# not relevant for the analysis
	}) %>%
	filter(is.finite(log_vgr)) %>%
	select(-target_range_lower, -target_range_upper, -vgr, -tbi) %>%
	group_by(target_range, subgroup) %>%
	melt_qoac(c("log_vgr", "mean_inr", "dose", "log_tbi" ), drop = FALSE) %>%
	mutate(key = as.character(key)) %>%
	ungroup() %>%
	spread(period_id, value)
setDT(mqoac, key = c("subgroup", "peso_id", "key"))

mqoac$ds <- mqoac$`short-term` - mqoac$acenocoumarol
mqoac$dl <- mqoac$`long-term` - mqoac$acenocoumarol

reg_data <- mqoac %>%
	group_by(subgroup, target_range, key) %>%
	nest() %>%
	mutate(
		model     = vector("character", n()),
		mod_short = vector("list", n()),
		mod_long  = vector("list", n())
	)
setDT(reg_data)

reg_data[ , model := "linear"]

reg_data[model == "linear", mod_short := map(data, ~p_lmer(ds ~ cc + (1 | set), .x))]
reg_data[model == "linear", mod_long  := map(data, ~p_lmer(dl ~ cc + (1 | set), .x))]
reg_data[model == "linear", short     := map(mod_short, broom.mixed::tidy, conf.int = TRUE, effects = "fixed")]
reg_data[model == "linear", long      := map(mod_long,  broom.mixed::tidy, conf.int = TRUE, effects = "fixed")]

matched_data <- bind_rows(
	reg_data %>%
		mutate(N = map_int(data, ~sum(!is.na(.x$`short-term`)))) %>%
		select(target_range, subgroup, N, key, short, model) %>%
		unnest(short) %>%
		mutate(period_id = "short-term"),
	reg_data %>%
		mutate(N = map_int(data, ~sum(!is.na(.x$`long-term`)))) %>%
		select(target_range, subgroup, N, key, long, model) %>%
		unnest(long) %>%
		mutate(period_id = "long-term")
) %>%
	relevel_period() %>%
	filter(term != "(Intercept)") %>%
	ungroup()
setDT(matched_data)

matched_data[ key == "dose" | key == "log_tbi",
			  c("estimate", "conf.low", "conf.high") := .(exp(estimate), exp(conf.low), exp(conf.high)) ]
matched_data[ key == "dose",    key := "doseratio" ]
matched_data[ key == "log_tbi", key := "tbi" ]
matched_data[ , c("i", "Key") := .(pretty_order[key], pretty_names[key]) ]

saveRDS(matched_data, "cache/matched_analyses.rds", compress = FALSE)

matched_data[ key %in% c("in_range", "above_range", "below_range"),
			  c("estimate", "conf.low", "conf.high", "digits") := .(100 * estimate, 100 * conf.low, 100 * conf.high, 1)]
matched_data[ key %in% c("log_vgr", "dose_ratio"), digits := 2]

matched_data %>%
	filter(subgroup == "all", 
		   key %in% c("in_range", "above_range", "below_range", "log_vgr")) %>%
	mutate(
		Print = paste0(est_ci( . , digits = digits), ", p=", format_number(p.value))
	) %>%
	within({
		Print[key %in% c("above_range", "below_range", "in_range")] <- gsub(" \\(", "pp (", Print[key %in% c("above_range", "below_range", "in_range")])
	}) %>%
	select(target_range, Key, i, period_id, Print) %>%
	group_by(target_range, Key) %>%
	spread(period_id, Print) %>%
	arrange(target_range, i) %>%
	select( target_range,
		   `Variable` = Key,
		   `Short-term difference` = `short-term`,
		   `Long-term difference` = `long-term`) %>%
	split( . , .$target_range) %>%
	iwalk(function(tab, tr) {
		tr <- gsub(" ", "", tr)
		tab %>%
			ungroup() %>%
			select(-target_range) %>%
			knitr::kable("markdown") %>%
			write(paste0("tables/matched-", tr, ".md"))
	})

matched_data %>%
	filter(subgroup != "all",
		   key %in% c("in_range", "above_range", "below_range", "log_vgr")) %>%
	mutate(
		Print = est_ci(.)
	) %>%
	select(target_range, subgroup, Key, period_id, Print, i) %>%
	group_by(target_range, subgroup, Key) %>%
	spread(period_id, Print) %>%
	arrange(target_range, subgroup, i) %>%
	select(`Target range` = target_range,
		   `Subgroup` = subgroup,
		   `Variable` = Key,
		   `Short-term difference` = `short-term`,
		   `Long-term difference` = `long-term`) %>%
	knitr::kable("markdown",
				 caption = "QOAC in switchers versus non-switchers (ergo positive number = switchers higher") %>%
	write("tables/matched_subgroups.md")

#---- Dichotomised data ----
# Assess whether patients achieve a good TTR
# Report as absolute risk difference

good_cutoff <- 0.65
good_ttr <- reg_data %>%
	ungroup() %>%
	filter(key == "in_range") %>%
	select(subgroup, target_range, data) %>%
	unnest(data) %>%
	select(subgroup, target_range, patient_id, cc, `short-term`, `long-term`) %>%
	gather("period_id", "ttr", `short-term`, `long-term`) %>%
	nest(patient_id, cc, ttr) %>%
	mutate(good_cutoff = !! good_cutoff)
setDT(good_ttr)
good_ttr[ , tab := map(data, ~with(.x, table(cc, ttr >= good_cutoff)))]
good_ttr[ , ard := map(tab, function(tab) {
	prop.test(tab[2:1, 2:1], correct = FALSE) %>%
		broom::tidy() %>%
		mutate(estimate = estimate1 - estimate2)
}) ]

good_ttr %>%
	select(-data, -tab) %>%
	unnest(ard) %>%
	arrange(subgroup, target_range, desc(period_id)) %>%
	saveRDS("cache/matched_ard_good.rds")
