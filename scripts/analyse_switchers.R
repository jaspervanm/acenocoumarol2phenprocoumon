case_qoac <- readRDS("cache/qoac.rds") %>%
	filter(cc == "case") %>%
	filter_tr()

case_doses <- readRDS("data/all_doses.rds") %>%
	inner_join(
		case_qoac %>%
			distinct(patient_id, target_range),
		by = "patient_id") %>%
	mutate(
		doseratio_acenocoumarol = 1L,
		`doseratio_short-term`  = (`dos_short-term` / dos_acenocoumarol) * 100,
		`doseratio_long-term`   = (`dos_long-term`  / dos_acenocoumarol) * 100
	) %>%
	as.data.table()

qoac <- inner_join(
	case_qoac,
	case_doses %>%
		select(patient_id, contains("dos_")) %>%
		gather(period_id, dose, -patient_id) %>%
		mutate(
			period_id = gsub("dos_", "", period_id)
		)
) %>%
	inner_join(
		case_doses %>%
			select(patient_id, contains("doseratio_")) %>%
			gather(period_id, doseratio, -patient_id) %>%
			mutate(
				period_id = gsub("doseratio_", "", period_id)
			)
	) %>%
	melt_qoac(c("vgr", "mean_inr", "dose", "doseratio", "tbi")) %>%
	inner_join(
		readRDS("data/subgroups.rds") %>%
			select(patient_id, cc, subgroup)
	) %>%
	mutate(
		value = value * ifelse(key %in% c("in_range", "above_range", "below_range"), 100, 1)
	) %>%
	select(period_id, target_range, subgroup, key, cc, patient_id, value, switch_date, center) %>%
	group_by(target_range, subgroup, period_id, key, cc) %>%
	nest()

qoac %>%
	filter(subgroup == "all") %>%
	summarise_iqr_wilcox() %>%
	ungroup() %>%
	select(target_range, period_id, key, Print) %>%
	within({
		Print[key %in% c("doseratio", "in_range", "above_range", "below_range")]  <- gsub(" \\[", "% [", Print[key %in% c("doseratio", "in_range", "above_range", "below_range")])
	}) %>%
	spread(period_id, Print) %>%
	within({
		key <- as.character(key)
		acenocoumarol[key == "doseratio"] <- "ref"
		i   <- pretty_order[key]
		key <- pretty_names[key]
	}) %>%
	arrange(target_range, i) %>%
	select(
		`Target range` = target_range,
		`Parameter` = key,
		`Before switch` = acenocoumarol,
		`Short-term` = `short-term`,
		`Long-term` = `long-term`
	) %>%
	knitr::kable("markdown") %>%
	write("tables/switchers_qoac.md")

qoac %>%
	filter(subgroup != "all", key %in% c("dose", "doseratio", "tbi")) %>%
	group_by(subgroup) %>%
	do(summarise_iqr_wilcox(.)) %>%
	select(target_range, subgroup, period_id, key, Print) %>%
	within({
		Print[key == "doseratio"]  <- gsub(" \\[", "% [", Print[key == "doseratio"])
	}) %>%
	spread(period_id, Print) %>%
	within({
		key <- as.character(key)
		acenocoumarol[key == "doseratio"] <- "ref"
		i   <- pretty_order[key]
		key <- pretty_names[key]
	}) %>%
	arrange(target_range, subgroup, i) %>%
	select(
		`Target range` = target_range,
		`Subgroup` = subgroup,
		`Parameter` = key,
		`Before switch` = acenocoumarol,
		`Short-term` = `short-term`,
		`Long-term` = `long-term`
	) %>%
	knitr::kable("markdown") %>%
	write("tables/switchers_qoac_subgroups.md")

case_doses %>%
	group_by(target_range) %>%
	summarise(
		ratio_st = median_iqr(`doseratio_short-term`, na.rm = TRUE, digits = 0),
		ratio_lt = median_iqr(`doseratio_long-term`, na.rm = TRUE, digits = 0)
	) %>%
	saveRDS("cache/dose_switchers_summary.rds")

cor_data <- full_join(
	case_qoac,
	case_doses %>%
		select(patient_id, contains("dos_")) %>%
		gather(period_id, dose, -patient_id) %>%
		mutate(period_id = gsub("dos_", "", period_id)),
	by = c("patient_id", "period_id")
) %>%
	select(patient_id, target_range, center, period_id,
		   in_range, above_range, below_range, vgr, dose, tbi) %>%
	gather(key, value, -patient_id, -target_range, -center, -period_id) %>%
	spread(period_id, value)
setDT(cor_data)

testCor <- function(x, y) {
	z <- cbind(x, y)
	i <- complete.cases(z)

	out <- DescTools::SpearmanRho( x[i], y[i], conf.level = 0.95) %>%
		rbind.data.frame()
	colnames(out) <- c("estimate", "conf.low", "conf.high")
	out
}

correlations <- cor_data[ !is.na(acenocoumarol), .(
	`short-term` = .(testCor(acenocoumarol, `short-term`)),
	`long-term`  = .(testCor(acenocoumarol, `long-term`))
), by = c("target_range", "key")] %>%
	gather(period_id, value, contains("-term")) %>%
	unnest(value) %>%
	mutate(Rho = est_ci( . , CI_text = "95% CI ")) %>%
	arrange(key, desc(period_id), target_range)

saveRDS(correlations, "cache/switchers_correlations.rds")
