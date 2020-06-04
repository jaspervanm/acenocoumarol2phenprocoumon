mqoac <- readRDS("cache/matched_qoac.rds") %>%
	inner_join(
		readRDS("data/all_doses.rds") %>%
			gather(period_id, dose, contains("dos_")) %>%
			mutate(
				period_id = gsub("dos_", "", period_id)
			)
	) %>%
	filter(subgroup == "all")

x <- mqoac %>%
	mutate(pid = frank( map_chr(patient_id, digest::digest), ties.method = "dense")) %>%
	select(subgroup, patient_id = pid, target_range, cc, set,
		   propensity_score = ps,
		   period_id, in_range, vgr, above_range, below_range, mean_inr, dose, tbi) %>%
	arrange(subgroup, patient_id, target_range, period_id)

write_csv2(x, "data/matched_qoac_export.csv")
