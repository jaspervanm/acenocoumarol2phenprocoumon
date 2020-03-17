source("scripts/db_connect.R")

matched_ids <- readRDS("cache/close_matches_qoac.rds") %>%
	mutate(
		extr_from = switch_date - 180,
		extr_to   = switch_date + 360
	) %>%
	distinct(peso_id, switch_date, set, extr_from, extr_to, cc)

doses <- tbl(Data, "doses") %>%
	inner_join(matched_ids, by = "peso_id", copy = TRUE) %>%
	group_by(peso_id) %>%
	filter(dose_date >= extr_from, dose_date <= extr_to) %>%
	ungroup() %>%
	select(peso_id, switch_date, drug, drug_specific, dose_date, dose, set, cc) %>%
	collect() %>%
	mutate(
		set = as.integer(set),
		peso_id = as.integer(peso_id)
	)

saveRDS(doses, "cache/doses.rds")
