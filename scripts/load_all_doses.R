source("scripts/db_connect.R")

pat_data     <- readRDS("data/patient_data.rds") %>%
	distinct(patient_id, peso_id, center)
dose_periods <- readRDS("cache/eligibility_qoac.rds") %>% # required to select correct switch_date
	filter(sel_random == TRUE) %>%
	select(patient_id, switch_date) %>%
	left_join(pat_data, by = "patient_id") %>%
	mutate(
		start = switch_date - 180 - 25,
		stop  = switch_date + 360
	) %>%
	as.data.table(key = c("patient_id", "peso_id", "switch_date"))

#--- Get doses ----
doses <- tbl(Data, "doses") %>%
	right_join(dose_periods, by = c("center", "peso_id"), copy = TRUE) %>%
	group_by(center, peso_id) %>%
	filter(start_date >= start, start_date <= stop) %>%
	ungroup() %>%
	select(-start, -stop) %>%
	collect()
setDT(doses, key = "patient_id")

library(furrr)
plan(multiprocess)

dose_periods$dos <- future_map2(dose_periods$patient_id, dose_periods$switch_date, function(i, x, doses) {
	dos <- doses[patient_id == i] %>%
		separate(doses, into = as.character(seq(0, 24)), sep = "_") %>%
		gather(tt, dose, -peso_id, -center, -patient_id, -drug, -start_date) %>%
		filter(!is.na(dose)) %>%
		mutate(
			dose_date = start_date + as.integer(tt),
			dose = as.numeric(dose),
			start_date = NULL,
			tt = NULL
		)
	setDT(dos, key = "dose_date")
	
	map_df(x, ~tibble(
		`dos_acenocoumarol` = mean(dos[dose_date >= .x - 180 & dose_date <= .x]$dose),
		`dos_short-term`    = mean(dos[dose_date >= .x & dose_date <= .x + 180]$dose),
		`dos_long-term`     = mean(dos[dose_date >= .x + 180 & dose_date <= .x + 360]$dose)
	))
}, doses, .progress = TRUE)

unnest(dose_periods, dos) %>%
	filter(!is.na(dos_acenocoumarol)) %>%
	select(-start, -stop) %>%
	saveRDS("data/all_doses.rds")
