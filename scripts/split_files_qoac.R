n_sets <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])

eligibility <- readRDS("cache/eligibility_qoac.rds")[ sel_random == TRUE ]

patient_data <- readRDS("data/patient_data.rds") %>%
	select(patient_id, switch_date, contains("target_range"), cc) %>%
	semi_join(eligibility, by = c("patient_id", "switch_date"))
setDT(patient_data, key = c("patient_id"))

INR_data <- readRDS("data/INR_data.rds")[ eligibility, .(
	peso_id, center,
	inr_date = as.integer(difftime(inr_date, switch_date, units = "days")),
	inr      = inr
), by = .EACHI, on = "patient_id"] %>%
	.[between(inr_date, -180 - 60, 360 + 60)]

split_sets <- INR_data[ , .(N = .N), by = "patient_id"]
split_sets$cum <- cumsum(split_sets$N)
split_sets$set <- split_sets$cum %/% ceiling(split_sets[[nrow(split_sets), "cum"]] / n_sets)

iwalk( split(split_sets$patient_id, split_sets$set), function(pids, set_nr) {
	x <- data.table(patient_id = pids, key = "patient_id")
	list(
		patient_data = patient_data[x, on = "patient_id", nomatch = NULL],
		INR_data     = INR_data[x, on = "patient_id", nomatch = NULL]
	) %>%
		saveRDS(paste0("cache/splitted_inrs/split_", (as.integer(set_nr) + 1), ".rds"))
})
