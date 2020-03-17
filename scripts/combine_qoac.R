# qoac_files <- list.files("cache/splitted_qoac/", "qoac_[0-9]+\\.rds", full.names = TRUE)
qoac_files <- commandArgs(TRUE)
patient_data <- readRDS("data/patient_data.rds")
all_qoac   <- map_dfr(qoac_files, readRDS) %>%
	left_join(
		patient_data %>%
			distinct(patient_id, center, peso_id, cc),
		by = c("patient_id", "cc")
		) %>%
	mutate( tbi = timespan / (nINR - 1)) %>%
	filter( nINR > 1, timespan > 0, tbi <= 56) %>%
	mutate(period_id = factor(period_id, levels = c("acenocoumarol", "short-term", "long-term"))) %>%
	arrange(patient_id, cc, period_id)
saveRDS(all_qoac, "cache/qoac.rds")

all_qoac %>%
	filter(cc == "case") %>%
	saveRDS("cache/switchers_qoac.rds")

qoac_tr_splits <- all_qoac %>%
	filter_tr() %>%
	as.data.table(key = "target_range") %>%
	split(by = "target_range")

iwalk(qoac_tr_splits, ~saveRDS(.x, paste0("cache/qoac-tr_", gsub(" ", "", .y), ".rds")))
