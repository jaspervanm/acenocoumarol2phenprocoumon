patient_data <- readRDS("data/patient_data.rds") %>%
	distinct(patient_id, peso_id, center)
eligibility <- readRDS("cache/eligibility_qoac.rds") %>%
	.[sel_random == TRUE] %>%
	merge(patient_data, by = "patient_id", nomatch = NULL)

source("scripts/db_connect.R")

all_followup <- tbl(Data, "inrs") %>%
	inner_join(eligibility, by = c("peso_id", "center"), copy = TRUE) %>%
	group_by(patient_id, peso_id, center, cc) %>%
	summarise(
		nINR = n(),
		fINR = min(inr_date, na.rm = TRUE),
		lINR = max(inr_date, na.rm = TRUE)
	) %>%
	collect()

setDT(all_followup, key = c("patient_id", "center"))

all_followup[ , fu_dur := as.integer(difftime(lINR, fINR, units = "days"))]

all_followup[ , .(
	ninr    = sum(nINR),
	npat    = length(unique(peso_id)),
	nswitch = sum(cc == "case"),
	futot   = sum(fu_dur) / 365.25,
	fumed   = median_iqr(fu_dur, digits = 0),
	fINR    = min(fINR)
), by = "center"] %>%
	saveRDS("cache/summary_followup.rds")
