source("scripts/db_connect.R")

#---- Identify cases ----
drug_periods <- "SELECT t.* FROM treatment_periods t
RIGHT JOIN (
		SELECT DISTINCT peso_id, center FROM treatment_periods WHERE drug IN ('acenocoumarol', 'fenprocoumon') GROUP BY peso_id, center HAVING COUNT(DISTINCT drug) > 1
	) AS subset
	ON t.peso_id = subset.peso_id AND t.center = subset.center" %>%
	dbGetQuery(Data, . ) %>%
	arrange(peso_id, from_date) %>%
	filter(span > 180) %>%
	as.data.table(key = "peso_id")
n_inrs <- tbl(Data, "inrs") %>%
	semi_join(drug_periods, by = c("peso_id", "tx_set", "center"), copy = TRUE) %>%
	group_by(peso_id, tx_set, center) %>%
	summarise(N = n()) %>%
	ungroup() %>%
	collect()
drug_periods <- inner_join(drug_periods, n_inrs, by = c("peso_id", "tx_set", "center")) %>%
	mutate(target_range = paste(target_range_lower, target_range_upper, sep = " - ")) %>%
	filter(target_range %in% c("2 - 3", "2 - 3.5", "2.5 - 3.5"))
setDT(drug_periods, key = c("peso_id", "center"))

# nu hebben we een dataset met van iedere combinatie streef/medicijn de periode
# nu nog uitzoeken welke 2 periodes we dan gaan uitzoeken;
# bij voorkeur de eerste switch van A naar F, en dan met dezelfde streef.

# dus drug[i] == "acenocoumarol" & drug[i + 1] == "fenprocoumon"
# en target_range_lower[i] == target_range_lower[i + 1]
# en target_range_upper[i] == target_range_upper[i + 1]
# en span[i] > 180 & span[i + 1] > 180
# en N[i] >= 5 & N[i + 1] >= 5

xA <- drug_periods[drug == "acenocoumarol" & N >= 5]
yA <- drug_periods[drug == "fenprocoumon" & N >= 5][ , tx_set := tx_set -1 ]

selected_sets <- yA[ xA ,
					 on = .(peso_id, center,
					 	   target_range_lower = target_range_lower,
					 	   target_range_upper = target_range_upper,
					 	   tx_set = tx_set # +1 staat hierboven
					 ), 
					 nomatch = 0
					 ] %>%
	do({
		x <- .
		bind_rows(
			x %>%
				select(peso_id, center, from_date = i.from_date, to_date = i.to_date, drug = i.drug, contains("target_range_"), tx_set, span = i.span),
			x %>%
				select(peso_id, center, from_date, to_date, drug, contains("target_range_"), tx_set, span) %>%
				mutate(tx_set = tx_set + 1)
		)
	}) %>%
	arrange(peso_id, center, tx_set)

selected_sets <- full_join(
	selected_sets,
	selected_sets %>% filter(drug == "acenocoumarol") %>% select(peso_id, switch_date = to_date, center),
	by = c("peso_id", "center")
)
setDT(selected_sets, key = "peso_id")

#---- Identify controls ----
# We are going to fetch data from controls to see how our switchers score
# compared with the rest of the cohort
# 
# this should be patients on acenocoumarol, matched only on target range

controls <- list()
controls$periods <- tbl(Data, "treatment_periods") %>%
	filter(drug == "acenocoumarol", span > 2*180) %>%
	anti_join(selected_sets, by = c("peso_id", "center"), copy = TRUE) %>%
	semi_join(selected_sets, by = c("target_range_lower", "target_range_upper"), copy = TRUE) %>%
	collect()
setDT(controls$periods, key = c("peso_id", "center"))

# bepaal welke periode controls minimaal moeten hebben
selected_sets[drug == "acenocoumarol", control_to_date   := switch_date + 180]
selected_sets[drug == "acenocoumarol", control_from_date := switch_date - 180]

controls$matches <- selected_sets[drug == "acenocoumarol"][
	controls$periods,
	.(case_id = x.peso_id, control_id = i.peso_id, control_tx_set = i.tx_set, switch_control_date = x.switch_date, target_range_lower, target_range_upper, center),
	on = .( target_range_lower, target_range_upper, control_from_date >= from_date, control_to_date <= to_date, center ),
	mult = "all", nomatch = 0, allow.cartesian = TRUE
]

selected_sets$control_to_date <- NULL

#---- Fetch data ----
cases_and_controls <- rbind(
	selected_sets %>%
		select(peso_id, center, tx_set, switch_date, drug, contains("target_range")) %>%
		mutate(cc = "case"),
	controls$matches %>%
		select(peso_id = control_id, center, tx_set = control_tx_set, switch_date = switch_control_date, contains("target_range")) %>%
		mutate(cc = "control", drug = "acenocoumarol") %>%
		as.data.frame()
) %>%
	as.data.table()

db_data <- list(
	patients = tbl(Data, "patients") %>%
		right_join(cases_and_controls %>%
				   	distinct(center, peso_id, cc),
				   by = c("center", "peso_id"), copy = TRUE) %>%
		collect(),
	indications = tbl(Data, "indications") %>%
		semi_join(cases_and_controls %>%
				  	distinct(center, peso_id),
				  by = c("center", "peso_id"), copy = TRUE) %>%
		collect() %>%
		mutate(
			indication_from = as.Date(indication_from),
			indication_to   = as.Date(indication_to)
		) %>%
		data.table(key = c("center", "peso_id")),
	inrs = tbl(Data, "inrs") %>%
		semi_join(cases_and_controls %>%
				  	distinct(center, peso_id, tx_set),
				  by = c("center", "peso_id", "tx_set"),
				  copy = TRUE) %>%
		distinct(center,peso_id, inr_date, inr, tx_set) %>%
		arrange(center, peso_id, inr_date) %>%
		collect() %>%
		data.table(key = c("center", "peso_id"))
)

db_data$indications[is.na(indication_to), indication_to := lubridate::now()]

indications <- db_data$indications[
	cases_and_controls,
	.(center, peso_id, switch_date, indication_cat, indication, indication_code),
	on = .(center, peso_id == peso_id, indication_from <= switch_date, indication_to >= switch_date),
	mult = "all", allow.cartesian = TRUE]
setkey(indications, center, peso_id, switch_date, indication_cat)

indications <- indications[
	, .(
		ind_VTE = ("VTE" %in% indication_cat),
		ind_AF  = ("AF" %in% indication_cat),
		ind_MVR = ("mechanical valve" %in% indication_cat)
	), by = .(center, peso_id, switch_date)
][ ind_VTE | ind_AF | ind_MVR ]

# Save INR data separately from patient data,
# since one patient can be control for multiple cases
# and otherwise this would create multiple copies of the same data

patient_data <- cases_and_controls %>%
	distinct(center, peso_id, switch_date, target_range_lower, target_range_upper, cc) %>%
	inner_join(
		db_data$patients,
		by = c("center", "peso_id", "cc")
	) %>%
	inner_join(indications, by = c("center", "peso_id", "switch_date")) %>%
	mutate(
		age = as.integer(age - (2018 - lubridate::year(switch_date))),
		VKA_exp = difftime(switch_date, registration_date, units = "days") %>%
			as.integer() %>%
			`/`(365.25) %>% # in years
			as.integer(),
		peso_id = as.integer(peso_id)
	) %>%
	filter(age >= 18) # only adults
setDT(patient_data, key = c("center", "peso_id", "switch_date"))

INR_data <- db_data$inrs %>%
	select(peso_id, inr_date, inr) %>%
	left_join(db_data$patients %>% select(center, peso_id, registration_date), by = c("peso_id")) %>%
	filter(inr_date >= registration_date + 90) %>% # exclude first 90 days, they are always unstable
	select(-registration_date) %>%
	mutate(peso_id = as.integer(peso_id)) %>%
	semi_join(patient_data, by = c("center", "peso_id")) # to remove patients aged <18
setDT(INR_data, key = c("peso_id", "inr_date"))	

# Save data
patient_data %>%
	select(-registration_date) %>%
	give_patient_id() %>%
	saveRDS("data/patient_data.rds")
INR_data  %>%
	give_patient_id() %>%
	saveRDS("data/INR_data.rds")

