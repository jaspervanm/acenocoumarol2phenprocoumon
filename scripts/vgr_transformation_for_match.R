patient_data <- readRDS("data/patient_data.rds") %>%
	inner_join(
		readRDS("data/all_doses.rds") %>% select(peso_id, switch_date, dos_acenocoumarol),
		by = c("peso_id", "switch_date")
	)
setDT(patient_data, key = c("peso_id", "switch_date"))

all_qoac <- readRDS("cache/qoac.rds")
qoac <- all_qoac %>%
	filter_tr() %>%
	as.data.table(key = "period_id") %>%
	.[ period_id == "acenocoumarol", .(peso_id, switch_date, below_range, above_range, vgr, mean_inr, target_range)] %>%
	inner_join(patient_data) %>%
	mutate(lvgr = log(vgr)) %>% # is ook gedaan in what_to_match_on.R
	filter(!is.na(lvgr), is.finite(lvgr), vgr != 0) %>%
	drop_na() %>%
	mutate(
		case = (cc == "case")
	) %>%
	group_by(target_range) %>%
	nest()

qoac$vgr_raw <- map(qoac$data, ~glm(case ~ age + gender + VKA_exp +
										above_range + below_range + mean_inr + vgr +
										ind_VTE + ind_AF + ind_MVR,
									family = binomial(),
									data = .x))
qoac$vgr_log <- map(qoac$data, ~glm(case ~ age + gender + VKA_exp +
								   	above_range + below_range + mean_inr + I(log10(vgr)) +
								   	ind_VTE + ind_AF + ind_MVR,
								   family = binomial(),
								   data = .x))
qoac$vgr_ln <- map(qoac$data, ~glm(case ~ age + gender + VKA_exp +
										above_range + below_range + mean_inr + I(log(vgr)) +
										ind_VTE + ind_AF + ind_MVR,
									family = binomial(),
									data = .x))
qoac$vgr_sqrt <- map(qoac$data, ~glm(case ~ age + gender + VKA_exp +
									 	above_range + below_range + mean_inr + I(sqrt(vgr)) +
									 	ind_VTE + ind_AF + ind_MVR,
									 family = binomial(),
									 data = .x))

pmap_df(qoac, function(...) {
	x <- list(...)
	
	tibble(
		target_range = x$target_range,
		vgr_raw = AIC(x$vgr_raw),
		vgr_ln = AIC(x$vgr_ln),
		vgr_log = AIC(x$vgr_log),
		vgr_sqrt = AIC(x$vgr_sqrt)
	)
})
