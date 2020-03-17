pat_data <- readRDS("data/patient_data.rds") %>%
	mutate(
		switch_date     = as.IDate(switch_date),
		merge_from_date = switch_date - 180 - 60, # 60 marge
		merge_to_date   = switch_date + 180 + 60,
		target_range    = paste(target_range_lower, target_range_upper, sep = " - ")
	) %>%
	select(patient_id, cc, switch_date, target_range, merge_from_date, merge_to_date)
setDT(pat_data, key = c("patient_id", "switch_date"))
pat_data[ , split_id := seq(1, .N) %/% 10000 ]

INR_data <- readRDS("data/INR_data.rds") %>%
	as.data.table(key = c("patient_id", "inr_date")) %>%
	.[ , .(inr = mean(inr)), by = .(patient_id, inr_date)]
INR_data[ , inr_date := as.IDate(inr_date)]
INR_data[ , inr_merge_date := inr_date ]

library(furrr)
plan(multiprocess)

eligibility <- future_map_dfr(split(pat_data, by = "split_id"), function(i, INR_data) {
	INR_data[ i,
			  .(switch_date, cc, target_range,
			    nINRs = .N,
			    before_switch = as.integer(difftime(switch_date, min(inr_date), units = "days")),
			    after_switch  = as.integer(difftime(max(inr_date), switch_date, units = "days")),
			    maxdiff = as.integer(max(diff(inr_date)))),
		  on = .(patient_id, inr_merge_date >= merge_from_date, inr_merge_date <= merge_to_date),
		  by = .EACHI, allow.cartesian = TRUE] 
 
}, INR_data, .progress = TRUE)

eligibility[ , inr_merge_date := NULL]

saveRDS(eligibility, "cache/eligibility_data.rds")
