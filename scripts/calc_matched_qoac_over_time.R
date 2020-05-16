matched_ids <- readRDS("cache/matched_qoac.rds") %>%
	distinct(patient_id, switch_date, cc, target_range_lower, target_range_upper, target_range)
setDT(matched_ids)
matched_ids[ , merge_from := switch_date - 180 - 42 ]
matched_ids[ , merge_to   := switch_date + 360 ]

all_INRs <- readRDS("data/INR_data.rds")
setDT(all_INRs, key = "patient_id")

INRs <- all_INRs[ matched_ids,
				  .( patient_id, switch_date, cc,
				     target_range_lower, target_range_upper, target_range,
				     INR_date = as.integer(difftime(x.inr_date, switch_date, units = "days")), INR = inr ),
				  on = .(patient_id, inr_date >= merge_from, inr_date <= merge_to )]

calc_qoac <- function(Data) {
	all_interpolated_inrs <- approx(Data$INR_date, Data$INR, seq(-180, 360)) %>%
		as.data.table()
	
	map_dfr(plot_times, function(i) {
		INR_date <- Data[INR_date <= i, .(max(INR_date))][[1]]
		int_inrs <- all_interpolated_inrs[between(x, INR_date - 180, INR_date), y]
		
		vgr <- Data[between(INR_date, i - 180, i), mean(diff(INR)^2 / as.numeric(diff(INR_date)))]
		
		tibble(
			cf_time     = i,
			from        = INR_date - 180,
			to          = INR_date,
			above_range = mean(int_inrs > Data[[1, "target_range_upper"]], na.rm = TRUE),
			below_range = mean(int_inrs < Data[[1, "target_range_lower"]], na.rm = TRUE),
			in_range    = 1 - above_range - below_range,
			vgr         = vgr
		)
	})
}

plot_times <- seq(0, 380, by = 10)

qoac <- INRs[ , calc_qoac(.SD), by = c("patient_id", "target_range", "cc")]

saveRDS(qoac, "cache/qoac_over_time.rds")
