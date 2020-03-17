el <- readRDS("cache/eligibility_qoac.rds")
el[ , suff_data := (suff_inrs & suff_time & cont_inrs)]
#---- Load data ----
em <- merge(
	el %>%
		# select only one relevant instance per patient_id and target_range
		# we need to have the ones that go down the flowchart the furthest:
		# so sort by random selection and enough data
		.[order(patient_id, -sel_random, -suff_data)] %>%
		.[!duplicated(paste(patient_id, target_range))],
	readRDS("cache/matched_qoac.rds") %>%
		filter(subgroup == "all") %>%
		distinct(patient_id, switch_date) %>%
		mutate(matched = TRUE),
	by = c("patient_id", "switch_date"),
	all = TRUE
) %>%
	merge(
		readRDS("data/all_doses.rds") %>%
			mutate(has_dose = as.logical(dos_acenocoumarol)) %>%
			select(patient_id, switch_date, has_dose),
		by = c("patient_id", "switch_date"),
		all = TRUE
	)

em[is.na(matched), matched := FALSE]
em[is.na(has_dose), has_dose := FALSE]

#---- Calculate components of flowchart ----
make_flow_data <- function(em, by = c("cc", "target_range")) {
	flow_items <- list()
	
	# Potential inclusions
	flow_items$pot_incl <- em[ , .(
		Npat  = length(unique(patient_id)),
		Ninst = .N
	), by = c(by)]
	
	# Exclude insufficient data
	flow_items$suff_data <- em[ , .(
		Npat  = length(unique(patient_id)),
		Ninst = .N
	), by = c(by, "suff_data")]
	
	# Exclude double instances
	flow_items$random <- em[ suff_data == TRUE,
							 .(
							 	Npat  = length(unique(patient_id)),
							 	Ninst = .N
							 ), by = c(by, "sel_random")]
	
	# Exclude patients without dose
	flow_items$dose <- em[ sel_random == TRUE,
						   .(Npat  = .N),
						   by = c(by, "has_dose")]
	
	# Exclude non-matched controls
	flow_items$matched <- em[ sel_random == TRUE & has_dose == TRUE,
							  .(Npat  = .N),
							  by = c(by, "matched")]
	
	flow_df <- map_df(flow_items, function(x) {
		i       <- which(map_lgl(x, is.logical))
		x$crit <- colnames(x)[i]
		colnames(x)[i] <- "crit_value"
		x
	})
	setDT(flow_df)
	flow_df[is.na(crit), crit := "all"]
	
	flow_df
}

save_flow <- function(Data, filename) {
	Data %>%
		arrange_( . , intersect(c("target_range", "cc", "Npat"), colnames( Data ))) %>%
		as.data.table() %>%
		saveRDS(filename)
}

save_flow(make_flow_data(em, by = c("cc", "target_range")), "cache/flow_cc_tr.rds")
save_flow(make_flow_data(em, by = "target_range"), "cache/flow_tr.rds")
save_flow(make_flow_data(em, by = "cc"), "cache/flow_cc.rds")

flow_df <- make_flow_data(em, by = c("cc", "target_range"))

#---- Making labels for plot ----
flow_df[ , Nfpat  := format(Npat,  big.mark = ",")]
flow_df[ , Nfinst := format(Ninst, big.mark = ",")]

flow_df[ crit == "all",
		 label := paste(Nfpat, "potential patients")]
flow_df[ crit == "suff_data" & crit_value == FALSE,
		 label := paste(Nfpat, "patients had insufficient data")]
flow_df[ crit == "suff_data" & crit_value == TRUE,
		 label := paste(Nfpat, "patients had sufficient data")]
flow_df[ crit == "sel_random" & crit_value == FALSE,
		 label := paste(Nfpat, "patients randomly removed")]
flow_df[ crit == "sel_random" & crit_value == TRUE,
		 label := paste(Nfpat, "patients included")]
flow_df[ crit == "has_dose" & crit_value == FALSE,
		 label := paste(Nfpat, "patients had no dose information")]
flow_df[ crit == "has_dose" & crit_value == TRUE,
		 label := paste(Nfpat, "patients had dose information")]
flow_df[ crit == "matched" & crit_value == FALSE,
		 label := paste(Nfpat, "patients were not matched")]
flow_df[ crit == "matched" & crit_value == TRUE,
		 label := paste(Nfpat, "patients were matched")]

flow_df[ , label := str_wrap(label, width = 40)]

levels(flow_df$cc) <- pretty_names[levels(flow_df$cc)]

#---- Drawing the plot ----
width  <- 25
height <- 3
xspace <- 1
yspace <- 1

flow_df[ , x := 0 ]
flow_df[ crit_value == FALSE, x := width + xspace]

flow_df[ , order := cumsum(!duplicated(crit)) - 1]
flow_df[ , y     := -1 * ( order * height + order * yspace)]

flowchart <- ggplot(flow_df) +
	geom_tile(aes(x = x, y = y, width = width, height = height),
			  fill = "white", colour = "black", size = 0.2) +
	geom_text(aes(x = x, y = y, label = label)) +
	# Vertical lines for included
	geom_segment(aes(x = 0, xend = 0, y = y + 0.5 * height + yspace, yend = y + 0.5 * height),
				 data = flow_df[crit_value == TRUE]) +
	# First horizontal segment for excluded
	geom_segment(aes(x = 0, xend = x, y = y + 0.5 * height + 0.5 * yspace, yend = y + 0.5 * height + 0.5 * yspace),
				 data = flow_df[crit_value == FALSE]) +
	# Vertical segment for excluded
	geom_segment(aes(x = x, xend = x, y = y + 0.5 * height + 0.5 * yspace, yend = y + 0.5 * height),
				 data = flow_df[crit_value == FALSE],
				 arrow = arrow(length = unit(2, "mm"))) +
	facet_grid(vars(target_range), vars(cc)) +
	theme_void()

ggsave("plots/flowchart.pdf", flowchart, cairo_pdf, width = 7, height = 9, scale = 1.8)
ggsave("plots/flowchart.eps", flowchart, cairo_ps, width = 7, height = 9, scale = 1.8)
ggsave("plots/flowchart.png", flowchart, width = 7, height = 9, scale = 1.8)
