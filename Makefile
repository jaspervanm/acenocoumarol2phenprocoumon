vpath %.R scripts
vpath %.rds cache

.PHONY: all skip submission
all: manuscript/manuscript.docx manuscript/supplements.docx

ifeq ($(OS),Windows_NT)     # is Windows_NT on XP, 2000, 7, Vista, 10...
DBDIR := H:/Data/postgresql/data/
else
DBDIR := /Users/jasper/Library/Application\ Support/Postgres/var-11
endif

data/INR_data.rds data/patient_data.rds: data/db_last_run.timestamp
data/db_last_run.timestamp: data_load_database.R data/source.txt db_connect.R
	pg_ctl start -D $(DBDIR) ; \
	Rscript $< ; \
	touch $@

data/all_doses.rds: load_all_doses.R cache/eligibility_qoac.rds data/patient_data.rds
	pg_ctl start -D $(DBDIR) ; \
	Rscript $<

cache/eligibility_data.rds: eligibility_data.R data/INR_data.rds data/patient_data.rds
	Rscript $<

cache/eligibility_qoac.rds: eligibility_qoac.R cache/eligibility_data.rds
	Rscript $<

cache/summary_followup.rds: summarise_followup.R cache/eligibility_qoac.rds data/patient_data.rds
	pg_ctl start -D $(DBDIR) ; \
	Rscript $<

data/subgroups.rds: define_subgroups.R data/patient_data.rds data/all_doses.rds qoac.rds
	Rscript $<

NSETS = 50
QSETS := $(patsubst %,cache/splitted_qoac/qoac_%.rds,$(shell seq 1 1 $(NSETS)))
DSETS := $(patsubst %,cache/splitted_inrs/split_%.rds,$(shell seq 1 1 $(NSETS)))

$(DSETS): cache/splitted_inrs/last_split.timestamp
cache/splitted_inrs/last_split.timestamp: split_files_qoac.R data/INR_data.rds data/patient_data.rds cache/eligibility_qoac.rds
	Rscript $< $(NSETS) ; \
	touch $@

cache/splitted_qoac/qoac_%.rds: calc_qoac.R cache/splitted_inrs/split_%.rds
	Rscript $^

TRs := 2.5-3.5 2-3.5 2-3
QOAC_TR := $(patsubst %,cache/qoac-tr_%.rds,$(TRs))

qoac.rds switchers_qoac.rds $(QOAC_TR): cache/combined.timestamp
cache/combined.timestamp: combine_qoac.R $(QSETS)
	Rscript $< $(QSETS); \
	touch $@

# Calculate some summaries on-the-fly START
cache/summary_matched_qoac.rds: cache/matched_qoac.rds
	Rscript -e "saveRDS(as.data.table(readRDS('$<'))[ subgroup == 'all', .( \
	n    = .N, mTTR = mean_sd(100*in_range, na.rm = TRUE, digits = 0), mVGR = mean_sd(vgr, na.rm = TRUE) \
	), by = c('target_range', 'cc', 'period_id')], '$@')"
# END on-the-fly summarisation

analysis_cases_switch.rds: analyse_cases_switch.R qoac.rds
	Rscript $<

MATCHED_TR := $(patsubst %,cache/matched_qoac-tr_%.rds,$(TRs))
cache/matched_qoac-tr_%.rds: match_qoac_cc.R cache/qoac-tr_%.rds data/patient_data.rds data/all_doses.rds data/subgroups.rds
	Rscript $^

cache/matched_qoac.rds: $(MATCHED_TR)
	Rscript -e "saveRDS(map_df(str_split('$^', ' ')[[1]], readRDS), '$@')"

plots/flowchart.% cache/flow_%.rds: flowchart.R cache/eligibility_qoac.rds cache/matched_qoac.rds
	Rscript $<

cache/qoac_over_time.rds: plot_qoac_matches.R data/INR_data.rds cache/matched_qoac.rds
	Rscript $< calc

tables/matching_performance.txt plots/matching_performance.pdf cache/balance_distance.rds: check_matching_performance.R $(MATCHED_TR)
	Rscript $<

tables/matched_overall.md \
cache/matched_ard_good.rds \
tables/matched_subgroups.md: cache/matched_analyses.rds
cache/matched_analyses.rds: analyse_qoac_matches.R cache/matched_qoac.rds
	Rscript $<

cache/matched_heterogeneity.rds plots/subgroup_forest.%: analyse_heterogeneity_subgroups.R cache/matched_analyses.rds
	Rscript $<

tables/propensity_score_models.md: propensity_estimates.R $(MATCHED_TR)
	Rscript $<

plots/correlation_heatmap.pdf: propensity_colinearity.R $(MATCHED_TR)
	Rscript $<

plots/matched_development_%.png plots/matched_development_%.eps: plot_qoac_matches.R cache/qoac_over_time.rds data/subgroups.rds
	Rscript $< plot

tables/switchers_qoac.md tables/switchers_qoac_subgroups.md cache/dose_switchers_summary.rds cache/switchers_correlations.rds: analyse_switchers.R qoac.rds data/subgroups.rds
	Rscript $<

plots/vgr_switchers.png: graph_vgr_switchers.R qoac.rds
	Rscript $<

tables/patchar_%.md: patient_characteristics.R \
	qoac.rds \
	data/patient_data.rds \
	data/all_doses.rds \
	data/subgroups.rds \
	cache/matched_qoac.rds
	Rscript $<

plots/periods.%: scripts/plot_periods.R
	Rscript $<

#---- Manuscript generation ----
manuscript/bibliography.bib: ../../articles/bib/variability_phenprocoumon.bib
	cp -f $< $@

manuscript_dependencies = manuscript/bibliography.bib \
	manuscript/settings.md \
	tables/switchers_qoac.md \
	tables/patchar_cases.md \
	tables/matched_overall.md \
	plots/periods.png \
	plots/flowchart.png \
	plots/matched_development_all.png \
	plots/subgroup_forest.png \
	cache/switchers_correlations.rds \
	cache/dose_switchers_summary.rds \
	cache/summary_followup.rds \
	cache/summary_matched_qoac.rds \
	cache/balance_distance.rds

manuscript/manuscript.md: manuscript.Rmd $(manuscript_dependencies)
	Rscript -e "knitr::knit('$<', '$@')"

manuscript/manuscript.docx: manuscript/manuscript.md $(manuscript_dependencies)
	pandoc manuscript/settings.md $< -o $@ --filter=pandoc-citeproc

manuscript/supplements.docx: manuscript/supplements.Rmd \
	manuscript/bibliography.bib \
	manuscript/settings.md \
	tables/patchar_all_controls.md \
	tables/switchers_qoac_subgroups.md \
	tables/propensity_score_models.md \
	tables/patchar_matched.md \
	tables/patchar_subgroups.md \
	tables/matched_subgroups.md
	Rscript -e "knitr::knit('$<', 'supplements_temp.md')"
	pandoc manuscript/settings.md supplements_temp.md -o $@ --filter=pandoc-citeproc
	rm supplements_temp.md

manuscript/abstract.md: abstract.Rmd \
	cache/flow_cc_tr.rds \
	cache/matched_analyses.rds
	Rscript -e "knitr::knit('$<', '$@')"

manuscript/abstract.docx: manuscript/abstract.md
	pandoc manuscript/settings.md $< -o $@

#---- End manuscript generation ----
#---- Generate files for submission ----
submission: submission/manuscript.docx \
	submission/abstract.docx \
	$(patsubst %,submission/S%_Table.docx, 1 2 3 4 5 6) \
	$(patsubst %,submission/Fig_%.eps, 1 2 3 4)

submission/Fig_1.eps: plots/periods.eps
submission/Fig_2.eps: plots/flowchart.eps
submission/Fig_3.eps: plots/matched_development_all.eps
submission/Fig_4.eps: plots/subgroup_forest.eps

submission/%.eps:
	cp -f $< $@

submission/S1_Table.docx: tables/patchar_all_controls.md
submission/S2_Table.docx: tables/switchers_qoac_subgroups.md
submission/S3_Table.docx: tables/propensity_score_models.md
submission/S4_Table.docx: tables/patchar_matched.md
submission/S5_Table.docx: tables/patchar_subgroups.md
submission/S6_Table.docx: tables/matched_subgroups.md

submission/S%_Table.docx:
	pandoc manuscript/settings_supplements.md \
	$(patsubst %.docx, manuscript/caption_%.md, $(notdir $@)) $< \
	-o $@ --reference-doc=manuscript/table_reference.docx

submission/abstract.docx: manuscript/abstract.docx
	cp -f $< $@

submission_dependencies = manuscript/abstract.md \
	manuscript/keywords.md \
	manuscript/title_page.md

submission/manuscript.docx: manuscript/manuscript.md \
	manuscript/abstract.md \
	$(manuscript_dependencies) \
	$(submission_dependencies)
	pandoc manuscript/settings.md \
	manuscript/title_page.md \
	manuscript/abstract.md \
	manuscript/keywords.md \
	manuscript/manuscript.md \
	-o $@ --filter=pandoc-citeproc \
	--reference-doc=manuscript/general_reference.docx

submission/supplements.docx: manuscript/supplements.docx
	cp -f $< $@
#---- End submission generation ----

# To cheat Makefile into not doing the heavy steps again
skip:
	touch data/INR_data.rds; \
	touch data/patient_data.rds; \
	touch cache/eligibility_data.rds; \
	touch cache/eligibility_qoac.rds; \
	touch cache/summary_followup.rds; \
	touch cache/splitted_inrs/*; \
	touch cache/splitted_qoac/*; \
	touch data/all_doses.rds
