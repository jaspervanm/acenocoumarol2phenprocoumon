mfiles   <- list.files("cache/matchobjects", "matchobject-tr", full.names = TRUE)
ps_models <- map(mfiles, readRDS) %>%
	map("model")

names(ps_models) <- str_match(mfiles, "-tr_(.*?)\\.rds")[,2]

map_df(ps_models, broom::tidy, exponentiate = TRUE, conf.int = TRUE, .id = "model") %>%
	mutate(
		Print = JasperTools::format_number(estimate, 2)
	) %>%
	mutate(
		temp         = strsplit(model, "_", fixed = TRUE),
		target_range = map_chr(temp, 1),
		subgroup     = map_chr(temp, 2),
		temp         = NULL
	) %>%
	select(target_range, subgroup, term, Print) %>%
	spread(subgroup, Print) %>%
	knitr::kable(style = "markdown") %>%
	write("tables/propensity_score_models.md")
