# We are assessing colinearity in the unmatched data
# to explain potentially weird point estimates

mfiles   <- list.files("cache/matchobjects", "matchobject-tr", full.names = TRUE)

mobjects <- map(mfiles, readRDS)
names(mobjects) <- str_match(mfiles, "-tr_(.*?)\\.rds")[,2]

map_df(mobjects, function(matched) {
	model.matrix(matched$formula, matched$model$data) %>%
		cor() %>%
		round(2) %>%
		reshape2::melt()
}, .id = "group") %>%
	filter(Var1 != "(Intercept)", Var2 != "(Intercept)") %>%
	ggplot(aes(x = Var1, y = Var2, fill = value, label = value)) + geom_tile() +
	geom_text() +
	scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
						 midpoint = 0, limit = c(-1,1), space = "Lab", 
						 name = "Pearson\nCorrelation") +
	facet_wrap("group") +
	ggthemes::theme_few() +
	theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave("plots/correlation_heatmap.pdf", width = 15, height = 10, scale = 1.5)
