
#================================================
# COMPUTES AND PLOT ANALYSIS OF CORRELATION AND DIFFERENCES USING LM AND GML
# SEND_TO_ TAKES TWO DATAFRAMES CONTAININ THE RESPONSES AND PREDICTORS AND CALCULATE THEIR REGRESSION
# THE FLAGS CATEGORICA AND LOGIT MUST BE SET TO TRUE TO ENABLE BINOMIAL GLM ANALYSIS OR CATEGORICAL PREDICTORS
#================================================

#function REGRESSIONS significant values
one_by_one_significant_predictors_lm  <- function(one_response, predictors, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = FALSE)
{
    linear_model <- ifelse(logit_binomial_flag, 
        function(x)glm(as.factor(one_response) ~ x, family = 'binomial'),
        function(x)lm(one_response ~ x)
        )
    if(categorical_flag) predictors[] <- lapply(predictors, function(x) as.factor(x))
    #THIS REFACTURING USES SAPPLY INSTEAD OF MAP
	models  <- sapply(predictors, linear_model, simplify = FALSE)
    #USE COLUMN Pr>| t or z |) WHICH IS THE FOURTH COLUM IN THE COEFFICIENT MATRIX
	coeffs  <- sapply(models, function(x) coef(summary(x))[-1, 4], simplify = FALSE)
	sig  <- sapply(coeffs, function(x) x[ x <= threshold_significance & !is.na(x) ]  , simplify = FALSE)
	selector <- sapply(sig, function(x) length(x) > 0)
	models  <- models[selector]
	trim <-  sig[selector]
	return(trim)
}

send_responses_to_predictors_lm <- function(responses_dataset, predictors_dataset, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = FALSE)
{
	results_predictors_response_one_by_one <- lapply(responses_dataset, one_by_one_significant_predictors_lm, predictors_dataset, threshold_significance = threshold_significance, categorical_flag = categorical_flag, logit_binomial_flag = logit_binomial_flag)
    #NOTE THAT ALL THE FUNCTION ARGUMENTS NEED TO BE CARRIED OVER TO THE LAPPLY
	selector <- sapply(results_predictors_response_one_by_one, function(x) length(x)>0)
	return(results_predictors_response_one_by_one[selector])
}

#=================================================================
# THE FOLLOWING THREE FUNCTIONS IMPLEMENT THE MAPPED LM ANALISES OF TWO DATAFRAMES. THEY ARE SIMILAR TO THE SEND_TO_ AND ONE_BY_ONE_ FUNCTIONS IN MY ORIFINAL CATEGORICA_GLM PACKAGE. THESE NEW FUNTION USE AUTOMATIC DECISION OF MODEL BASED ON THE CONTENET OF THE COLUMNS (FACTORS INTEGER 0 TO N). THE OLD FUNCTION ARE KEPT FOR COMPATIBILITY.
#=================================================================
# SELECTS GLM MODEL DEPENDING ON THE CONTENT OF COLUMNS, MULTICAT COLUMNS ARE COLUMN THE CONTAIN ONLY INTEGERS BEYWEEN 0 AND N NUMBER_OF_LEVELS
linear_model_automatic_selector  <- function(response_column, predictor_column, number_of_levels = number_of_levels)
{
        #DECISION ON DEPENDENT VARIABLES
        #INVERT THE ORDER RESPONSE PREDICTOR IN THE COLUMN IS MUTICAT
        if(is_multicat_but_not_binary(response_column, number_of_levels = number_of_levels))
        {
                temp  <- response_column
                response_column <- predictor_column
                predictor_column <- temp
        }

        #DECISION ON INDEPENDENT VARIABLES
        if(is_any_kind_of_categorical(predictor_column, number_of_levels = number_of_levels)) predictor_column <- as.factor(predictor_column)

        #DEcISIONS ON WHETHER THE RESPONSE IS BINARY
        #IF RESPONSE IS BINARY USE LOGISTIC BINOMIAL REGRESSION
        if(is_binary_factor(response_column))
        {
                response_column <- as.factor(response_column)
                linear_model <- function(y, x)glm(y ~ x, family = 'binomial')
        }
        else
        {
                response_column <- as.numeric(response_column, drop.unused.levels = FALSE)
                linear_model <- function(y, x)lm(y ~ x)
        }

        #RUN MODEL AND EXTRACT THE COEFFICIENTS COLUMN _ COLUMN FOUR _ THE IRST ROW IS REMOVED SINCE SUCH ROW IS NOT RELEVANT
        pvalues_coefficients_column <- coef(summary(linear_model(response_column, predictor_column)))[-1, 4]
        return(pvalues_coefficients_column)
}

#function REGRESSIONS significant values
analyze_one_response_many_predictors_using_automatic_lm_selector  <- function(one_response, predictors, number_of_levels = number_of_levels, threshold_significance = 0.05)
{
    #THIS REFACTURING USES SAPPLY INSTEAD OF MAP
	coeffs  <- sapply(predictors, function(x)linear_model_automatic_selector(one_response, x, number_of_levels = number_of_levels), simplify = FALSE)
    #USE COLUMN Pr>| t or z |) WHICH IS THE FOURTH COLUM IN THE COEFFICIENT MATRIX
	sig  <- sapply(coeffs, function(x) x[ x <= threshold_significance & !is.na(x) ]  , simplify = FALSE)
	selector <- sapply(sig, function(x) length(x) > 0)
	sig[selector]
}

# RUN LM ANALYSES BY MAPPIN MANYREPONSES TO MANY PREDICORS _ TAKES TWO DATAFRAMES AS INPUTS _ RETURNS A LIST WOTH SIGNIFICANT PVALUES
analyze_many_response_many_predictors_using_automatic_lm_selector <- function(responses_dataset, predictors_dataset, number_of_levels = number_of_levels, threshold_significance = 0.05)
{
	results_predictors_response_one_by_one <- sapply(responses_dataset, analyze_one_response_many_predictors_using_automatic_lm_selector, predictors_dataset, number_of_levels = number_of_levels, threshold_significance = threshold_significance, simplify = FALSE)
    #NOTE THAT ALL THE FUNCTION ARGUMENTS NEED TO BE CARRIED OVER TO THE LAPPLY
	selector <- sapply(results_predictors_response_one_by_one, function(x) length(x)>0)
	return(results_predictors_response_one_by_one[selector])
}
#=================================================================
#=================================================================
#=================================================================


merged_categorical_and_torrance_totals <- function(columns_dataset, categorical_names = c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx'), sign=0.05)
{
    merged_dataset <- factors %>%
        add_id_column_numero %>%
        merge(add_id_column_numero(scales), by='numero') %>%
        merge(add_id_column_numero(raven), by='numero') %>%
        merge(add_id_column_numero(columns_dataset), by='numero') %>%
        mutate(numero=as.numeric(numero)) %>%
        select(!starts_with('X'))

    tor <- merged_dataset %>%
        select(c(names(columns_dataset), -numero))
    torrest <- merged_dataset %>%
        select(!names(columns_dataset))


    #categorical_names <- c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx')
    categorical_dataset <- merged_dataset %>%
        select(all_of(categorical_names)) %>%
        mutate(across(where(is.numeric), as.factor))

    return(list(tor, categorical_dataset))
}


merged_lm_responses_to_predictors <- function(lmee_responses, lmee_categoricals, responses_names = names(lmee_responses), categoricals_names = names(lmee_categoricals), threshold_significance = 0.05, categorical_flag = FALSE, side = 1, cex = 1.0, pch = 1.0, alpha = 2/5, size = 1.7)
{
tttt <- merge(add_id_column_numero(lmee_responses), add_id_column_numero(lmee_categoricals), by = 'numero')
cats <- tttt[, names(lmee_categoricals)]
#if(is.null(responses_names)) responses_names <- names(lmee_responses)
resp <- tttt[, responses_names]
results_list <- send_responses_to_predictors_lm(resp, cats, threshold_significance, categorical_flag)
if(length(results_list) > 0)
{
	print(results_list)

	pairs_list <- find_list_significant_differences_in_multi_lm(results_list)
	anova_graphs_list <- anova_graphs_from_lm_pairs_list(tttt, pairs_list, threshold_significance = threshold_significance, side=side, cex=cex, pch=pch, alpha=alpha, size=size)
			#Show a single graph from the list of anova graphs
			#NOTE: marrangeGrobs does not work with these anova plots, find the replacement in stackoverflow.
			library('gridExtra')
			library('cowplot')
			if(length(anova_graphs_list) > 1)
			{
#printing in the anova_graphs funtion itself
			#print(anova_graphs_list)
			#print(cowplot::plot_grid(plotlist = anova_graphs_list, ncol = 3, nrow = 2))
			#print(gridExtra::marrangeGrob(grobs = anova_graphs_list, ncol = 3, nrow = 2))
			}
			else {
				#print(anova_graphs_list)
				}

return(results_list)
}
else
{
print('=== NO SIGNIFICANT RESULTS ===')
return(NULL)
}

}


#================================================================
#FUNCTION TO EXTACT THE STRING_PAIRS FROM THE MULTI LM RETURNED LIST
#================================================================
find_list_significant_differences_in_multi_lm <- function(pairee_list)
{
	lm_pairs_list <<- NULL
	responses_names <- names(pairee_list)
	llll <- lapply(responses_names, 
		function(x) lapply(names(pairee_list[[x]]),
			function(y) { print(paste(x,y)); lm_pairs_list[[ length(lm_pairs_list) + 1 ]] <<- c(x,y) }
		) )
	return(lm_pairs_list)
}




#================================================================
#ANOVA from pairs list
#================================================================
anova_graphs_from_lm_pairs_list <- function(complete_dataset, lm_pairs_list, threshold_significance = 0.05, side = 1, cex = 1.0, pch = 1.0, alpha = 2/5, size = 1.7)
{
	anova_graphs_list <- lapply(lm_pairs_list, 
		function(x)
			one_way_anova_graph(complete_dataset, x[1], x[2], threshold_significance = threshold_significance, side=side, cex=cex, pch=pch, alpha=alpha, size=size)
			)
	return(anova_graphs_list)
}


#================================================================
two_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, grouping_column2, threshold_significance = 0.05)
{

#Make a data frame with the group labels
#Now we need to make an additional data frame so we can add these groupwise differences to our graph.
#First, summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.
complete_dataset[,grouping_column1] <- as.factor(complete_dataset[, grouping_column1])
complete_dataset[,grouping_column2] <- as.factor(complete_dataset[, grouping_column2])

summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1), !! as.symbol(grouping_column2)) %>%
  summarise(mean = mean(!! as.symbol(response_column)))
print('=== GROUPS DESCRIPTIVE STATISTICS ===')
print(summarized_stats)


#Next, add the group labels as a new variable in the data frame.
#summarized_stats$group <- c("a","b","c","d")

summarized_stats


library(ggplot2)
library(ggbeeswarm)

gganova <- ggplot(complete_dataset, aes(get(grouping_column2), get(response_column), col = get(grouping_column2), group=get(grouping_column1))) +
## HERE UNCOMMCOMMENT TO USE BEESWARM
  #geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.02, h = 0)) +
  geom_beeswarm() +
#Add the means and standard errors to the graph
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column2), y=mean)) +
#Split up the data
#  geom_text(data=summarized_stats, label=summarized_stats$group, vjust = -8, size = 5) +
  facet_wrap(~ get(grouping_column1)) +
#Make the graph ready for publication
#  theme_classic() +
  labs(title = paste(grouping_column2, ' Diferrences in ', response_column, '\n Faceted by ', grouping_column1),
      x = grouping_column2,
      y = response_column)

return(gganova)

}



  

#================================================================
one_way_anova_graph <- function(complete_dataset, response_column, grouping_column1, threshold_significance = 0.05, printing_flag = TRUE, side = 1, cex = 1.0, pch = 1.0, alpha = 2/5, size = 1.7)
{
complete_dataset[,grouping_column1] <- as.factor(complete_dataset[, grouping_column1])
#Summarize the original data using grouping_column1 type and planting grouping_column2 as grouping variables.
library(dplyr)
summarized_stats <- complete_dataset %>%
  group_by(!! as.symbol(grouping_column1)) %>%
  summarise(mean = mean(!! as.symbol(response_column)), sd = sd(get(response_column)), num = n())
#summarized_stats

library(ggbeeswarm)
#Make graph
gganova <- ggplot(complete_dataset, aes(get(grouping_column1), get(response_column), col = get(grouping_column1))) +
  #geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.05, h = 0)) +
  #geom_beeswarm(side = 1, cex = 1.0, pch = 1.0, alpha = 2/5, size = 1.7) +
  geom_beeswarm(side=side, cex=cex, pch=pch, alpha=alpha, size=size) +
  #geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.02, h = 0)) +
#Add the means and standard errors to the graph
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
geom_point(data=summarized_stats, aes(x=get(grouping_column1), y=mean)) +
#Make the graph ready for publication
#  theme_classic() +
  labs(title = paste(grouping_column1, '-groups Diferrences in ', response_column),
      x = grouping_column1,
      y = response_column)

	if(printing_flag)
		{
		print('=== GROUP DIFFERENCES GRAPH ===')
		print(gganova)
		print(paste0('=== DESCRIPTIVE STATS OF ', response_column, ' IN ', grouping_column1, ' GROUPS ==='))
		print(summarized_stats)
		}

return(gganova)
}

#descriptee_dataset <- factors[, 1:3]
#descriptee_dataset <- raven[, raven_names]
#contrasting_label <- 'percentile'

#boxplots_raw_cleaned_outlaiers <- function(baseline_dataset, contrasting_dataset = baseline_dataset, baseline_label = 'original', contrasting_label = 'contrasting')
descriptives_skim_boxplot_histo_polygon <- function(descriptee_dataset)
{
#baseline_dataset <- data.frame(lapply(baseline_dataset, as.numeric))
library(skimr)
library('ggplot2')
library('dplyr')
library('tidyr')

#Compute bw as the optimal binwidth for histogram
x <- descriptee_dataset[, 3]
bw <- 2 * IQR(x) / length(x)^(1/3)

skimmed_dataset <- descriptee_dataset %>%
	skim %>%
	#select(-skim_type, -n_missing, -complete_rate, -factor.ordered, -factor.n_unique, -factor.top_counts)
	select(-n_missing, -complete_rate) %>%
	rename(var = skim_variable, mean = numeric.mean, sd = numeric.sd, p0 = numeric.p0, p25 = numeric.p25, p50 = numeric.p50, p75 = numeric.p75, p100 = numeric.p100, hist = numeric.hist)


#lambda definition pipeline

long <- . %>% 
	pivot_longer( cols=everything(),
		names_to='variable',
		values_to='value')

#Creates longtidy format
long_dataset     <- descriptee_dataset %>% long

ggboxplot <- ggplot(long_dataset, aes(variable, value, fill = variable)) +
geom_boxplot()


ggbar <- ggplot(long_dataset, aes(value, fill=variable)) +
#geom_bar(alpha=0.5, width=0.99) +
geom_histogram(binwidth = bw, col = 'black') +
facet_wrap(~ variable, scales='free')

ggpolygon <- ggplot(long_dataset, aes(value, fill=variable, col=variable)) +
#geom_freqpoly(alpha=0.5, binwidth=4) +
geom_freqpoly(binwidth = bw * 1) +
facet_wrap(~ variable, scales='free')

print(skimmed_dataset)
print(ggboxplot)
print(ggbar)
print(ggpolygon)
}







#threshold_significance <- 0.05
#complete_dataset <- cbind(torrance_percentil, torrance_csv_original)
#lm_pairs_list <- find_list_significant_differences_in_multi_lm(rrrr)
			#Send list of names to generate scatterplots
			#scatters_list <- lapply(pairs_list, scatterp_with_regression_lines, rows_dataset, columns_dataset)


library('tidytext')
library('widyr')
library('ggplot2')
library('dplyr')
library('tidyr')
library('tibble')
library('psych')
library(broom)
library(purrr)
library(tidyr)
library(dplyr)
library(rmarkdown)
library(hugodown)


categorical_names <- c('perfil', 'escuela', 'grupo', 'sexo', 'edad', 'percentil', 'rango', 'dx')
