boxplots_1num_multicat_ggplot <- function(single_numerical_variable, categorical_groupings_vars_df, name_single_numerical=names(single_numerical_variable), names_categorical_vars=names(categorical_groupings_vars_df))
{
library('ggbeeswarm')
library('ggplot2')
library('dplyr')
library('tidyr')
single_numerical_variable <- data.frame(single_numerical_variable)
categorical_groupings_vars_df <- lapply(categorical_groupings_vars_df, as.factor)
#lambda definition pipeline
#names_categorical_vars <- names(categorical_groupings_vars_df)
long <- . %>% pivot_longer( cols = all_of(names_categorical_vars),
		names_to = 'grouping_variable',
		values_to = 'categorical_value')
#Merge into a single long df
merged_dataset <- data.frame(single_numerical_variable, categorical_groupings_vars_df)
#Creates longtidy formats and assign outlaiers status in variable imputation
#raw to long pipeline
	merged_dataset <- merged_dataset %>% long
	name_single_numerical <- sym(name_single_numerical[1])
	return(
		ggplot(merged_dataset, aes(categorical_value, !!name_single_numerical, fill = categorical_value)) +
		geom_beeswarm(groupOnX=FALSE, alpha=0.15) +
		#geom_violin(alpha=0.5) +
		geom_boxplot(alpha=0.5) +
		facet_wrap(~ grouping_variable, scales='free')
	)
}




# Function: boxplots_multinumeric_multicat_lapply
# Description: Creates boxplots for a single numerical variable against multiple categorical variables.
# Inputs:
#   - boxplotee_numerical_df: Numerical dataframe with a single column to be used for boxplots.
#   - categorical_groupings_vars_df: Categorical dataframe containing grouping variables.
#   - names_numerical_vars: Names of the numerical dataframe (default is names of boxplotee_numerical_df).
#   - names_categorical_vars: Names of the categorical dataframe (default is names of categorical_groupings_vars_df).
# Output: 
#   - List containing outputs from the boxplot function.
########################################################################
########################################################################
########################################################################
## SENCOD DEFINITON MANUALLY DONE BY ME
boxplots_multinum_multicat_mapped <- function(boxplotee_numerical_df=boxplotee_numerical_df, 
												categorical_groupings_vars_df=categorical_groupings_vars_df,
                                                      names_numerical_vars = names(boxplotee_numerical_df),
                                                      names_categorical_vars = names(categorical_groupings_vars_df))
{
  # Validate inputs
  if (!is.data.frame(boxplotee_numerical_df) || !is.data.frame(categorical_groupings_vars_df))
	{
    stop("Inputs must be data frames.")
	}
  
  # Create a list to store boxplot outputs
  boxplot_outputs <- lapply(names_numerical_vars, function(num_var)
	{
		# Create a dataframe with the single numerical variable
		num_df <- data.frame(boxplotee_numerical_df[[num_var]])
		## INSER MY OWN GGPLOT BOXPLOT FOR ONLY ONE NUMERICAL COLUMN
		boxplot_title <- paste("Boxplot of", num_var)
		# CREATE BOXPLOT FOR EACH CATEGORICAL VARIABLE
		boxplots_result <- boxplots_1num_multicat_ggplot(num_df, categorical_groupings_vars_df)
		return(boxplots_result)
	})
	return(boxplot_outputs)
}

######################################
######################################
######################################
bars_1cat_multicat_ggplot <- function(single_cat_variable, categorical_groupings_vars_df, arg_position = 'dodge', name_single_cat=names(single_cat_variable), names_categorical_vars=names(categorical_groupings_vars_df))
{
library('ggbeeswarm')
library('ggplot2')
library('dplyr')
library('tidyr')
single_cat_variable <- data.frame(single_cat_variable)
categorical_groupings_vars_df <- lapply(categorical_groupings_vars_df, as.factor)
#lambda definition pipeline
#names_categorical_vars <- names(categorical_groupings_vars_df)
long <- . %>% pivot_longer( cols = all_of(names_categorical_vars),
		names_to = 'faceting_grouping_variable',
		values_to = 'category_val')
#Merge into a single long df
merged_dataset <- data.frame(single_cat_variable, categorical_groupings_vars_df)
#Creates longtidy formats and assign outlaiers status in variable imputation
#raw to long pipeline
	merged_dataset <- merged_dataset %>% long
	name_single_cat <- sym(name_single_cat[1])
	######################################
		#ggplot(merged_dataset, aes(categorical_value, !!name_single_cat, fill = category_val)) +
		#geom_boxplot(alpha=0.5) +
		#facet_wrap(~ faceting_grouping_variable, scales='free')
# Function: create_ggplot_graphs_stacked
# Description: Creates stacked bar graphs based on three categorical variables (genus, category_val, and faceting_grouping_variable)
#              and saves the images to disk.
# Inputs:
#   - merged_dataset: Dataframe containing the variables generic_type, category_val, and faceting_grouping_variable.
#   - faceting_grouping_variable: Name of the faceting grouping variable to facet the graphs.
#   - output_dir: Directory path where the images will be saved.
# Output: 
#   - None (Images are saved to disk).
  # Create ggplot stacked bar graphs and save images
  ggplot(merged_dataset, aes(x = category_val, fill = !!name_single_cat)) +
    geom_bar(position = arg_position) +
    facet_wrap(~ faceting_grouping_variable, scales='free') +
    labs(title = "Categorical Distribution")
	## SAVE TO DISK IN CURRENT DIRECTORY
    #ggsave(filename = paste0('cat_levels_stacked_', names(single_cat_variable), '.png'))
}

# Function: boxplots_multinumeric_multicat_lapply
# Description: Creates boxplots for a single numerical variable against multiple categorical variables.
# Inputs:
#   - boxplotee_numerical_df: Numerical dataframe with a single column to be used for boxplots.
#   - categorical_groupings_vars_df: Categorical dataframe containing grouping variables.
#   - names_numerical_vars: Names of the numerical dataframe (default is names of boxplotee_numerical_df).
#   - names_categorical_vars: Names of the categorical dataframe (default is names of categorical_groupings_vars_df).
# Output: 
#   - List containing outputs from the boxplot function.
## NOTE: THE OUTPUT LIST MUST BE SAVED TO A VARIABLE. OTHERWISE IT CHRASHES
########################################################################
########################################################################
########################################################################
## SENCOND DEFINITON MANUALLY DONE BY ME

bars_multicat_multicat_mapped <- function(baree_levels_categories=baree_levels_categories, 
												baree_facetting_categories=baree_facetting_categories,
												arg_position = 'dodge',
                                                      names_levels_cats = names(baree_levels_categories),
                                                      names_facets_cats = names(baree_facetting_categories))
{
  # Validate inputs
  if (!is.data.frame(baree_levels_categories) || !is.data.frame(baree_facetting_categories))
	{
    stop("Inputs must be data frames.")
	}
  
  # Create a list to store graphic outputs
  graphic_outputs <- lapply(names_levels_cats, function(single_level) {
		## single_level is a character variable
		# Create a dataframe with the single numerical variable
		single_level_df <- data.frame(baree_levels_categories[[single_level]])
		names(single_level_df) <- single_level
		## INSER MY OWN GGPLOT FOR ONLY ONE LEVEL CATEGORY
		# create graph for each categorical variable
		bars_1cat_multicat_ggplot(single_level_df, baree_facetting_categories, arg_position = arg_position)
	})
	return(graphic_outputs)
}

