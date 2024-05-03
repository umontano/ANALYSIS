#packages_vector <- c('dplyr')
load_or_install_packages <- function(loadee_package_name)
{
        if(!require(loadee_package_name))
        {
                install.packages(loadee_package_name)
                library(loadee_package_name)
        }
}
#lapply(packages_vector, load_or_install_packages)

#funs() was deprecated in dplyr 0.8.0.
# Please use a list of either functions or lambdas:
# Simple named list: list(mean = mean, median = median)
# Auto named with `tibble::lst()`: tibble::lst(mean, median)
# Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))


# COMPUTES THE DESCRIPTIVE STATISTIC _MEAN AND SD_ OF A DATA FRAME GROUPE BY ONE VARIABLE
# TAKES AS INPUT 1_ A STRING CONTAINING THE NAME OF THE GROUPING VARIABLE
# 2_ THE DATASET TO BE DESCRIBED
descriptives_grouped_by_first_column <- function(grouping_variable, descriptee_dataset)
{
    library(dplyr)
    summarized_statistics <- descriptee_dataset %>%
        tibble %>%
        group_by(!!sym(grouping_variable)) %>%
        # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
        summarise_each(list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE)))
        return(summarized_statistics)
}

# MAPPED DESCRIPTIVES. FIRST ARGUMENT IS A DATASET WITH THE MANY GROUPIN VARIAVLES. SECOND ARGUMENT IS A DATASET WITH ALL THE VARIABLES TO BE GROUPED AND SUMMARIZED.
# RETURNS A LIST OF AS MANY DESCRIBING DATASETS AS INPUTS IN THE FIRST ARGUMENT.
describe_many_groupings_to_many_descrebees <- function(groupings_dataset, descriptee_dataset)
{
        date_time <- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')
        descriptions_file_name <- paste0('xDESCRIPTIONS_', date_time, '.yaml')
        print(date_time)
        print(descriptions_file_name)
        descriptions_list <- sapply(names(groupings_dataset),function(each_grouping_column_name) aggregate(x = descriptee_dataset,
                                                                                                           list(group = groupings_dataset[, each_grouping_column_name]),
                                                                                                           FUN = 
                                                                                                           function(x) c(mean = mean(x), sd = sd(x))), simplify = FALSE)
        #descriptions_list <- sapply(groupings_dataset,function(each_grouping_column_name) aggregate(. ~ each_grouping_column_name, descriptee_dataset, function(x) c(mean = mean(x), sd = sd(x))), simplify = FALSE)
        #descriptions_list <- sapply(groupings_dataset, descriptives_grouped_by_first_column, descriptee_dataset, simplify = FALSE)
        if(require(yaml)) write_yaml(descriptions_list, descriptions_file_name)
        return(descriptions_list)
}



names_of_non_grouping_variables <- function(grouping_variable, descriptee_dataset)
{
    non_grouping_vars <- names(descriptee_dataset)[grouping_variable != names(descriptee_dataset)]
    print(grouping_variable != names(descriptee_dataset) & 'mpg' != names(descriptee_dataset))
    print(non_grouping_vars)
    return(non_grouping_vars)
}


string_labels_from_variable_symbols <- function(var)
{
 var <- enquo(var)
 as_label(var)
}





# CHECKS WHETHER THE MUBERA ARE ALL ONLY INTEGERS BETWEEN ZERO AND N
# CHECK FOR MEMBERSHIP IN  A ZERO_BASED INTEGER SET
# TAKES AS INPUT A COLUMN TO CHECK _A VECTOR_ AND THE HIGHEST NUMBER THAT BE CONSIDERED AS CATEGORIC _INTEGER_
# ITS AIM IS TO HELP TO SELECT COLUMS IN A DATASET THAT ENCODE CATEGORICAL VARIABLES USING INTEGERS
#has_only_n_integers <- function(checkee_column, number_of_levels = 5){all(checkee_column[!is.na(checkee_column)] %in% 0 : number_of_levels)}
# THIS IS A VARIATION OF THE ABOVE FUNCTION _ IT COMPUTES THE NUMBER OF LEVLS DIRECTLY INSTEAD OF CHECKING FOR MEMBERSHIP IN A ZERO_BASED INTEGER SET
#revised version
has_only_n_integers <- function(checkee_column, number_of_levels = 5)
{
  unique_values <- as.numeric(levels(checkee_column))
  all_integers <- all(!is.na(unique_values) & unique_values %% 1 == 0)  # Check if all unique values are integers
  in_range <- all(!is.na(unique_values) & unique_values >= 0 & unique_values <= (number_of_levels - 1))  # Check if all unique values are within the specified range
  return(all_integers & in_range)
}

 #oldversion failure                                                                                                          
#has_n_leves_or_less <- function(checkee_column, number_of_levels = 5)length(levels(as.factor(checkee_column))) <= number_of_levels

#UNUSED OLD
unused_old_has_only_n_integers <- function(checkee_column, number_of_levels = 5)
{
    print(head(checkee_column))
        set_of_integers <- c(0 : number_of_levels)
    #print(set_of_integers)
        #all(checkee_column %in% set_of_integers, na.rm = TRUE)
    print(length(levels(as.factor(checkee_column))))
    length(levels(as.factor(checkee_column))) <= number_of_levels
}

# COMPUTES WHETHER THE SUPPLIED COLUM IS BINARY _ THAT IS _ IS CATEGORICAL WITH ONLY TWO LEVELS
# TAKES AS INPUT A COLUMN 
# RETURNS TRUE IF THE COLUMN IS BINARY, FALSE OTHERWISE
is_binary_factor <- function(binaryee_factor_column)
{
        binaryee_factor_column <- binaryee_factor_column[!is.na(binaryee_factor_column)]
    binaryee_factor_column <- as.factor(binaryee_factor_column)
    number_of_levels <- length(levels(binaryee_factor_column))
    return(number_of_levels == 2)
}





#=================================================================
# IS THE INPUT COLUMN MADE ONLY OF INTEGERSBUT NOT BINARY INTEGERS, TAKES A DATACOLUMDMN, RETUNS TRUE OR FALSE
#=================================================================
is_multicat_but_not_binary <- function(multicatee_column, number_of_levels = number_of_levels) 
{
        !is_binary_factor(multicatee_column) && is_any_kind_of_categorical(multicatee_column, number_of_levels = number_of_levels)
}


# NEW FUNCTION
# CHECKS WHETHER THE INPUT VECTOR FALLS IS ANY CLASS THAT CN BE ;INTERPRETED AS CATEGORICAL
is_any_kind_of_categorical <- function(checkee_column, number_of_levels = 5)
{has_only_n_integers(checkee_column, number_of_levels) || is.factor(checkee_column) || is.character(checkee_column)}


# SEPARATES A DATASET INTO CATEGORICAL AND NUMERIC VARIABLES
# EVEN IF THE CATEORICAL VARIABLES ARE ENCODED AS INTEGERS
# IT INVOKES THE has_only_n_integers FUNCTION
# TAKES AS INPUT THE DATASET TO SPLIT AND THE HIGHER NUMBER THAT WILL BE CONSIDERED AS CATEGORIC _SUCH NUMBER IS CARRIED TO THE INVIKED FUNCTION
split_categoric_numeric <- function(splitee_dataset, number_of_levels = 5)
{
        if(length(splitee_dataset) < 1)
        {
                binary_dataset <- splitee_dataset
                categorical_dataset <- splitee_dataset
                numeric_dataset <- splitee_dataset
        }
        else
        {
            selector_categorical <- sapply(splitee_dataset, is_any_kind_of_categorical, number_of_levels = number_of_levels)
            print(selector_categorical)
            categorical_dataset <- splitee_dataset[, selector_categorical, drop = FALSE]
#SOLUION IS TO  CONDIIONALLLY APPLY SELECTIION OF BINARY SET ONLY WHEN THERE ARE MORE THAN ON FACTOR COLUMN
            if(length(categorical_dataset) < 1) binary_dataset <- categorical_dataset
            else
            {
                    categorical_dataset[]  <- sapply(categorical_dataset, as.factor, simplify = FALSE)
                    binary_columns_selector  <- sapply(categorical_dataset, is_binary_factor)
                    binary_dataset  <- categorical_dataset[, binary_columns_selector, drop = FALSE]
                    categorical_dataset <- categorical_dataset[, !binary_columns_selector, drop = FALSE]
                    }
            numeric_dataset <- splitee_dataset[!selector_categorical]
        }
    return(list(binary = binary_dataset, categoric = categorical_dataset, numeric = numeric_dataset))
}





# SEPARATES A VECTOR OF VARIABLE NAMES INTO GROUPS DELIMITED BY A DELIMITER MARKER
# TAKES AS INPUT A LIST OF VARIALES AND A STRING WHICH WILL BE USED AS MARKER FOR SEPARATION
# OUTPUT VARIABLE IS A LIST IN WHICH ECAH  ELEMENT CONTAINS THE SEPARATED VECTORS OF NAMES
user_variables_splitter <- function(read_variables = names(mtcars), delimiter_marker = '----')
{
        print(read_variables)
        print(delimiter_marker)
        indices <- which(read_variables == delimiter_marker)
        previous_index <- 0

        sliced_variables <- lapply(indices,
                function(current_index) {
                sliced_vector <- read_variables[previous_index+1 : current_index-1]
                previous_index <<- current_index
                return(sliced_vector) })
        sliced_vector <- read_variables[-indices[1] : -1]
        return(list(responses = unlist(sliced_variables), predictors = sliced_vector))
}








# SEPARATE AS INDICATED IN FILES
# I TAKES A FILE VARIABLES.TXT CONTAINING A LIST OF RESPONSE VARIABLES, A SEPARATOR ---- AND A LIST OF RESPONSES
# TO SEPARATE THE GIVEN DATASETS,IN ADDITION THE RESULTIND DATASETS ARE FURTHER SEPAEATED INTO BINARY CATEGORICAL AND NUMERICAL VARIABLES
# THE OUTPUT I  LIST WINTH THE SEMA NUMBER OF ENTRIES AS THE NUMBER OF DATASETS SUPPIED, EACH LIST ELEMENT IS A LIST CONTAINING THE SEPARATED BINARY CATEGORIC AND NUMERIC DATASETS
separate_all_input_dataset <- function(..., number_of_levels = 5, variables_file = 'variables.txt')
{
read_variables <- unlist(readLines(variables_file))
        datasets <- list(...)
        #datasets <- lapply(datasets, function(dset)ifelse(class(dset) != 'data.frame', as.data.frame(dset), dset))
        all_separated_datasets <- lapply(datasets,
                 function(dset){
                        names_vectors <- user_variables_splitter(read_variables)
 print(names_vectors)
                        responses_predictors_datasets_list <- lapply(names_vectors, function(each_nv){split_categoric_numeric(dset[names(dset) %in% each_nv], number_of_levels = number_of_levels)})
#print(length(responses_predictors_datasets_list))
                        #by_categorical_responses_predictors_datasets_list <- lapply(responses_predictors_datasets_list, function(x){ x <- ifelse(class(x) == 'list', data.frame(unlist(x)), data.frame(x)) })
 #print(by_categorical_responses_predictors_datasets_list)
                        #names(by_categorical_responses_predictors_datasets_list)  <- c('responses_split', 'predictors_split')
                        #return(by_categorical_responses_predictors_datasets_list)
        })
        return(all_separated_datasets)
}


#================================================================
# SEPARATE A DATASET INTO TWO, CONTAINING THE RESPONSES AND PRESICTORS AS SPECIFIED IN THE SPECIFIED FILE, DEFAULT FILE[ variables.txt
# TAKES A DATAFREME TO SPLIT, A NEE OF FILE (DEFAULT variables.txt). RETURNNS A LIST CONTAINING 1 THE RESPONSES DATAFRAME, AND, 2 THE PREDICTORS DATAFRAME.
separate_responses_predictors_as_specified_in_variables_file  <- function(glmee_dataset, variables_file = 'variables.txt')
{
        read_variables <- unlist(readLines(variables_file))
        names_vectors <- user_variables_splitter(read_variables)
        responses_predictors_datasets_list <- lapply(names_vectors, function(each_names_vector)glmee_dataset[names(glmee_dataset) %in% each_names_vector])
}


#RUN GENERL GLM ANALYSES BUT SPLTING THEDATASETS INTO TWO  _RESPONSES PEDICTORS_ AND INTO THREE _BINRY MULTICAT AND NUMERICAL
# TAKES 1_DATASET AND 2_ A FILE CONTAING THE RESPONSES AND PREDICTOR SEPARATED BY A FOUR DASHES ----
# RETURNS THE LSIT OF SIGNIFICANT PVALUES
run_analyses_separating_into_responses_predictors_and_into_binary_multicat_numerical <- function(analysee_list)
{
        #responses_list  <- analysee_list[[1]]
        resulting_list <- lapply(analysee_list, function(xxxx)
         {
        # DEFINE VARIABLES
        binary_dependents <-xxxx[[1]][[1]]
        dependents_multilevel <-xxxx[[1]][[2]]
        dependents_numerical <-xxxx[[1]][[3]]
        independents_multilevel <-xxxx[[2]][[2]]
        joint_independents <- do.call(cbind, xxxx[[2]])
        non_multi_independents <- do.call(cbind,  list(xxxx[[2]][[1]], xxxx[[2]][[3]]))
        joint_dependents <- do.call(cbind, xxxx[[1]])
        # SEND TO ANALYZE, AFTER CHECHING THAT DATASETS ARE NOT EMPTY
        if(length(binary_dependents) < 1) banalyses <- NULL else 
                banalyses <- send_responses_to_predictors_lm(responses_dataset = binary_dependents, predictors_dataset = joint_independents, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = TRUE)
        if(length(dependents_numerical) < 1) manalyses <- NULL else 
                nanalyses <- send_responses_to_predictors_lm(responses_dataset = dependents_numerical, predictors_dataset = joint_independents, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = FALSE)
# MAKE JOINT FROM MULTICAT AND NUMERICAL PREDICTORS
        if(length(independents_multilevel) < 1)
        {
                multibanalyses <- NULL
                multinanalyses <- NULL
        }
        else 
        {
                independents_numerical <- xxxx[[2]][[3]]
                joint_multicat_numerical_independents <- do.call(cbind, list(independents_multilevel, independents_numerical))
                # INVERT MULTICATS _ THEY ARE USED AS PREDICTOR IN MULTICAT ANOVAS INSTEAD OF BEING THE RESPONSES
                dependents_binary  <- binary_dependents
                multibanalyses <- send_responses_to_predictors_lm(responses_dataset = dependents_binary, predictors_dataset = independents_multilevel, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = TRUE)
                # INVERTED SECOND PART NONBINARIES
                multinanalyses <- send_responses_to_predictors_lm(responses_dataset = joint_multicat_numerical_independents, predictors_dataset = independents_multilevel, threshold_significance = 0.05, categorical_flag = FALSE, logit_binomial_flag = TRUE)
        }

        list(banalyses = banalyses, multibanalyses = multibanalyses, multinanalyses = multinanalyses, nanalyses = nanalyses)
        })
}



# SAVE THE LIST OF SIGNIFICANT RESULT  TO A YAML FILE
save_list_to_yaml <- function(significants_list = significants_list, resp_pred = resp_pred, number_of_levels = number_of_levels, threshold_significance = threshold_significance, variables_file = 'variables.txt')
{
        # MAKE RESULTS FILE NAME
        date_time <- format(Sys.time(), 'x%y%m%d_%Hh%Mm%Ss_')
        yaml_file_name <- paste0('xSIGNIFICAT_RESULTS', '_deped_', names(resp_pred[[1]])[1], '_independ_', names(resp_pred[[2]])[1], '_specs_file_', variables_file, '_levels', number_of_levels, '_signif', threshold_significance,  date_time, '.yaml')
        print(yaml_file_name)
        if(require('yaml')) write_yaml(significants_list, yaml_file_name)
}

#=================================================================
# GENERAL RUN ANALYSES, USING AUTOMATIC GLM MODEL SELECTION
#================================================================
run_autoselected_glm_analyses <- function(..., number_of_levels = 5, threshold_significance = 0.05, variables_file = 'variables.txt')
{
        arguments_list <- list(...)
        # DECISION ON NUMBER OF ATASETS.
        # WITH MORE THAM DATASET FEED THEM DIRECTLY TO THE ANALYSIS FUNCTION.
        if(length(arguments_list) > 1) resp_pred <- list(responses = ..1, predictors = ..2)
        # WITH ON DATASETS SEND TO THE SEPARATING FUNCTION AND THE RETURNED RESULTS IS THEN SEND TO THE ANALYSIS FUNCTION
        else resp_pred <- separate_responses_predictors_as_specified_in_variables_file(..1, variables_file = variables_file)

        # SEND THE LIST OF DATASETS TO THE ANALYZE FUNCTION
        significants_list  <- analyze_many_response_many_predictors_using_automatic_lm_selector(resp_pred[[1]], resp_pred[[2]], number_of_levels = number_of_levels, threshold_significance = threshold_significance)
        save_list_to_yaml(significants_list = significants_list, resp_pred = resp_pred, number_of_levels = number_of_levels, threshold_significance = threshold_significance, variables_file = 'variables.txt')
        return(significants_list)
}
