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
# TAKES AS INPUT A COLUMN TO CHECK _A VECTOR_ AND THE HIGHEST NUMBER THAT BE CONSIDERED AS CATEGORIC _INTEGER_
# ITS AIM IS TO HELP TO SELECT COLUMS IN A DATASET THAT ENCODE CATEGORICAL VARIABLES USING INTEGERS
has_only_n_integers <- function(checkee_column, number_integers = 5)
{
    print(names(checkee_column))
    print(head(checkee_column))
        set_of_integers <- c(0 : number_integers)
    print(set_of_integers)
        all(checkee_column %in% set_of_integers, na.rm = TRUE)
}

# COMPUTES WHETHER THE SUPPLIED COLUM IS BINARY _ THAT IS _ IS CATEGORICAL WITH ONLY TWO LEVELS
# TAKES AS INPUT A COLUMN 
# RETURNS TRUE IF THE COLUMN IS BINARY, FALSE OTHERWISE
is_binary_factor <- function(binaryee_factor_column)
{
    binaryee_factor_column <- as.factor(binaryee_factor_column)
    number_of_levels <- length(levels(binaryee_factor_column))
    return(number_of_levels == 2)
}

# SEPARATES A DATASET INTO CATEGORICAL AND NUMERIC VARIABLES
# EVEN IF THE CATEORICAL VARIABLES ARE ENCODED AS INTEGERS
# IT INVOKES THE has_only_n_integers FUNCTION
# TAKES AS INPUT THE DATASET TO SPLIT AND THE HIGHER NUMBER THAT WILL BE CONSIDERED AS CATEGORIC _SUCH NUMBER IS CARRIED TO THE INVIKED FUNCTION
split_categoric_numeric <- function(splitee_dataset, number_integers = 5)
{
        if(length(splitee_dataset) < 1)
        {
                binary_dataset <- splitee_dataset
                categorical_dataset <- splitee_dataset
                numeric_dataset <- splitee_dataset
        }
        else
        {
            selector_categorical <- sapply(splitee_dataset, function(x){is.factor(x) || has_only_n_integers(x, number_integers) || is.character(x)})
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
separate_all_input_dataset <- function(variables_file = 'variables.txt', ...)
{
read_variables <- unlist(readLines(variables_file))
        datasets <- list(...)
        #datasets <- lapply(datasets, function(dset)ifelse(class(dset) != 'data.frame', as.data.frame(dset), dset))
        all_separated_datasets <- lapply(datasets,
                 function(dset){
                        names_vectors <- user_variables_splitter(read_variables)
 print(names_vectors)
                        responses_predictors_datasets_list <- lapply(names_vectors, function(each_nv){split_categoric_numeric(dset[names(dset) %in% each_nv]) })
#print(length(responses_predictors_datasets_list))
                        #by_categorical_responses_predictors_datasets_list <- lapply(responses_predictors_datasets_list, function(x){ x <- ifelse(class(x) == 'list', data.frame(unlist(x)), data.frame(x)) })
 #print(by_categorical_responses_predictors_datasets_list)
                        #names(by_categorical_responses_predictors_datasets_list)  <- c('responses_split', 'predictors_split')
                        #return(by_categorical_responses_predictors_datasets_list)
        })
        return(all_separated_datasets)
}

