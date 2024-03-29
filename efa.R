#=========================================
#Identify qitems with too many unanswered questions
#=========================================
show_questions_with_xpercent_unanswered <- function(unanswered_percentage = 20)
{
	thresh <- round(length(items[,1]) * unanswered_percentage / 100, digits = 0)
	print(paste('COMPUTED PERCENT__', thresh))
	unnum <- sapply(names(items), function(qname)
		{
			nasum <- sum(is.na(items[, qname]))
			if(nasum > thresh) {print(paste(qname, '__Number of nonanwsers__', nasum)); return(qname)}
		}
	)
	return(unlist(unnum))
}


#=========================================
#Identify participats with too many unanswereds
#=========================================
participants_with_xpercent_unanswered <- function(unanswered_percentage = 20)
{
	thresh <- round(length(items[1,]) * unanswered_percentage / 100, digits = 0)
	print(paste('COMPUTED PERCENT__', thresh))
	unnum <- sapply(row.names(items), function(idparticipant)
		{
			nasum <- sum(is.na(items[idparticipant, ]))
			if(nasum > thresh) {print(paste(idparticipant, '__Number of nonanwsers__', nasum)); return(idparticipant)}
		}
	)
	return(unlist(unnum))
}


#=========================================
#remove too many unanswered questions and participants from df
#=========================================
off20percent_dataset <- function(qunanswered_percentage = 20, punanswered_percentage = 20, dataset = items)
{
	qtoo_many_unanswered_list <- show_questions_with_xpercent_unanswered(qunanswered_percentage)
	ptoo_many_unanswered_list <- participants_with_xpercent_unanswered(punanswered_percentage)
	return(dataset[!names(dataset) %in% ptoo_many_unanswered_list, !names(dataset) %in% qtoo_many_unanswered_list])
}


#================================================================
#MAKE PSYCH FA FIT FROM NUMBER OF FACTORS AND CLEANED DATASET
#================================================================
wls_promax_fa_fit_from_n_data <- function(nfactors = nfactors, cleaned_dataset = cleaned_dataset)
{
        psych::fa(cleaned_dataset, nfactors= nfactors, rotate='promax', fm='wls')
}

#=========================================
#COMPUTE psych fa ITEMS TO BE REMOVED
#=========================================
efa_items_to_keep_and_remove <- function(fit_psych_fa = fit_psych_fa)
{
        psych::print.psych(fit_psych_fa, sort=TRUE)
  load_matrix <- fit_psych_fa$loadings
  load_matrix[abs(load_matrix) < 0.30] <- NA
  okloadings_list_items    <- unlist(sapply(row.names(load_matrix),function(x)if(sum(!is.na(load_matrix[x, ])) == 1) {print(x); return(x)}))
  crossloadings_list_items <- unlist(sapply(row.names(load_matrix),function(x)if(sum(!is.na(load_matrix[x, ])) > 1) {print(x); return(x)}))
  noloadings_list_items    <- unlist(sapply(row.names(load_matrix),function(x)if(sum(!is.na(load_matrix[x, ])) < 1) {print(x); return(x)}))
  print(paste('N CROSS __', length(crossloadings_list_items)))
  print(paste('N NO LOAD __', length(noloadings_list_items)))
	list(okloadings_list_items = okloadings_list_items, crossloadings_list_items = crossloadings_list_items, noloadings_list_items = noloadings_list_items, load_matrix = load_matrix)
}


#=========================================
#Compute the number and proportion of subdimesion in a given vector of items
#=========================================
proportion_of_subdims_in_testing_list <- function(testing_items, scale_items = separated_subdim)
{
	tested_name <- deparse(substitute(testing_items))
	proportions_list <- lapply(scale_items,
		function(x)
		{
			shared_items <- intersect(x[[2]], testing_items)
			nshared <- length(shared_items)
			percent_shared <- round(nshared * 100 / length(testing_items), 1)
			print(paste('TESTED__ ', 'SUBSCALE__', x[[1]], 'N__', nshared, 'PERCENT__', percent_shared, 'SHARED ITEMS__'))
			return(list(tested_name, x[[1]], nshared, percent_shared, shared_items))
		}
	)
	proportionsdf <- do.call(rbind, proportions_list)
	names(proportionsdf) <- c('tested_set', 'subscale', 'num', 'percent', 'shared_items')
	return(proportionsdf)
}

nproportions_from_dims_spec_lists <- function(dims_spec_list, original_spec_list)
{
	nprop_list <- lapply(dims_spec_list, function(x)
		{
			neach_dim <- proportion_of_subdims_in_testing_list(x[[2]], original_spec_list)
			return(list(x[[1]], neach_dim))
		
		}
	)
	return(nprop_list)
}



#=========================================
#compute the list of items members of subdimension
#generate lists of pairs comprising 
#dimension name and the list of items members of such dimension
#=========================================
generate_new_spec_lists <- function(clean_matrix)
{
	new_spec_lists <- lapply(colnames(clean_matrix),
			function(x) 
			{
				list_items <- lapply(rownames(clean_matrix), function(y) if(!is.na(clean_matrix[y,x])) return(y))
				return(list(x, unlist(list_items)))
			}
		)
	return(new_spec_lists)
}



#=========================================
#=========================================
cronbachs_alpha_of_names_list <- function(alphaees_list, dataset)
{
	dfalphaee <- dataset[, alphaees_list, drop = FALSE]
        if(is.data.frame(dfalphaee) && length(dfalphaee) > 1) 
        {
	rep(print(strrep('I', 64)), 3)
	return(psych::alpha(dfalphaee, na.rm = TRUE, check.keys = TRUE, warnings = FALSE)$total$std.alpha)
        }
        else return(c(no_alpha = 0))
}

cronbachs_alpha_dims_items_list <- function(dims_items, dataset)
{
	rep(print(strrep('D', 64)), 3)
	alphas <- sort(unlist(sapply(dims_items, function(x)
		{
			cronbachs_alpha_of_names_list(x[[2]], dataset)
		}
	)), decreasing = TRUE)
	names(alphas) <- unlist(sapply(dims_items, function(x) x[[1]]))
	return(alphas)
}



#=================================================
# MAKE THE LAVAAN CFA SPECIFICATION TEXT FROM AN INPUT LOADINGS MATRIX
#=================================================
lavaan_spec_from_na_marked_loadings_matrix <- function(for_cfa_spec_items_above_minimal = for_cfa_spec_items_above_minimal)
{
    for_cfa_spec_items_above_minimal <- as.data.frame(for_cfa_spec_items_above_minimal)
    # MAKE MATRIX BOOLEAN
    for_cfa_spec_items_above_minimal[] <- !is.na(for_cfa_spec_items_above_minimal)

    item <- rownames(for_cfa_spec_items_above_minimal)
    # MAKE TEXT SPECIICATION FOR LAAVAN CFA

    dims_names <- names(for_cfa_spec_items_above_minimal)
    spec_lines <- lapply(dims_names, function(x)
        {
            summands <- item[for_cfa_spec_items_above_minimal[, x] ]
            summands <- paste(summands, sep = ' + ', collapse = ' + ')
            paste0(x, ' =~ ', summands)
        }
    )
    paste(spec_lines, collapse = '\n')
}


#=================================================
# TAKES A LOADINGS MATRIX AND COUNTS HOW MANY ITESMS ARE NOT NA IN EACH FACTOR
# ALSO GENERATES THE MATRIX FOR CFA SPEC, BY REMOVIN SUBDIMENSION WITH LESS THAN MINIMAL _7_ ITEMS
#=================================================
count_items_in_each_dimension <- function(countee_items_by_subdim, minimal_n_items = minimal_n_items)
{
	subdims_names <- dimnames(countee_items_by_subdim)[[2]]
	n_items_in_each_dim <- sort(sapply(subdims_names,
		function(subdimension) sum(!is.na(countee_items_by_subdim[, subdimension]))
		), decreasing = TRUE)
	n_less_than_min <- sum(n_items_in_each_dim < minimal_n_items , na.rm = TRUE)
	# GENERATES THE LOADINGS MATRIX TO MAKE THE LAVAAN CFA SPECIFICATION
	# MAKE LOAD MATRIX READY TO IMPORT TO A CFA SPECIFICATION
	# BY REMOVING THE COLUMNS THAT HAS LESS THAN MINIMAL _ 7 _ NUMBER OF ITEMS
	selector_more_than_minimal <- sapply(subdims_names, function(subdimension) sum(!is.na(countee_items_by_subdim[, subdimension])) >= minimal_n_items)
	rows_items_names <- dimnames(countee_items_by_subdim)[[1]]
	selector_rows <- sapply(rows_items_names, function(each_row) sum(!is.na(countee_items_by_subdim[each_row,])) > 0)
	for_cfa_spec_items_above_minimal <- countee_items_by_subdim[selector_rows, selector_more_than_minimal]
    # MAKE LAVAAN SPEC BY CALLING THE FUNCTION
    for_cfa_spec_items_above_minimal <- lavaan_spec_from_na_marked_loadings_matrix(for_cfa_spec_items_above_minimal)
	#JOIN AND NAME RESULTS
	list(n_items_in_each_dim = n_items_in_each_dim, n_less_than_min = n_less_than_min, for_spec = for_cfa_spec_items_above_minimal)
}



#================================================================
# COMPUTES THE ALPHAS AND THE REST OF THE TALLIES , CONTS AND AND STATISTICS OF THE EFAS
#================================================================
final_tally_stats_efa_alphas <- function(fit_psych_fa = fit_psych_fa, minimal_n_items = minimal_n_items, unanswered_cleaned_dataset = unanswered_cleaned_dataset)
{
        keepees_removees <- efa_items_to_keep_and_remove(fit_psych_fa)

	#TALY of items in each subdimension and how many of those subdimensions has less than a minimal amount of items _seven as default
	n_items_in_each_dim <- count_items_in_each_dimension(keepees_removees[[4]], minimal_n_items = 7)
	# ALSO GENERATES THE LOADINGS MATRIX TO MAKE THE LAVAAN CFA SPECIFICATION
	for_spec <- n_items_in_each_dim[['for_spec']]
	n_items_in_each_dim <- n_items_in_each_dim[1:2]


	#A matrix of loadings is generated from the keepees_removees list by selecting the items to be kept, element1, from the loadins matrix, element4
	clean_matrix <- keepees_removees[[4]][keepees_removees[[1]], ]
	new_spec_lists <- generate_new_spec_lists(clean_matrix)

	#number and percentage of each orifinal demension in each factor of the testes model
	nprop_dims_results <- nproportions_from_dims_spec_lists(dims_spec_list = new_spec_lists, original_spec_list = separated_subdim)

	#Cronbach's Alpha for each efa factor is computed and resturned as a list
	cbqitemsalphas <- cronbachs_alpha_dims_items_list(new_spec_lists[ ], unanswered_cleaned_dataset)
	print(cbqitemsalphas)

	# JOIN AND NAME THE RESULTS
	efa_alpha_results <- list(for_spec, keepees_removees, n_items_in_each_dim, nprop_dims_results, cbqitemsalphas)
	names(efa_alpha_results) <- c('for_spec', 'ok_cross_0_matrix', 'items_in_dim', 'percents', 'alphas')
	efa_alpha_results
}

#=========================================
#=========================================
# MAIN FUNCTIION
# List psych efa and alphas
# ALSO GENERATES THE LOADINGS MATRIX TO MAKE THE LAVAAN CFA SPECIFICATION
#=========================================
#=========================================
compute_efa_alpha <- function(nfactors = 15, dataset = imputed_notoo, minimal_n_items = 7)
{
	#remove too many unanswered questions and participants from df
	unanswered_cleaned_dataset <- off20percent_dataset(15, 20, dataset)

	#loadings_efa <- compute_efa_loadings(each_nfactors = nfactors, notoo) 
	#numdims <- proportion_of_subdims_in_testing_list(noloadings_list_items)
	#keepees_removees is a list which contains 
	#1_ the ok items, 2_ the crossloaded items, 3_ the noloading items, and 4_ the matrix cut off at 0.3
fit_psych_fa <- wls_promax_fa_fit_from_n_data(nfactors = nfactors, cleaned_dataset = dataset)
        final_tally_stats_efa_alphas(fit_psych_fa, minimal_n_items = minimal_n_items)
}

