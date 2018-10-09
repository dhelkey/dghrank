processMinimalData = function(minimal_data, num_cat, num_cont, subset = FALSE,
		verbose = TRUE, 
		outcome.na = 'set0', 
		subset.na = 'category', 
		cat.na = 'category', 
		cont.na = 'median'){
	#Processes the minimal_data input
	#Output will have NO NA values
	#control
	#outcome.na: remove/set0
	#subset.na: remove/category
	#cat.na: remove/category
	#cont.na: remove/median #Todo add/randomize 

	#Count total records
	N_full = dim(minimal_data)[1]
	
	minimaldataAlert = function(N_old, msg = ''){
		#Counts how many observations in minimal_data to alert when observations are discarded
		#Inherits minimal_data and verbose from its environment
		N = dim(minimal_data)[1]
		if (N < N_old){
			if (verbose){ 
					message(N_old - N, ' observations removed due to ',msg, ' NA values')
			}
		}
		return(N)
	}
	
	#Sort by institution
	minimal_data = minimal_data[order(minimal_data[ ,2]), ]
	if (sum(!complete.cases(minimal_data[ ,2])) > 0){stop('Missing values for institution not allowed')}

	#If subsetting by a variable (e.g. race)
	var_start_index = 3
	subset_vec = NULL
	if (subset){
		if (subset.na == 'remove'){
			minimal_data = minimal_data[complete.cases(minimal_data[ ,3]), ]
		}
		subset_vec = as.numeric(as.character(minimal_data[ ,3])) 
		subset_vec[!complete.cases(subset_vec)] = 99 
		#subset category code
        subset_vec = as.factor(subset_vec)
        var_start_index = 4
    }
	unique_subset_vec = NULL
	if (subset){unique_subset_vec = sort(unique(subset_vec)) }
			
	#Handle NA outcome variables
    if (outcome.na == 'remove'){
		minimal_data = minimal_data[complete.cases(minimal_data[ ,1]), ]
	} else if (outcome.na == 'set0'){
		minimal_data[!complete.cases(minimal_data[ ,1]),1] = 0
	}
	N_new = minimaldataAlert(N_full, msg = 'outcome')
	
	#Categorical risk adjusters
	cat_var_mat = cat_var_locat = NULL
	if (num_cat > 0){
		cat_var_locat = var_start_index:(var_start_index + num_cat - 1)	
		#Remove NA categoricals
		if (cat.na == 'remove'){
			minimal_data = minimal_data[
				complete.cases(minimal_data[ ,cat_var_locat]), ]
		}
	}
	N_new = minimaldataAlert(N_new, msg = 'categorical')
	
	#Continuous risk adjusters
	imputeFun = function(x){
	  if (is.numeric(x)){
		x[!complete.cases(x)] = median(x, na.rm = TRUE)
		}
		return(x)
	}

    cont_var_mat = NULL
    if (num_cont > 0){
		cont_var_locat = (var_start_index + num_cat):(var_start_index + num_cat + num_cont - 1)
		if (cont.na == 'remove'){
			minimal_data = minimal_data[
				complete.cases(minimal_data[ ,cont_var_locat]), ]
		}
        cont_var_mat = as.data.frame(minimal_data[  ,cont_var_locat])
		if (cont.na == 'median'){
			cont_var_mat = cbind(sapply(cont_var_mat, imputeFun))
		}
		colnames(cont_var_mat) = names(minimal_data)[cont_var_locat]
    }
	N_new = minimaldataAlert(N_new, msg = 'continious')
		
	#This is done last, after we deal with all NA values
	#Extract categoricals as a matrix and explicitly turn into a factor
	if (num_cat > 0){
		cat_var_mat = minimal_data[  ,cat_var_locat]
		cat_var_mat = data.frame(sapply(cat_var_mat, as.character), stringsAsFactors = FALSE)
		if (cat.na == 'category'){
			cat_var_mat[is.na(cat_var_mat)] = '99'
		}

		colnames(cat_var_mat) = names(minimal_data)[cat_var_locat]
		#Explicitly turn each categorical into a factor  
		for (i in 1:num_cat){
		   cat_var_mat[ ,i] = as.factor(cat_var_mat[ ,i] )
		}
	}

	#Extract variables
	inst_vec = as.factor(minimal_data[ ,2])
	y = minimal_data[ ,1]
	
	#Return object
	return(list(
		indicator_name = names(minimal_data)[1],
		o_overall = mean(y),
		y = y,
		N = length(y),
		p = length(unique(inst_vec)),
		inst_vec = inst_vec, 
		subset_vec = subset_vec,
		cat_var_mat = cat_var_mat,
		cont_var_mat = cont_var_mat,
		unique_inst_vec = unique(minimal_data[ ,2])))
}
