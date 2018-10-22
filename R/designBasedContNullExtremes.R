designBasedContNullExtremes = function(gamma_vec, outcome_vec, pcf_cat_vec, instid_vec, mode = 'global'){
	#Does either global or local...use to verify results
	#' Compute Design Based Draper-Gittoes Z-Scores for Null Simulations
	#'
	#'	\code{designBased} implements design based approach and
	#' computes O, E, and z vectors following Draper-Gittoes (2004)
	#'
	#' @param gamma_vec Vector with elements in [0,1] controling pooling.
	#' gamma_vec = 1 is complete global pooling, gamma_vec = 0 is complete local pooling.
	#' @param outcome_vec Vector with elements in {0,1} denoting the binary outcome.
	#' @param pcf_cat_vec Vector with unique values for each PCF category.
	#' @param instid_vec Vector with unique values for each institution.
	
	
	###Return a p x G matrix (p number of insts, G number of gamma_vec_vec entries)

	#Some of the inputs need to be factors
	pcf_cat_vec = as.factor(pcf_cat_vec)
	instid_vec = as.factor(instid_vec)
	instid_vec_i = as.integer(instid_vec)
	pcf_cat_vec_i = as.integer(pcf_cat_vec)

	#Summarize
	instid_unique = levels(instid_vec)
	instid_unique_i = as.integer(instid_unique)
	n_students = length(outcome_vec)
	n_u = length(levels(instid_vec))
	unique_cat = levels(pcf_cat_vec)
	n_cat = length(unique_cat)
	p_global = mean(outcome_vec)

	#Compute Table 2 in Draper-Gittoes
	y_mat = n_mat = s_mat = matrix(0, nrow =n_u, ncol = n_cat)

	for (i in 1:n_students){
    inst = as.integer(instid_vec[i])
    categ = as.integer(pcf_cat_vec[i])
    n_mat[inst,categ] = n_mat[inst,categ] + 1
    y_mat[inst, categ] = y_mat[inst, categ] + outcome_vec[i]
	}
	ybar_mat = y_mat / n_mat
	ybar_mat[n_mat == 0] = 0

	s_global = var(outcome_vec)
	for (i in 1:n_cat){
		for (j in 1:n_u){
			outcomes = outcome_vec[pcf_cat_vec_i == i & instid_vec_i == j]
			if (length(outcomes) > 1){
				s_mat[j,i] = var(outcomes)
				}
		}
	}

	#Compute marginals
	pcf_ybar_vec = apply(y_mat, 2, sum) / apply(n_mat, 2, sum)
	n_u_vec = apply(n_mat, 1, sum)
	n_cat_vec = apply(n_mat, 2, sum)

	#Equation (2) in Draper-Gittoes (2004)
	O = apply(y_mat, 1, sum) / apply(n_mat, 1, sum)
	#Equation (3) in D-G
	E = apply(t(t(n_mat) * pcf_ybar_vec), 1, sum) / n_u_vec
	#Equation (4) in D-G
	D = O - E

	##Now compute variance
	lambdaFun = function(i,k,j){
		#Equation (7) in D-G. #Unchanged for continious
		if (i == k){
			return(n_mat[i,j]/n_u_vec[i] * (1 - (n_mat[i,j]/n_cat_vec[j])))
		} else{
			return( -1 * (n_mat[i,j] * n_mat[k,j]) / (n_u_vec[i] * n_cat_vec[j]) )
		}
	}
	vFun = function(k,j, gamma_vec = c(0,.5,1)){
		#Equation (11) in D-G #Changed for continous
		n = n_mat[k,j]
		pcomb = function(p_local){gamma_vec * p_global + (1-gamma_vec) * p_local}
		if (n > 0){
			s_local = s_mat[k,j]
			s_use = gamma_vec * s_global + (1-gamma_vec) * s_local
			return(s_use/n)
		} else {
			return( rep(0, length(gamma_vec)))
		}
	}
	computeVi = function(i, gamma_vec){
		#Equation (8) in D-G #Unchanged for cnontnious
		v = rep(0, length(gamma_vec))
		for (k in 1:n_u){
			for (j in 1:n_cat){
				v = v + lambdaFun(i,k,j)^2 * vFun(k, j, gamma_vec)
			}
		}
		return(v)
	}
	# #Now compute w/ lapply
	V = do.call('cbind',lapply(1:n_u, computeVi, gamma_vec))
	
	#REturn SE mat
	return( ( matrix(O, nrow = length(gamma_vec), ncol = n_u, byrow = TRUE) -
			  matrix(E, nrow = length(gamma_vec), ncol = n_u, byrow = TRUE)) /
				sqrt(V))
}
