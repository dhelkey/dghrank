probitK<-function(minimal_data,num_cat,num_cont, returned_iters = 500, burn = 25,
prior_var = 10)
{
	intvar = groupvar = one_way_var = two_way_var = cont_var = prior_var

	
  #numper of iterations the sampler will have to run
  iters=burn+returned_iters
  #create lm formula to fit frequentist model (and the model matrix)
  
  
  minimal_data[ ,2] = as.factor(minimal_data[ ,2])
  
  
  namer=paste0(names(minimal_data)[3:(3+num_cat-1)],sep="+",collapse = "")
  namer=substr(namer,1,nchar(namer)-1)
  formulaa=paste(names(minimal_data)[1],'~',names(minimal_data)[2],"+ (",namer,")^2")
  #handle continuous
  if(num_cont>0)
  {
    cont_addition=paste0(names(minimal_data)[(3+num_cat):length(names(minimal_data))],sep="+",collapse = "")
    cont_addition=substr(cont_addition,1,nchar(cont_addition)-1)
    formulaa=paste(formulaa,'+',cont_addition)
  }
  if(num_cat+num_cont<1)
  {
    formulaa=paste0(names(minimal_data)[1],'~',names(minimal_data)[2])
  }
    frequentist_model=glm(formula(formulaa),family=binomial(link="logit"),data=minimal_data)
  
  

  
  #extract the parameter estimates for the hospitals / subunits, hard code first to zero to take into account reference level
  ests=c(0,coefficients(frequentist_model)[which(substr(names(coefficients(frequentist_model)),1,6)==names(minimal_data)[2])])
  

  
  #same for covariance
  covv=vcov(frequentist_model)[1:(length(ests)),1:(length(ests))]
  covv[1,]=0
  covv[,1]=0
  
  #reparameterize as described in D-G paper
  xform_mat=diag(1,length(ests))-replicate(length(ests),table(minimal_data[,2]))/sum(table(minimal_data[,2]))
  new_ests=c(ests%*%xform_mat)
  new_cov_mat=t(xform_mat)%*%covv%*%xform_mat
  z_frequentist=new_ests/sqrt(diag(new_cov_mat))
  
  #bayesian model, first extract model matrix
  xx=model.matrix(frequentist_model)
  yy=as.numeric(as.character(eval(parse(text=paste0("minimal_data$",fun=names(minimal_data)[1])))))
  num_group_coef=length(table(minimal_data[,2]))-1
  
  num_2way=dim(xx)[2]-dim(model.matrix(glm(gsub("^2",'',formulaa,fixed=TRUE),family=binomial(link="logit"),data=minimal_data)))[2]
  num_1way=dim(xx)[2]-(1+num_group_coef+num_2way+num_cont)
  n=length(yy)
  p=dim(xx)[2]
  
  #create limits for the gibbs sampler truncated normals
  lower=rep(-Inf,n)*(-(yy-1))
  lower[is.nan(lower)]<-0
  upper=rep(Inf,n)*yy
  upper[is.nan(upper)]<-0
    
 #initialize storage and get initial values
  mcmc_betas=matrix(0,iters,p)
  mcmc_betas[1,]=coefficients(frequentist_model)
  colnames(mcmc_betas)=names(coefficients(frequentist_model))
  mcmc_latent=truncnorm::rtruncnorm(n,lower,upper,xx%*%mcmc_betas[1,])

  #priors, specified by macro
  priormean=rep(0,p)
  priorcov=c(rep(intvar,1),rep(groupvar,num_group_coef),rep(one_way_var,num_1way),rep(two_way_var,num_2way))
  if(num_cont>0)
  {
    priorcov=c(priorcov,rep(cont_var,num_cont))
  }
  
  #pre calculate some stuff from data to make regression faster
  XpX=crossprod(xx)
  IR=backsolve(chol(XpX/1+diag(1/priorcov)),diag(p))
  sigmasq=1
  
  #MCMC: probit gibbs
  for(i in 2:iters)
  {
    #first perform regression on latent normals
    Xpy=crossprod(xx, as.matrix(mcmc_latent))
    btilde=crossprod(t(IR))%*%(Xpy/sigmasq+diag(1/priorcov)%*%priormean)
    mcmc_betas[i,] = t(replicate(1,as.vector(btilde))) + rnorm(p)%*%t(IR)
    #then conditional on the regression coefficients, generate truncated normals
    mcmc_latent=truncnorm::rtruncnorm(n,lower,upper,xx%*%mcmc_betas[i,])
  }
  #delete burn in section
  mcmc_betas=mcmc_betas[(burn+1):iters,]
  #now reparameterize for d-g paper, but using the bayesian estimate of the covariance of the coefficients
  
  #estimate of covariance between the subunits, last row is all zero for effect coded category
  covv=cov(mcmc_betas)[1:(length(ests)),1:(length(ests))]
  covv[1,]=0
  covv[,1]=0
  
  #calculate new xform and covariance matrix to reparameterize
  xform_mat=diag(1,length(ests))-replicate(length(ests),table(minimal_data[,2]))/sum(table(minimal_data[,2]))
  new_cov_mat=t(xform_mat)%*%covv%*%xform_mat

  #extract the coefficients for the subunits
  tempp=(mcmc_betas[,1:length(ests)])
  #code first to zero for our dummy coded category
  tempp[,1]=0
  #uncertainty bands for our z scores plus a point estimate at the median
  est_matrix=tempp%*%xform_mat
  z_mat=est_matrix%*%diag(sqrt(1/(diag(new_cov_mat))))
  bayesian_z_ests=cbind(group_label=names(table(minimal_data[,2])),z_score=apply(z_mat, 2, quantile, probs = c(.5),  na.rm = TRUE))
  colnames(est_matrix)=paste0("group_",names(table(minimal_data[,2])))
  
  #create data to return
  returner=list(
  column_names=names(minimal_data)[1],
  mcmc_betas=mcmc_betas,
  z_matrix=z_mat,
  frequentist_model=frequentist_model,
  z_frequentist=z_frequentist,
  minimal_dataset=minimal_data,
  regression_formula=formulaa,
  bayesian_z_ests=bayesian_z_ests,
  group_labels=names(table(minimal_data[,2])),
  bayesian_ests=est_matrix,
  model_matrix = xx,
  Z= as.numeric(bayesian_z_ests[ ,2]),
  xform_mat = xform_mat
  )
  return(returner)
}