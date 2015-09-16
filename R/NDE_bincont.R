NDE_bincont <-
function(betas, thetas, treatment, mediator, covariates, a_old=1, a_new=0){
  covariatesTerm <- 0
  for(c in covariates){
    covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
  }
  
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
  
  nde <- thetas[treatment]*(a_old-a_new)+interactionTerm*(a_old-a_new)*(exp(betas[1]+betas[treatment]*a_new+covariatesTerm)/
																	 (1+exp(betas[1]+betas[treatment]*a_new+covariatesTerm)))

  unname(nde)
}
