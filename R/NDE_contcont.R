NDE_contcont <-
function(betas, thetas, treatment, mediator, covariates, a_old=1, a_new=0){
  covariatesTerm <- 0
  for(c in covariates){
    covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  

  nde <- (thetas[treatment]+interactionTerm*betas[1]+interactionTerm*betas[treatment]*a_new+interactionTerm*covariatesTerm)*(a_old-a_new)
  unname(nde)
}
