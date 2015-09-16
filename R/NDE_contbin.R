NDE_contbin <-
function(betas, thetas, treatment, mediator, covariates, variance=1., a_old=1, a_new=0){
  covariatesTerm <- 0
  for(c in covariates){
    covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
  
  ORnde <- exp((thetas[treatment]+interactionTerm*(betas[1]+betas[treatment]*a_new+covariatesTerm+thetas[mediator]*variance))*(a_old-a_new)+
               0.5*interactionTerm^2*variance*(a_old^2-a_new^2))
  
  unname(ORnde)
}
