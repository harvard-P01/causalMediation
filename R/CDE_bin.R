CDE_bin <-
function(thetas, treatment, mediator, m=0, a_old=1, a_new=0){
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
  ORcde <- exp((thetas[treatment]+interactionTerm*m)*(a_old-a_new))
  unname(ORcde)
}
