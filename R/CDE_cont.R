CDE_cont <-
function(thetas, treatment, mediator, m=0, a_old=1, a_new=0){
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
  cde <- (thetas[treatment]+interactionTerm*m)*(a_old-a_new)
  unname(cde)
}
