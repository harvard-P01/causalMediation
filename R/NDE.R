NDE_binbin <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
    }
    
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                              0, thetas[paste(treatment, mediator, sep=':')])  
    
    ORnde <- (exp(thetas[treatment]*a) * (1+exp(thetas[mediator] +interactionTerm*a+betas[1]+betas[treatment]*a_star+covariatesTerm)))/
      (exp(thetas[treatment]*a_star)*(1+exp(thetas[mediator]+interactionTerm*a_star+betas[1]+betas[treatment]*a_star+covariatesTerm)))
    unname(ORnde)
  }

NDE_bincont <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
    }
    
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
    
    nde <- thetas[treatment]*(a-a_star)+interactionTerm*(a - a_star)*(exp(betas[1]+betas[treatment]*a_star+covariatesTerm)/
                                                                            (1+exp(betas[1]+betas[treatment]*a_star+covariatesTerm)))
    
    unname(nde)
  }

NDE_contbin <- function(betas, thetas, treatment, mediator, covariates, variance, a_star = 0, a = 1, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                              0,
                              thetas[paste(treatment, mediator, sep=':')])  
    
    ORnde <- exp((thetas[treatment]+interactionTerm * (betas[1] + betas[treatment]*a_star+covariatesTerm+thetas[mediator]*variance))*(a-a_star)+
                   0.5 * interactionTerm ^ 2 * variance*(a ^ 2 - a_star ^ 2))
    
    unname(ORnde)
  }

NDE_contcont <- function(betas, thetas, treatment, mediator, covariates, a_star = 1, a = 0, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                              0, thetas[paste(treatment, mediator, sep = ':')])  
    
    nde <- (thetas[treatment] + interactionTerm * betas[1] + interactionTerm * betas[treatment] * a_star + interactionTerm*covariatesTerm)*(a - a_star)
    unname(nde)
  }
