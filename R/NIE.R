NIE_binbin <-
  function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                              0,
                              thetas[paste(treatment, mediator, sep = ':')])  
    ORnie <- (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm) *
                (1 + exp(thetas[mediator]+interactionTerm * a + betas[1] + betas[treatment] * a + covariatesTerm)))/
      ((1+exp(betas[1]+betas[treatment]*a+covariatesTerm))*(1+exp(thetas[mediator]+interactionTerm*a+betas[1]+betas[treatment]*a_star+covariatesTerm)))
    unname(ORnie)
  }

NIE_bincont <-
  function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
    
    nie <- (thetas[mediator]+interactionTerm*a) * 
      (exp(betas[1] + betas[treatment] * a + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a + covariatesTerm)) -
         exp(betas[1] + betas[treatment] * a_star + covariatesTerm) / (1+exp(betas[1] + betas[treatment] * a_star + covariatesTerm)))
    unname(nie)
  }

NIE_contbin <-
  function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                              0,
                              thetas[paste(treatment, mediator, sep = ':')])
    
    ORnie <- exp((thetas[mediator]*betas[treatment]+interactionTerm*betas[treatment]*a)*(a - a_star))
    unname(ORnie)
  }

NIE_contcont <-
  function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE){
    covariatesTerm <- 0
    for(c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
    }
    interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                              0,
                              thetas[paste(treatment, mediator, sep=':')])  
    
    nie <- (thetas[mediator]*betas[treatment]+interactionTerm*betas[treatment]*a_star) * (a - a_star)  
    unname(nie)
  }
