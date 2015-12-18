NDE_binbin <- function(betas, thetas, treatment, mediator, covariates, cval,
                       a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  if (is.null(cval)) {
    for (c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
    }
  } else {
    for (i in 1:length(covariates)) {
      covariatesTerm <- covariatesTerm + betas[covariates[i]] * cval[i]
    }
  }
  
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                            0, thetas[paste(treatment, mediator, sep=':')])  
  
  ORnde <- (exp(thetas[treatment]*a) * (1+exp(thetas[mediator] +interactionTerm*a+betas[1]+betas[treatment]*a_star+covariatesTerm)))/
    (exp(thetas[treatment]*a_star)*(1+exp(thetas[mediator]+interactionTerm*a_star+betas[1]+betas[treatment]*a_star+covariatesTerm)))
  unname(ORnde)
}

NDE_bincont <- function(betas, thetas, treatment, mediator, covariates, cval,
                        a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  if (is.null(cval)) {
    for (c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
    }
  } else {
    for (i in 1:length(covariates)) {
      covariatesTerm <- covariatesTerm + betas[covariates[i]] * cval[i]
    }
  }
  
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),0,thetas[paste(treatment, mediator, sep=':')])  
  
  nde <- thetas[treatment]*(a-a_star)+interactionTerm*(a - a_star)*(exp(betas[1]+betas[treatment]*a_star+covariatesTerm)/
                                                                      (1+exp(betas[1]+betas[treatment]*a_star+covariatesTerm)))
  
  unname(nde)
}

NDE_contbin <- function(betas, thetas, treatment, mediator, covariates, cval,
                        variance, a_star = 0, a = 1, interaction = TRUE){
  covariatesTerm <- 0
  if (is.null(cval)) {
    for (c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
    }
  } else {
    for (i in 1:length(covariates)) {
      covariatesTerm <- covariatesTerm + betas[covariates[i]] * cval[i]
    }
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  ORnde <- exp((thetas[treatment]+interactionTerm * (betas[1] + betas[treatment]*a_star+covariatesTerm+thetas[mediator]*variance))*(a-a_star)+
                 0.5 * interactionTerm ^ 2 * variance*(a ^ 2 - a_star ^ 2))
  
  unname(ORnde)
}

NDE_contcont <- function(betas, thetas, treatment, mediator, covariates, cval,
                         a_star = 1, a = 0, interaction = TRUE){
  covariatesTerm <- 0
  if (is.null(cval)) {
    for (c in covariates){
      covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
    }
  } else {
    for (i in 1:length(covariates)) {
      covariatesTerm <- covariatesTerm + betas[covariates[i]] * cval[i]
    }
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0, thetas[paste(treatment, mediator, sep = ':')])  
  
  nde <- (thetas[treatment] + interactionTerm * betas[1] + interactionTerm * betas[treatment] * a_star + interactionTerm*covariatesTerm)*(a - a_star)
  unname(nde)
}


NDE_contcont_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=TRUE
  #debug=TRUE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  f <- "x2"
  if(interaction){
    f <- paste0("(", f, " + x", k, "*x", k+1, " + x", k, "*x", k+2, "*a_star + ")
    
    fc <- paste0("x", k + 2 + 1:j, "  * ", "vecc_", 1:j, collapse = " + ")
    fc <- paste0("x",k," *(", fc, ")")
    
    f <- paste0(f, fc, ")")
  }
  f <- paste0(" ~ ", f, "*(a-a_star)")
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NDE_contcont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  return(as.formula(f))
}

### FIXME: NDE_contbin_delta is a though one... ongoing work
NDE_contbin_delta <- function(thetas, vecc, variance, interaction = TRUE, debug=FALSE) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  thetas <- c(1,2,3,4)
  vecc   <- c(1,2)
  variance
  interaction=TRUE
  debug=TRUE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  f <- "exp((x2"
  if(interaction){
    f <- paste0(f,)
    f <- paste0(f, "*(a-a_star))")
    f <- paste0(f, "+") ### FIXME: add terms
  }else{
    f <- paste0(f,")")
    f <- paste0(f, "*(a-a_star))")
  }
  
  f <- paste0(" ~ ", f)
  f
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NDE_contbin_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  return(as.formula(f))
}

NDE_bincont_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=TRUE
  #debug=TRUE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  f <- "~ x2 * (a-a_star)"
  if(interaction){
    f <- paste0(f, " + (x",k," * (a-a_star)")
    
    # Numerator
    N2 <- paste0("x", k+1:2, collapse = " + " )
    N2 <- paste0("exp(", N2, "*a_star + ")
    N2 <- paste(N2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
    # Denominator
    D2 <- paste0("x", k+1:2, collapse = " + " )
    D2 <- paste0("1 + exp(", D2, "*a_star + ")
    D2 <- paste(D2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
    D2 <- paste0("(", D2, ")")
    
    F2 <- paste0("(",N2,"/",D2,")")
    
    f <- paste0(f,"*",F2, ")")
    
  }
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NDE_bincont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  return(as.formula(f))
}

NDE_binbin_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=FALSE
  #debug=TRUE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  # First term in nominator
  N1 <- "exp(x2*a)"
  
  # Second term in nominator
  if(interaction){
    N2 <- paste0("(1+exp(x3 + x",k,"*a + x",k+1," + x", k+2, "*a_star + ", paste0("x", k + 2 + 1:j, "*", "vecc_", 1:j, collapse = " + "), "))")
  }else{
    N2 <- paste0("(1+exp(x3 + x",           k+1," + x", k+2, "*a_star + ", paste0("x", k + 2 + 1:j, "*", "vecc_", 1:j, collapse = " + "), "))")
  }
  
  # Construct nominator
  N <- paste(N1, N2, sep="*")
  
  # First term in denominator
  D1 <- "exp(x2*a_star)"
  
  # Second term in denominator
  if(interaction){
    D2 <- paste0("(1+exp(x3 + x",k,"*a_star + x",k+1," + x", k+2, "*a_star + ", paste0("x", k + 2 + 1:j, "*", "vecc_", 1:j, collapse = " + "), "))")
  }else{
    D2 <- paste0("(1+exp(x3 + x",                k+1," + x", k+2, "*a_star + ", paste0("x", k + 2 + 1:j, "*", "vecc_", 1:j, collapse = " + "), "))")
  }
  
  # Construct denominator
  D <- paste(D1, D2, sep="*")
  
  # Construct formula
  f <- paste0(" ~ (",N,"/",D,")")
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NDE_bincont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  return(as.formula(f))
}

