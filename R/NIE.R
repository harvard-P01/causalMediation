total_NIE_binbin <- function(betas, thetas, treatment, mediator, covariates, cval,
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
  
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  ORnie <- (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm) *
              (1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a + covariatesTerm)))/
    ((1 + exp(betas[1] + betas[treatment]*a+covariatesTerm))*(1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(ORnie)
}

pure_NIE_binbin <- function(betas, thetas, treatment, mediator, covariates, cval,
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
  
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  ORnie <- (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm) *
              (1 + exp(thetas[mediator] + interactionTerm * a_star + betas[1] + betas[treatment] * a + covariatesTerm)))/
    ((1 + exp(betas[1] + betas[treatment]*a+covariatesTerm))*(1 + exp(thetas[mediator] + interactionTerm * a_star + betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(ORnie)
}


total_NIE_bincont <- function(betas, thetas, treatment, mediator, covariates, cval,
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
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator]+interactionTerm*a) *
    (exp(betas[1] + betas[treatment] * a + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a + covariatesTerm)) -
       exp(betas[1] + betas[treatment] * a_star + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(nie)
}

pure_NIE_bincont <- function(betas, thetas, treatment, mediator, covariates, cval,
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
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator]+interactionTerm*a_star) *
    (exp(betas[1] + betas[treatment] * a + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a + covariatesTerm)) -
       exp(betas[1] + betas[treatment] * a_star + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(nie)
}

total_NIE_contbin <- function(betas, thetas, treatment, mediator, covariates, cval,
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
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  
  ORnie <- exp((thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a) * (a - a_star))
  unname(ORnie)
}

pure_NIE_contbin <- function(betas, thetas, treatment, mediator, covariates, cval,
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
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  
  ORnie <- exp((thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a_star) * (a - a_star))
  unname(ORnie)
}

total_NIE_contcont <- function(betas, thetas, treatment, mediator, covariates, cval,
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
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a) * (a - a_star)  
  unname(nie)
}

pure_NIE_contcont <- function(betas, thetas, treatment, mediator, covariates, cval,
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
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a_star) * (a - a_star)  
  unname(nie)
}

total_NIE_contcont_delta <- function(thetas, interaction=TRUE, debug=FALSE, a_star = 0, a = 1){
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #interaction=FALSE
  
  k <- length(thetas)
  
  F1 <- paste0("(x3 * x", k+2)
  F2 <- ")"
  F3 <- " * (a-a_star)"
  
  if(interaction){
    F2 <- paste0(" + x", k, " * x", k+2, " * a)")
  }
  
  f = paste0(" ~ ", F1, F2, F3)
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_contcont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: formula = ", f))
  }  
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

pure_NIE_contcont_delta <- function(thetas, interaction=TRUE, debug=FALSE, a_star = 0, a = 1){
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #interaction=FALSE
  
  k <- length(thetas)
  
  F1 <- paste0("(x3 * x", k+2)
  F2 <- ")"
  F3 <- " * (a - a_star)"
  
  if(interaction){
    F2 <- paste0(" + x", k, " * x", k+2, " * a_star)")
  }
  
  f = paste0(" ~ ", F1, F2, F3)
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_contcont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: formula = ", f))
  }  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

total_NIE_contbin_delta <- function(thetas, interaction=TRUE, debug=FALSE, a_star = 0, a = 1){
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #interaction=TRUE
  
  k <- length(thetas)
  
  F1 <- paste0("exp((x3 * x", k+2)
  F2 <- ")"
  F3 <- " * (a-a_star))"
  
  if(interaction){
    F2 <- paste0(" + x", k, " * x", k+2, " * a)")
  }
  
  f = paste0(" ~ ", F1, F2, F3)
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_contbin_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

pure_NIE_contbin_delta <- function(thetas, interaction=TRUE, debug=FALSE, a_star = 0, a = 1){
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #interaction=TRUE
  
  k <- length(thetas)
  
  F1 <- paste0("exp((x3 * x", k+2)
  F2 <- ")"
  F3 <- " * (a-a_star))"
  
  if(interaction){
    F2 <- paste0(" + x", k, " * x", k+2, " * a_star)")
  }
  
  f = paste0(" ~ ", F1, F2, F3)
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_contbin_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: formula = ", f))
  }

  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

total_NIE_bincont_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE, a_star = 0, a = 1) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=FALSE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  F1 <- paste0("(x3 + x", k, "*a)")
  
  # First numerator
  N1 <- paste0("x", k+1:2, collapse = " + " )
  N1 <- paste0("exp(", N1, "*a + ")
  N1 <- paste(N1, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  
  # Second numerator
  N2 <- paste0("x", k+1:2, collapse = " + " )
  N2 <- paste0("exp(", N2, "*a_star + ")
  N2 <- paste(N2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  
  # First denominator
  D1 <- paste0("x", k+1:2, collapse = " + " )
  D1 <- paste0("1 + exp(", D1, "*a + ")
  D1 <- paste(D1, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  D1 <- paste0("(", D1, ")")
  
  # Second denominator
  D2 <- paste0("x", k+1:2, collapse = " + " )
  D2 <- paste0("1 + exp(", D2, "*a_star + ")
  D2 <- paste(D2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  D2 <- paste0("(", D2, ")")
  
  # Construct formula
  f <- paste0(" ~ ", F1, " * ( (", N1, "/", D1, ") - (", N2, "/", D2, ") )")
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_bincont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  
  for (i in 1:j){
    ss <- stringr::str_replace_all(s, paste("vecc", i, sep = "_"), vecc[i])
  }
  
  return(as.formula(ss))
}

pure_NIE_bincont_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE, a_star = 0, a = 1) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=FALSE
  
  j <- length(vecc)
  k <- length(thetas)
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  F1 <- paste0("(x3 + x", k, "*a_star)")
  
  # First numerator
  N1 <- paste0("x", k+1:2, collapse = " + " )
  N1 <- paste0("exp(", N1, "*a + ")
  N1 <- paste(N1, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  
  # Second numerator
  N2 <- paste0("x", k+1:2, collapse = " + " )
  N2 <- paste0("exp(", N2, "*a_star + ")
  N2 <- paste(N2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  
  # First denominator
  D1 <- paste0("x", k+1:2, collapse = " + " )
  D1 <- paste0("1 + exp(", D1, "*a + ")
  D1 <- paste(D1, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  D1 <- paste0("(", D1, ")")
  
  # Second denominator
  D2 <- paste0("x", k+1:2, collapse = " + " )
  D2 <- paste0("1 + exp(", D2, "*a_star + ")
  D2 <- paste(D2, paste0("x", k + 2 + 1:j, " * ", "vecc_", 1:j, collapse = " + "), ")")
  D2 <- paste0("(", D2, ")")
  
  # Construct formula
  f <- paste0(" ~ ", F1, " * ( (", N1, "/", D1, ") - (", N2, "/", D2, ") )")
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_bincont_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  
  for (i in 1:j){
    ss <- stringr::str_replace_all(s, paste("vecc", i, sep = "_"), vecc[i])
  }
  
  return(as.formula(ss))
}


total_NIE_binbin_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE, a_star = 0, a = 1) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=FALSE
  
  j <- length(vecc)
  k <- length(thetas)
  
  ### FIXME: it seems that only X2 differs with/without interaction 
  ###        shall we get the other terms out of the if/else statement?
  
  ### QUESTION: Why do we need to specify xk all the time and x3 can remain like that? 
  ###           Doesn't it matter if extra terms are added to the thetas vector?
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  X1 <- paste0("x", k + 1:2, collapse = " + ")
  XC <- paste0("x", k + 2 + 1:j, "  * ", "vecc_", 1:j, collapse = " + ")
  s1 <- paste0("(1 + exp(", X1, " * a_star + ", XC, "))")
  s3 <- paste0("(1 + exp(", X1, " * a + ", XC, "))")
  s4 <- paste0("(1 + exp(", X2, " + ", X1, " * a_star +", XC, "))")
  
  if (interaction) {
    X2 <- paste0("x3 +x",k)
    s2 <- paste0("(1 + exp(", X2, " * a + ", X1, " * a +", XC, "))")
  } else {
    X2 <- paste0("x3")
    s2 <- paste0("(1 + exp(", X2, " + ", X1, " * a +", XC, "))")
  }

  f <- paste0(" ~ ", "(", s1, "*", s2, ")/(", s3, "*", s4, ")")
  
  ### DEBUG: for testing purposes
  if(debug){
      print("DEBUG: NIE_binbin_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  
  for (i in 1:j){
    ss <- stringr::str_replace_all(s, paste("vecc", i, sep = "_"), vecc[i])
  }
  
  return(as.formula(ss))
}

pure_NIE_binbin_delta <- function(thetas, vecc, interaction = TRUE, debug=FALSE, a_star = 0, a = 1) {
  ### vecc = vector of covariates
  ### DEBUG: for testing purposes
  #thetas <- c(1,2,3,4)
  #vecc   <- c(1,2)
  #interaction=FALSE
  
  j <- length(vecc)
  k <- length(thetas)
  
  ### FIXME: it seems that only X2 differs with/without interaction 
  ###        shall we get the other terms out of the if/else statement?
  
  ### QUESTION: Why do we need to specify xk all the time and x3 can remain like that? 
  ###           Doesn't it matter if extra terms are added to the thetas vector?
  
  for (i in 1:j){
    assign(paste("vecc", i, sep = "_"), vecc[i])
  }
  
  X1 <- paste0("x", k + 1:2, collapse = " + ")
  XC <- paste0("x", k + 2 + 1:j, "  * ", "vecc_", 1:j, collapse = " + ")
  s1 <- paste0("(1 + exp(", X1, " * a_star + ", XC, "))")
  s3 <- paste0("(1 + exp(", X1, " * a + ", XC, "))")
  s4 <- paste0("(1 + exp(", X2, " + ", X1, " * a_star +", XC, "))")
  
  if (interaction) {
    X2 <- paste0("x3 +x",k)
    s2 <- paste0("(1 + exp(", X2, " * a_star + ", X1, " * a +", XC, "))")
  } else {
    X2 <- paste0("x3")
    s2 <- paste0("(1 + exp(", X2, " + ", X1, " * a +", XC, "))")
  }
  
  f <- paste0(" ~ ", "(", s1, "*", s2, ")/(", s3, "*", s4, ")")
  
  ### DEBUG: for testing purposes
  if(debug){
    print("DEBUG: NIE_binbin_delta")
    print(paste0("DEBUG: length(thetas) = ", length(thetas)))
    print(paste0("DEBUG: length(vecc)   = ", length(vecc)))
    print(paste0("DEBUG: formula = ", f))
  }
  
  s <- stringr::str_replace_all(f, pattern = c("a_star" = a_star, "a" = a))
  
  for (i in 1:j){
    ss <- stringr::str_replace_all(s, paste("vecc", i, sep = "_"), vecc[i])
  }
  
  return(as.formula(ss))
}
