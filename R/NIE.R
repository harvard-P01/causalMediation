NIE_binbin <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  for (c in covariates){
    covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm=TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  ORnie <- (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm) *
              (1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a + covariatesTerm)))/
    ((1 + exp(betas[1] + betas[treatment]*a+covariatesTerm))*(1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(ORnie)
}

NIE_binbin_delta <- function(betas, thetas, vecc, treatment, mediator, a_star = 0, a = 1, interaction = TRUE) {
  j <- length(vecc)
  k <- length(thetas)
  #   X1 <- paste0("x", k + 1:3, collapse = " + ")
  #   X2 <- paste0("x3 +x",k)
  #   XC <- paste0("x", k + 3 + 1:j, "  * ", "vecc[", 1:j, "]", collapse = " + ")
  #   s1 <- paste0("((1 + exp(", X1, " * a_star + ", XC, "))")
  #   s2 <- paste0("(1 + exp(", X2, " * a + ", X1, " * a +", XC, ")))")
  #   s3 <- paste0("((1 + exp(", X1, " * a + ", XC, "))")
  #   s4 <- paste0("(1 + exp(", X2, " * a + ", X1, " * a_star +", XC, ")))")
  #   "((1 + exp(x_{k+1} + x_{k+2} + x_{k+3} * a_star + x_{k+4} * c_1 +...+ x_{k+3+j} * c_j))*
  #   (1 + exp(x_{3} + x_{k} * a + x_{k+1} + x_{k+2} + x_{k+3} * a + x_{k+4} * c_1+...+x_{k+3+j} * c_j))) /
  #   ((1 + exp(x_{k+1} + x_{k+2} + x_{k+3} * a + x_{k+4} * c_1+...+ x_{k+3+j} * c_j))*
  #   (1 + exp(x_{3} + x_{k} * a + x_{k+1} + x_{k+2} + x_{k+3} * a_star + x_{k+4} * c_1 +...+ x_{k+3+j} * c_j)))"
  #   
  
  #   
  #     ORnie <- (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm) *
  #               (1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a + covariatesTerm)))/
  #     ((1 + exp(betas[1] + betas[treatment] * a + covariatesTerm)) * (1 + exp(thetas[mediator] + interactionTerm * a + betas[1] + betas[treatment] * a_star + covariatesTerm)))
  #   unname(ORnie)
  
  for (i in 1:length(vecc))
    assign(paste("vecc", i, sep = "_"), vecc[i])
  
  if (interaction) {
    X1 <- paste0("x", k + 1:2, collapse = " + ")
    X2 <- paste0("x3 +x",k)
    XC <- paste0("x", k + 2 + 1:j, "  * ", "vecc_", 1:j, collapse = " + ")
    s1 <- paste0("((1 + exp(", X1, " * a_star + ", XC, "))")
    s2 <- paste0("(1 + exp(", X2, " * a + ", X1, " * a +", XC, ")))")
    s3 <- paste0("((1 + exp(", X1, " * a + ", XC, "))")
    s4 <- paste0("(1 + exp(", X2, " * a + ", X1, " * a_star +", XC, ")))")
    s <- paste0("~ ", s1, "*", s2, "/", s3, "*", s4)
  } else {
    X1 <- paste0("x", k + 1:2, collapse = " + ")
    X2 <- paste0("x3")
    XC <- paste0("x", k + 2 + 1:j, "  * ", "vecc_", 1:j, collapse = " + ")
    s1 <- paste0("((1 + exp(", X1, " * a_star + ", XC, "))")
    s2 <- paste0("(1 + exp(", X2, " + ", X1, " * a +", XC, ")))")
    s3 <- paste0("((1 + exp(", X1, " * a + ", XC, "))")
    s4 <- paste0("(1 + exp(", X2, " + ", X1, " * a_star +", XC, ")))")
    s <- paste0("~ ", s1, "*", s2,"/", s3,"*", s4)
  }
  return(as.formula(s))
}

NIE_bincont <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  for (c in covariates){
    covariatesTerm <- covariatesTerm + betas[c]*apply(df[c], 2, mean, na.rm=TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator]+interactionTerm*a) *
    (exp(betas[1] + betas[treatment] * a + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a + covariatesTerm)) -
       exp(betas[1] + betas[treatment] * a_star + covariatesTerm) / (1 + exp(betas[1] + betas[treatment] * a_star + covariatesTerm)))
  unname(nie)
}

NIE_contbin <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  for (c in covariates){
    covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep = ':')]),
                            0,
                            thetas[paste(treatment, mediator, sep = ':')])
  
  ORnie <- exp((thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a) * (a - a_star))
  unname(ORnie)
}

NIE_contcont <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
  covariatesTerm <- 0
  for (c in covariates){
    covariatesTerm <- covariatesTerm + betas[c] * apply(df[c], 2, mean, na.rm = TRUE)
  }
  interactionTerm <- ifelse(is.na(thetas[paste(treatment, mediator, sep=':')]),
                            0,
                            thetas[paste(treatment, mediator, sep=':')])  
  
  nie <- (thetas[mediator] * betas[treatment] + interactionTerm * betas[treatment] * a_star) * (a - a_star)  
  unname(nie)
}

NIE_contcont_delta <- function(betas, thetas, treatment, mediator, covariates, a_star = 0, a = 1, interaction = TRUE) {
  if (interaction) {
    # s <- "(x3 * x_{k + 2} + x_{k} * x_{k + 2}*a_star)*(a-a_star)"
    s <- paste0("~ (x3 * x", length(thetas) + 2, " + x", length(thetas), " * x", length(thetas) + 2, " * a_star) * (a - a_star)")
  } else {
    # s <- "(x3 * x_{k + 2})*(a-a_star)"
    s <- paste0("~ (x3 * x", length(thetas) + 2, " * a_star) * (a - a_star)")
  }
  return(as.formula(s))
}
