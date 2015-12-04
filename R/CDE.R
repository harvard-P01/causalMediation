 CDE_bin <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) { 
  ORcde <- exp(thetas[treatment] + ifelse(interaction, thetas[paste(treatment, mediator, sep = ':')] * m, 0) * (a - a_star))
  
  
  
  unname(ORcde)
}

CDE_cont <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  cde <- (thetas[treatment] + ifelse(interaction, thetas[paste(treatment, mediator, sep = ':')] * m, 0)) * (a - a_star)
  unname(cde)
}

CDE_bin_delta <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  s <- ifelse(interaction,
              paste0("~ exp((x2 + x", length(thetas), " * m) * (a - a_star))"),
              paste0(" ~exp(x2 * (a - a_star))"))
  return(as.formula(s))
}

CDE_cont_delta <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  s <- ifelse(interaction,
              paste0("~ (x2 + x", length(thetas), " * m) * (a - a_star)"),
              paste0(" ~ x2 * (a - a_star)"))
  return(as.formula(s))
}

CDE_cont_delta_test <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  s <- ifelse(interaction,
              paste0("~ (x2 + x", length(thetas), " * m) * (a - a_star)"),
              paste0(" ~ (x2 + x", length(thetas) + 1, " * m)* (a - a_star)"))
  return(as.formula(s))
}


