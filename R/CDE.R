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
  s <- stringr::str_replace_all(s, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

CDE_cont_delta <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  s <- ifelse(interaction,
              paste0("~ (x2 + x", length(thetas), " * m) * (a - a_star)"),
              paste0(" ~ x2 * (a - a_star)"))
  s <- stringr::str_replace_all(s, pattern = c("a_star" = a_star, "a" = a))
  return(as.formula(s))
}

