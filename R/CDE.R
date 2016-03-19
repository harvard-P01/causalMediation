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
  s <- stringr::str_replace_all(s, pattern = c("a_star" = a_star, "a" = a, "m" = m))
  return(as.formula(s))
}

CDE_cont_delta <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE) {
  s <- ifelse(interaction,
              paste0("~ (x2 + x", length(thetas), " * m) * (a - a_star)"),
              paste0(" ~ x2 * (a - a_star)"))
  s <- stringr::str_replace_all(s, pattern = c("a_star" = a_star, "a" = a, "m" = m))
  return(as.formula(s))
}

CDE_estimate <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE,
                      mreg = "linear", yreg = "linear") {
  if (mreg != "linear" & yreg != "linear")
    cde <- CDE_bin(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg != "linear" & yreg == "linear")
    cde <- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg == "linear" & yreg != "linear")
    cde <- CDE_bin(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg == "linear" & yreg == "linear")
    cde <- CDE_cont(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  return(list(cde = cde))
}

CDE_delta <- function(thetas, treatment, mediator, m = 0, a_star = 0, a = 1, interaction = TRUE,
                      mreg = "linear", yreg = "linear") {
  if (mreg != "linear" & yreg != "linear")
    cded <- CDE_bin_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg != "linear" & yreg == "linear")
    cded <- CDE_cont_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg == "linear" & yreg != "linear")
    cded <- CDE_bin_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  else if (mreg == "linear" & yreg == "linear")
    cded <- CDE_cont_delta(thetas = thetas, treatment = treatment, mediator = mediator, interaction = interaction, m = m, a_star = a_star, a = a)
  return(list(cded = cded))
}
