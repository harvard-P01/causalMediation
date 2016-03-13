# PLEASE, GIVE IT A TRY. IT SHOULD BE:
  # WHEN Y CONT: PM=TNIE/(PNDE+TE)
  # WHEN Y NOT CONT: PM=(PNDE*(TNIE-1))/(PNDE*TNIE-1)

total_effect <- function(pnde, tnie, ycont = FALSE) {
  res <- 0
  if (ycont) {
    res <- tnie + pnde
  } else {
    res <- tnie * pnde
  }
  res  
}

total_effect_delta <- function(ycont = FALSE) {
  res <- 0
  if (ycont) {
    res <- "~x1 + x2"
  } else {
    res <- "~x1 * x2"
  }
  as.formula(res)  
}