# PLEASE, GIVE IT A TRY. IT SHOULD BE:
  # WHEN Y CONT: PM=TNIE/(PNDE+TE)
  # WHEN Y NOT CONT: PM=(PNDE*(TNIE-1))/(PNDE*TNIE-1)

proportion_mediated <- function(pnde, tnie, te, ycont = FALSE) {
  res <- 0
  if (ycont) {
    res <- tnie / (pnde + te)
  } else {
    res <-  (pnde * (tnie - 1)) / (pnde * tnie - 1)
  }
  res  
}

proportion_mediated_delta <- function(ycont = FALSE) {
  res <- 0
  if (ycont) {
    res <- "~x2 / (x1 + x3)"
  } else {
    res <-  "~(x1 * (x2 - 1)) / (x1 * x2 - 1)"
  }
  as.formula(res) 
}