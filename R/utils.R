format_row_boot <- function(boot.out, index = 1, conf = 0.95) {
  ci <- boot.ci(boot.out, index = index, type = "perc", conf = conf)
  d <- cbind(as.data.frame(t(tail(ci$percent[1, ], 2))))
  return(d)
}

format_df_boot <- function(boot.out, conf = 0.95) {
  d_all <- NULL
  for (i in 1:7) {
    d <- format_row_boot(boot.out, i, conf = conf)
    d_all <- rbind(d_all, d)
  }
  estimate <- boot.out$t0
  bias <- apply(boot.out$t, 2, mean) - estimate
  std.error <- apply(boot.out$t, 2, sd)
  d_all <- cbind(estimate, bias, std.error, d_all)
  label_CI <- paste0(round(conf * 100, 2), c("% CIL", "% CIU"))
  colnames(d_all) <- c("estimate", "bias", "std.error", label_CI[1], label_CI[2])
  rownames(d_all) <- c("cde", "pnde", "tnde", "pnie", "tnie", "te", "pm")
  return(d_all)
}

format_df_delta <- function(delta.out) {
  d_all <- data.frame(matrix(NA, 7, 4))
  colnames(d_all) <- c("estimate", "std.error", "95% CIL", "95% CIU")
  rownames(d_all) <- c("cde", "pnde", "tnde", "pnie", "tnie", "te", "pm")
  ##----- cde
  d_all["cde", ] <- c(delta.out$cde.cde, delta.out$se.cde.cded, 
                      delta.out$cde.cde - qnorm(.975) * delta.out$se.cde.cded,
                      delta.out$cde.cde + qnorm(.975) * delta.out$se.cde.cded)
  ##----- pnde
  d_all["pnde", ] <- c(delta.out$pnde, delta.out$se.pnde, 
                       delta.out$pnde - qnorm(.975) * delta.out$se.pnde,
                       delta.out$pnde + qnorm(.975) * delta.out$se.pnde)
  ##----- tnde
  d_all["tnde", ] <- c(delta.out$tnde, delta.out$se.tnde, 
                       delta.out$tnde - qnorm(.975) * delta.out$se.tnde,
                       delta.out$tnde + qnorm(.975) * delta.out$se.tnde)
  ##----- pnie
  d_all["pnie", ] <- c(delta.out$pnie, delta.out$se.pnie, 
                       delta.out$pnie - qnorm(.975) * delta.out$se.pnie,
                       delta.out$pnie + qnorm(.975) * delta.out$se.pnie)
  ##----- tnie
  d_all["tnie", ] <- c(delta.out$tnie, delta.out$se.tnie, 
                       delta.out$tnie - qnorm(.975) * delta.out$se.tnie,
                       delta.out$tnie + qnorm(.975) * delta.out$se.tnie)
  ##----- te
  d_all["te", ] <- c(delta.out$te, delta.out$se.te, 
                     delta.out$te - qnorm(.975) * delta.out$se.te,
                     delta.out$te + qnorm(.975) * delta.out$se.te)
  ##----- pm
  d_all["pm", ] <- c(delta.out$pm, delta.out$se.pm, 
                     delta.out$pm - qnorm(.975) * delta.out$se.pm,
                     delta.out$pm + qnorm(.975) * delta.out$se.pm)
  return(d_all)
}


