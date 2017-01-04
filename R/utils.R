add_columns <- function(s, n) {
  z <- s$estimate/(s$std.error / sqrt(n))
  pval <- 2 * pnorm(-abs(s2$z))
  return(data.frame(z = z, pval = pval))
}

format_row_boot <- function(boot.out, index = 1, conf = 0.95) {
  ci <- boot.ci(boot.out, index = index, type = "perc", conf = conf)
  d <- cbind(as.data.frame(t(tail(ci$percent[1, ], 2))))
  return(d)
}

format_df_boot <- function(boot.out, conf = 0.95, n) {
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
  return(cbind(d_all, add_columns(d_all, n = n)))
}

format_df_delta <- function(delta.out, conf = 0.95, n) {
  d_all <- data.frame(matrix(NA, 7, 4))
  alpha <- (1 - conf) / 2
  z <- qnorm(1 - alpha)
  label_CI <- paste0(round(conf * 100, 2), c("% CIL", "% CIU"))
  colnames(d_all) <- c("estimate", "std.error", label_CI[1], label_CI[2])
  rownames(d_all) <- c("cde", "pnde", "tnde", "pnie", "tnie", "te", "pm")
  ##----- cde
  d_all["cde", ] <- c(delta.out$cded, delta.out$se_cded, 
                      delta.out$cded - z * delta.out$se_cded,
                      delta.out$cded + z * delta.out$se_cded)
  ##----- pnde
  d_all["pnde", ] <- c(delta.out$pnde, delta.out$se_pnde, 
                       delta.out$pnde - z * delta.out$se_pnde,
                       delta.out$pnde + z * delta.out$se_pnde)
  ##----- tnde
  d_all["tnde", ] <- c(delta.out$tnde, delta.out$se_tnde, 
                       delta.out$tnde - z * delta.out$se_tnde,
                       delta.out$tnde + z * delta.out$se_tnde)
  ##----- pnie
  d_all["pnie", ] <- c(delta.out$pnie, delta.out$se_pnie, 
                       delta.out$pnie - z * delta.out$se_pnie,
                       delta.out$pnie + z * delta.out$se_pnie)
  ##----- tnie
  d_all["tnie", ] <- c(delta.out$tnie, delta.out$se_tnie, 
                       delta.out$tnie - z * delta.out$se_tnie,
                       delta.out$tnie + z * delta.out$se_tnie)
  ##----- te
  d_all["te", ] <- c(delta.out$te, delta.out$se_te, 
                     delta.out$te - z * delta.out$se_te,
                     delta.out$te + z * delta.out$se_te)
  ##----- pm
  d_all["pm", ] <- c(delta.out$pm, delta.out$se_pm, 
                     delta.out$pm - z * delta.out$se_pm,
                     delta.out$pm + z * delta.out$se_pm)
  return(cbind(d_all, add_columns(d_all, n = n)))
}


