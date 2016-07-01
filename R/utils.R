format_row_boot <- function(boot.out, index = 1) {
  ci <- boot.ci(r, index = index, type = "perc")
  d <- cbind(as.data.frame(t(tail(ci$percent[1, ], 2))))
  return(d)
}

format_df_boot <- function(boot.out) {
  d_all <- NULL
  for (i in 1:7) {
    d <- format_row_boot(boot.out, i)
    d_all <- rbind(d_all, d)
  }
  estimate <- boot.out$t0
  bias <- apply(boot.out$t, 2, mean) - estimate
  std.error <- apply(boot.out$t, 2, sd)
  d_all <- cbind(estimate, bias, std.error, d_all)
  colnames(d_all) <- c("estimate", "bias", "std.error", "lower", "upper")
  rownames(d_all) <- c("cde", "pnde", "tnde", "pnie", "tnie", "te", "pm")
  return(d_all)
}

# format_df_boot(r)
