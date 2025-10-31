run_sims <- function(param_sets) {
  results <- data.frame()

  for (i in 1:nrow(param_sets)) {
    pars <- as.list(param_sets[i, ])

    out <- ode(
      y = state,
      times = times,
      func = m_riley,
      parms = pars,
      forcing_tbl = dat_forc
    )

    df <- as.data.frame(out)

    metrics <- data.frame(
      p = pars$p,
      I_min = pars$I_min,
      r = pars$r,
      R_0 = pars$R_0,
      g = pars$g,
      Average = mean(df$Phyto_bm),
      Minimum = min(df$Phyto_bm),
      Timing = times[which.max(df$Phyto_bm)]
    )
    results <- rbind(results, metrics)
  }
  return(results)
}
