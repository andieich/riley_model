run_sims <- function(param_sets) {
  # empty df to store results
  results <- data.frame()

  #loop over each row in parameter df
  for (i in 1:nrow(param_sets)) {
    # select one row of parameters
    pars <- as.list(param_sets[i, ])

    # simulate with this set of parameters
    out <- ode(
      y = state,
      times = times,
      func = m_riley,
      parms = pars,
      forcing_tbl = dat_forc
    )

    # transforl deSolve object to df
    df <- as.data.frame(out)

    # Calculate metrics:
    # - mean Phyto_bm
    # - minimum Phyto_bm
    # - maximum Phyto_bm
    # - time (day) of maximum
    # add actual values of parameter set
    metrics <- data.frame(
      p = pars$p,
      I_min = pars$I_min,
      r = pars$r,
      R_0 = pars$R_0,
      g = pars$g,
      mean = mean(df$Phyto_bm),
      min = min(df$Phyto_bm),
      max = max(df$Phyto_bm),
      time_max = times[which.max(df$Phyto_bm)]
    )
    # combine into "growing" df
    results <- rbind(results, metrics)
  }
  return(results)
}
