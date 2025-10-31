solve_riley <- function(pars) {
  ode(
    y = state,
    times = times,
    func = m_riley,
    parms = pars,
    forcing_tbl = dat_forc
  )
}

run_riley <- function(par_vec) {
  solve_riley(par_vec) %>%
    unclass() %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, Phyto_bm)
}

plot_parameter_effect <- function(
  par_name,
  down_factor = 0.8,
  up_factor = 1.2
) {
  fmt_value <- function(x) formatC(x, format = "g", digits = 4)

  base_value <- parameters[[par_name]]
  low_value <- base_value * down_factor
  high_value <- base_value * up_factor

  base_label <- paste0(par_name, " = ", fmt_value(base_value))
  low_label <- paste0(par_name, " = ", fmt_value(low_value))
  high_label <- paste0(par_name, " = ", fmt_value(high_value))
  scenario_levels <- c(base_label, low_label, high_label)

  base_run <- run_riley(parameters) %>%
    mutate(scenario = base_label)

  low_pars <- parameters
  low_pars[[par_name]] <- low_value
  low_run <- run_riley(low_pars) %>%
    mutate(scenario = low_label)

  high_pars <- parameters
  high_pars[[par_name]] <- high_value
  high_run <- run_riley(high_pars) %>%
    mutate(scenario = high_label)

  bind_rows(base_run, low_run, high_run) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = scenario_levels
      )
    ) %>%
    ggplot(aes(time, Phyto_bm, colour = scenario, linetype = scenario)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste("Sensitivity to:", par_name),
      x = "Days",
      y = expression("Phytoplankton biomass (gC " * m^-2 * ")"),
      col = NULL,
      linetype = NULL
    ) +
    scale_colour_manual(
      values = setNames(c("black", "#1f78b4", "#e31a1c"), scenario_levels),
      breaks = scenario_levels,
      labels = scenario_levels
    ) +
    scale_linetype_manual(
      values = setNames(c("solid", "11", "11"), scenario_levels),
      breaks = scenario_levels,
      labels = scenario_levels
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
