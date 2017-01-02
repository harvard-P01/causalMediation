data(Mbin_int_data)

cm <- causmed$new(data = Mbin_int_data,
                  outcome = "Y_cont_int",
                  treatment = 'A',
                  mediator = 'M_bin',
                  covariates = "C",
                  interaction = TRUE,
                  yreg = "linear", mreg = "logistic",
                  boot = FALSE, nboot = 500, event = NULL, a_star = 0, a = 1, m = 3)

cm$delta()
cm$print_delta()
cm$print_output()
cm$print_delta(digits = 4)
cm$print_delta()
cm$print_delta(digits = 3, conf = .90)

summary(cm$outcome_regression)

cm$bootstrap()
cm$print_boot()
cm$print_output()
cm$print_boot(digits = 3)
cm$print_boot(digits = 3, conf = 0.90)

cm$mediation()
cm$medflex()

##----- Test methods

cm
cm$create_formulas()
cm$outcome_formula
cm$run_regressions()
cm$outcome_regression
cm$mediator_formula
cm$mediator_regression
cm$get_coef()
cm$vcov_block

cm$CDE_boot()
cm$cde_boot
cm$CDE_delta()
cm$cde_delta
cm$se_cde_delta
cm$bootstrap()

cm$NDE_boot()
cm$nde_boot
cm$NDE_delta()
cm$nde_delta
cm$se_pnde_delta
cm$se_tnde_delta

cm$NIE_boot()
cm$nie_boot
cm$NIE_delta()
cm$nie_delta
cm$se_pnie_delta
cm$se_tnie_delta

cm$NIE_boot()
cm$nie_boot
cm$NIE_delta()
cm$nie_delta

cm$total_effect_delta()
cm$te_delta
cm$se_te_delta

cm$total_effect_boot()
cm$te_boot

cm$proportion_mediated_boot()
cm$pm_boot

cm$proportion_mediated_delta()

cm$total_effect_boot()
cm$te_boot
cm$se_pm_delta

cm$bootstrap()
cm$se_cde_delta
cm$se_pm_delta
cm$se_pnde_delta
cm$se_pnie_delta
cm$se_te_delta
cm$se_tnde_delta
cm$se_tnie_delta

cm$boot_out
cm$print_boot()
