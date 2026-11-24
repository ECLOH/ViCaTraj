#' @author Elie.
#' @title La fonction "map_boot_survreg"
#' @description  l'objectif est d'utiliser les samples (créés par func_sample_id()) pour faire un certain nombre de statistiques et d'estimations
#' @param G_lires list. Résultat de func_sample_id()
#' @param DUREE nombre de mois max. après le démarrage pour les traitements
#' @param START_cumsum variable (non-standard evaluation). Variable de rang pour identifier le mois de départ
#' @param NIRs optionnel. Vecteur de NIRs à cibler en particulier (dans le groupe TEST) 
#' @param RSA_var variable (non-standard evaluation). Variable de perception du RSA
#' @return un data.table
#' @export
map_boot_survreg <- function(G_lires, DUREE = 24, START_cumsum = NULL, NIRs = NIR_selected, RSA_var = RSA_simple_corr) {
  
  RSA_var_char <- deparse(substitute(RSA_var))
  require(survival)
  message(1)

  res_n <- lapply(1:length(G_lires), function(i) {
    lires <- G_lires[[i]]
    results <- lires[["samples"]] |> bind_rows()
    results[, POID := 10 / .N, by = NIR_TEST]
    t.seq_comp <- seq_long[, date_o := date][NIR %in% results$NIR]
    t.seq_comp <- t.seq_comp[
      results[, .(NIR, date, TYPE, POID, NIR_TEST)],
      on = .(NIR == NIR, date_o >= date),
      nomatch = NULL
    ]
  message(2)

    namres <- c(RSA_var_char, names(results)) |> unique() #c(names(results), "NIR_TEST")
    #print(namres)
    #print("warning!! STATUS_TSI_rank")
    seq_comp <- bind_rows(
      seq_long |>
        dtplyr::lazy_dt() |>
        filter(NIR %in% results$NIR_TEST & {{ START_cumsum }} >= 1) |> #STATUS_TSI_rank >=1 ) |> #{{START_cumsum}} >=1) |>
        mutate(TYPE = "TEST", NIR_TEST = "", POID = 1) |>
        select(all_of(namres)) |>
        as.data.table(),
      #  VAR_rank >= 1][, TYPE := "TEST"][, NIR_TEST := ""][, POID := 1][, ..namres],
      t.seq_comp[, ..namres]
    )

    if (!is.null(NIRs)) {
      seq_comp <- seq_comp |>
        dtplyr::lazy_dt() |>
        dplyr::filter((NIR %in% NIRs | NIR_TEST %in% NIRs)) |>
        as.data.table()
    }
    #seq_comp <- seq_comp[order(NIR, date)]
    setorder(seq_comp, NIR, date)
    seq_comp[, NewRank := 1:.N, by = NIR]
    seq_comp <- seq_comp |>
      dtplyr::lazy_dt() |>
      mutate(Sortie = (is.na({{ RSA_var }}) | {{ RSA_var }} != "RSA")) |>
      as.data.table()

    #seq_comp[, Sortie := (is.na(..RSA_var_char) | ..RSA_var_char != "RSA")]
    seq_comp[, Sortie_cumsum := cumsum(Sortie), by = NIR]
    seq_comp[,
      Event := (Sortie &
        data.table::shift(Sortie, n = 1, type = "lag", fill = FALSE) == FALSE),
      by = NIR
    ]
    seq_comp[,
      TYPE := if_else(
        (is.na(TYPE) | TYPE != "TEMOIN"),
        "TEST",
        TYPE
      )
    ]

    message(
      "Ctrl : tous les mois ultérieurs sont présents, avec ou sans RSA : "
    )
    print(
      seq_comp |>
        group_by(DATE) |>
        summarise(N = n_distinct(NIR), N_total = n_distinct(seq_comp$NIR)) |>
        arrange(DATE) |>
        slice_tail(n = 1)
    )

    surv_comp <- seq_comp |>
      filter(NewRank <= DUREE) |>
      group_by(NIR) |>
      summarise(
        EVENT = sum(Event) >= 1,
        TIME_event = min(NewRank[Event]),
        TIME_censure = max(NewRank, na.rm = TRUE)
      ) |>
      mutate(
        TIME = if_else(
          EVENT,
          TIME_event,
          TIME_censure
        )
      ) |>
      left_join(seq_comp |> group_by(NIR) |> slice_min(date, n = 1)) # |>

    print(surv_comp)
    #mutate(ANC_cut2 := case_when(
    # RSA_simple_corr_CumSum_SinceLastNA_CUT %in% c("(0,6]", "(6,12]") ~ "(0,12]",
    # RSA_simple_corr_CumSum_SinceLastNA_CUT %in% c("(60,120]") ~ "(60,120]",
    # !is.na(RSA_simple_corr_CumSum_SinceLastNA_CUT) ~ "(12, 60]",
    # TRUE ~ RSA_simple_corr_CumSum_SinceLastNA_CUT
    #))
  message(3)

    seq_comp_duree <- seq_comp |>
      filter(NewRank <= DUREE) |>
      group_by(NIR) |>
      arrange(Q, .by_group = T) |>
      mutate(across(
        .cols = {{ RSA_var }},
        .fns = ~ tidyr::replace_na(.x, "NO.RSA")
      )) |>
      mutate(
        PHASE = {{ RSA_var }} !=
          shift({{ RSA_var }}, type = 'lag', fill = "NO_RSA", n = 1)
      ) |>
      mutate(PHASE = cumsum(PHASE)) |>
      group_by(TYPE, NIR, PHASE) |>
      summarise(PHASE_TYPE = unique( {{ RSA_var }} ), PHASE_mois = n() ) |>
      group_by(TYPE, NIR) |>
      mutate(TOTAL_duree = sum(PHASE_mois)) |>
      mutate(PHASE_prop = PHASE_mois / TOTAL_duree)

    phases_stats <- seq_comp_duree |>
      filter(TOTAL_duree >= DUREE) |>
      group_by(TYPE, PHASE_TYPE) |>
      summarise(
        Ni = n_distinct(NIR),
        Ni3 = n_distinct(NIR[PHASE == 3]),
        Mn_prop = mean(PHASE_prop),
        Mn_mois = mean(PHASE_mois)
      ) |>
      mutate(Ni3_prop = Ni3 / Ni)
  message(4)

    prop_stat <- seq_comp |>
      group_by(TYPE, NewRank, {{ RSA_var }}) |>
      summarise(Ni = n_distinct(NIR)) |>
      group_by(TYPE) |>
      mutate(prop = Ni / sum(Ni[NewRank == 1])) |>
      group_by(TYPE, NewRank) |>
      mutate(cens_prop = 1 - sum(prop))

    prop_stat_12 <- seq_comp |>
      group_by(NIR) |>
      mutate(MAX_NewRank = max(NewRank)) |>
      ungroup() |>
      filter(MAX_NewRank >= 12 & NewRank <= 12) |>
      group_by(TYPE, NewRank, {{ RSA_var }}, POID) |>
      summarise(Ni_simple = n_distinct(NIR)) |>
      mutate(Ni_poid = Ni_simple*POID) |> 
      group_by(TYPE, NewRank, {{ RSA_var }}) |>
      summarise(Ni = sum(Ni_poid)) |> 
      group_by(TYPE) |>
      mutate(prop = Ni / sum(Ni[NewRank == 1])) |>
      group_by(TYPE, NewRank) |>
      mutate(cens_prop = 1 - sum(prop))|> 
      left_join(
        seq_comp |>
      group_by(NIR) |>
      mutate(MAX_NewRank = max(NewRank)) |>
      ungroup() |>
      filter(MAX_NewRank >= 12 & NewRank <= 12) |> 
          group_by(TYPE, POID) |> 
          summarise(Ni_total_simple = n_distinct(NIR)) |> 
          mutate(Ni_total_poid = Ni_total_simple*POID) |> 
          group_by(TYPE) |> 
          summarise(Ni_total = sum(Ni_total_poid))
      )

    prop_stat_24 <- seq_comp |>
      group_by(NIR) |>
      mutate(MAX_NewRank = max(NewRank)) |>
      ungroup() |>
      filter(MAX_NewRank >= 24 & NewRank <= 24) |>
      group_by(TYPE, NewRank, {{ RSA_var }}, POID) |>
      summarise(Ni_simple = n_distinct(NIR)) |>
      mutate(Ni_poid = Ni_simple*POID) |> 
      group_by(TYPE, NewRank, {{ RSA_var }}) |>
      summarise(Ni = sum(Ni_poid)) |> 
      group_by(TYPE) |>
      mutate(prop = Ni / sum(Ni[NewRank == 1])) |>
      group_by(TYPE, NewRank) |>
      mutate(cens_prop = 1 - sum(prop))|> 
      left_join(
        seq_comp |>
      group_by(NIR) |>
      mutate(MAX_NewRank = max(NewRank)) |>
      ungroup() |>
      filter(MAX_NewRank >= 24 & NewRank <= 24) |> 
          group_by(TYPE, POID) |> 
          summarise(Ni_total_simple = n_distinct(NIR)) |> 
          mutate(Ni_total_poid = Ni_total_simple*POID) |> 
          group_by(TYPE) |> 
          summarise(Ni_total = sum(Ni_total_poid))
      )
  
  message(5)

    survobj <- survival::Surv(time = surv_comp$TIME, event = surv_comp$EVENT)
    fit_kp <- survfit(survobj ~ 1)
    
    limo <- list(
      "fit_1.5" = survreg(formula = survobj ~ 1, dist = "weibull", scale = 1.5),
      "fit_1.2" = survreg(formula = survobj ~ 1, dist = "weibull", scale = 1.2),
      "fit_1" = survreg(formula = survobj ~ 1, dist = "weibull", scale = 1),
      "fit_0.8" = survreg(formula = survobj ~ 1, dist = "weibull", scale = 0.8),
      "fit_0.5" = survreg(formula = survobj ~ 1, dist = "weibull", scale = 0.5),
      "loglogistic" = survreg(formula = survobj ~ 1, dist = "loglogistic"),
      "lognormal" = survreg(formula = survobj ~ 1, dist = "lognormal")
    )

    dfmo <- bind_rows(
      lapply(names(limo), function(ni) {
        df.wb <- predict(limo[[ni]], type = "quantile", p = seq(0, 1, 0.01)) %>%
          unique() %>%
          t() %>%
          data.frame()
        names(df.wb) <- "mois"
        df.wb$quant <- seq(0, 1, 0.01)
        df.wb$type <- ni
        return(df.wb)
      })
    )

    dfmo <- bind_rows(
      dfmo,
      data.frame("mois" = fit_kp$time, "quant" = 1 - fit_kp$surv, "type" = "KP")
    )

    p_dist <- ggplot(data = dfmo, aes(x = mois, y = 1 - quant, color = type)) +
      geom_line(data = filter(dfmo, type != "KP"), linewidth = 2) +
      geom_point(data = filter(dfmo, type == "KP"), color = "black", size = 1) +
      geom_point(data = filter(dfmo, type == "KP"), size = 5, alpha = 0.5) +
      scale_x_continuous(limits = c(0, 37)) +
      scale_y_continuous(limits = c(0.025, 1)) +
      scale_color_viridis_d() +
      theme_light()

  message(6)

    km_global <- survfit(
      survival::Surv(time = TIME, event = EVENT) ~ TYPE,
      weights = POID,
      data = surv_comp
    )

  message(7)

    seqc24 <- seq_comp |>
        group_by(NIR) |>
        mutate(MAX_NewRank = max(NewRank)) |>
        ungroup() |>
        filter(MAX_NewRank >= 24)

    surv_comp24 <- surv_comp |> filter(NIR %in% seqc24$NIR)
if(nrow(surv_comp24)>=1){
    km_global_24 <- survfit(
      survival::Surv(time = TIME, event = EVENT) ~ TYPE,
      weights = POID,
      data = surv_comp24
    )
} else {
  km_global_24 <- NULL
}

message(8)

    modsurv <- survival::survreg(
      dist = "lognormal",
      survival::Surv(time = TIME, event = EVENT) ~ TYPE,
      data = surv_comp,
      weights = POID,
      scale = 0
    )

    modsurv_ANC_g <- survival::survreg(
      dist = "lognormal",
      survival::Surv(time = TIME, event = EVENT) ~
        RSA_simple_corr_time_rang_CUT,
      data = surv_comp,
      weights = POID,
      scale = 0
    )

    modsurv_ANC_type <- survival::survreg(
      dist = "lognormal",
      survival::Surv(time = TIME, event = EVENT) ~
        RSA_simple_corr_time_rang_CUT + TYPE,
      data = surv_comp,
      weights = POID,
      scale = 0
    )

    modsurv_ANC_type_cox <- survival::coxph(
      survival::Surv(time = TIME, event = EVENT) ~
        RSA_simple_corr_time_rang_CUT + TYPE,
      data = surv_comp,
      weights = POID
    )

    modsurv_type_cox <- survival::coxph(
      survival::Surv(time = TIME, event = EVENT) ~
        TYPE,
      data = surv_comp,
      weights = POID
    )

    if ("AGE" %in% names(surv_comp)) {
      modsurv_ANC_type_global <- survival::survreg(
        dist = "lognormal",
        survival::Surv(time = TIME, event = EVENT) ~
          AGE +
            #typo_agence +
            
            SEXE +
            Global_fam +
            RSA_simple_corr_CumSum_SinceLastNA_CUT +
            TYPE,
        data = surv_comp,
        weights = POID,
        scale = 0
      )
    } else {
      modsurv_ANC_type_global <- NULL
    }

    res <- list(
      "lires" = lires,
      "phases_stats" = phases_stats,
      "prop_stat" = prop_stat,
      "prop_stat_12" = prop_stat_12,
      "prop_stat_24" = prop_stat_24,
      "survobj" = survobj,
      "p_dist" = p_dist,
      "km_global" = km_global,
      "km_global_24" = km_global_24, 
      "modsurv" = modsurv,
      "modsurv_ANC_g" = modsurv_ANC_g,
      "modsurv_ANC_type" = modsurv_ANC_type,
      "modsurv_ANC_type_global" = modsurv_ANC_type_global,
      "modsurv_ANC_type_cox"=modsurv_ANC_type_cox,
      "modsurv_type_cox"=modsurv_type_cox

    )
    return(res)
})
  return(res_n)
}
