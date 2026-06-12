## Ensemble, rolling-origin, and accuracy helpers corresponding to
## sections 7 and 8 of the original script.

loss_mse <- function(y_hat, y_true) {
  mean((y_hat - y_true)^2, na.rm = TRUE)
}

coalition_key <- function(S) {
  if (length(S) == 0) return("EMPTY")
  paste(sort(S), collapse = "|")
}

all_nonempty_coalitions <- function(players) {
  out <- list()
  idx <- 1L
  
  for (s in seq_along(players)) {
    cmb <- combn(players, s, simplify = FALSE)
    
    for (cc in cmb) {
      out[[idx]] <- cc
      idx <- idx + 1L
    }
  }
  
  out
}

compute_shapley_exact <- function(v_map, players) {
  n <- length(players)
  phi <- setNames(numeric(n), players)
  
  for (i in players) {
    others <- setdiff(players, i)
    
    for (k in 0:length(others)) {
      subsets <- if (k == 0) {
        list(character(0))
      } else {
        combn(others, k, simplify = FALSE)
      }
      
      for (S in subsets) {
        key_S  <- coalition_key(S)
        key_Si <- coalition_key(c(S, i))
        
        weight <- factorial(length(S)) *
          factorial(n - length(S) - 1) /
          factorial(n)
        
        phi[i] <- phi[i] +
          weight * (v_map[[key_Si]] - v_map[[key_S]])
      }
    }
  }
  
  phi
}

compute_performance_weights_mortality <- function(validation_wide, players) {
  mse_vec <- sapply(players, function(mn) {
    loss_mse(
      y_hat  = validation_wide[[mn]],
      y_true = validation_wide$m_obs
    )
  })
  
  xi <- mse_vec / max(mse_vec)
  
  w <- exp(-xi)
  w <- w / sum(w)
  names(w) <- players
  
  list(
    mse = mse_vec,
    xi = xi,
    weights = w
  )
}

compute_shapley_weights_mortality <- function(validation_wide, players) {
  y_true <- validation_wide$m_obs
  coalitions <- all_nonempty_coalitions(players)
  
  v_map <- list()
  
  v_map[["EMPTY"]] <- -loss_mse(
    y_hat  = rep(mean(y_true, na.rm = TRUE), length(y_true)),
    y_true = y_true
  )
  
  for (S in coalitions) {
    y_hat_S <- rowMeans(as.matrix(validation_wide[, S, drop = FALSE]))
    v_map[[coalition_key(S)]] <- -loss_mse(y_hat_S, y_true)
  }
  
  phi <- compute_shapley_exact(v_map, players)
  
  if (!is.finite(sd(phi)) || sd(phi) == 0) {
    w <- rep(1 / length(phi), length(phi))
    names(w) <- names(phi)
  } else {
    z <- (phi - mean(phi)) / sd(phi)
    z <- z - max(z)
    
    w <- exp(z) / sum(exp(z))
    names(w) <- names(phi)
  }
  
  list(
    phi = phi,
    weights = w,
    value_map = v_map
  )
}

## ==============================================================
## 8) ROLLING ONE-STEP FORECAST FUNCTION
## ==============================================================n
compute_rolling_one_step <- function(years_eval,
                                     models,
                                     model_names,
                                     DataStMoMo,
                                     MorData,
                                     sex_series,
                                     ages_fit,
                                     years_start,
                                     label = "VALIDATION") {
  rolling_outputs <- list()
  
  for (yr in years_eval) {
    rolling_outputs[[as.character(yr)]] <- list()
    fit_years <- years_start:(yr - 1)
    
    for (mn in model_names) {
      cat("[", label, "] year =", yr, "| model =", mn, "\n")
      
      rolling_outputs[[as.character(yr)]][[mn]] <- tryCatch(
        fit_and_forecast_model(
          spec              = models[[mn]],
          DataStMoMo        = DataStMoMo,
          MorData           = MorData,
          sex_series        = sex_series,
          ages_fit          = ages_fit,
          fit_years         = fit_years,
          forecast_end_year = yr,
          model_name        = mn
        ),
        error = function(e) {
          warning(
            paste(
              "Rolling model failed:",
              mn,
              "year:",
              yr,
              "|",
              e$message
            )
          )
          NULL
        }
      )
    }
  }
  
  prediction_panel <- purrr::map_dfr(years_eval, function(yr) {
    purrr::map_dfr(model_names, function(mn) {
      obj <- rolling_outputs[[as.character(yr)]][[mn]]
      
      if (is.null(obj) || is.null(obj$mx_fc)) {
        return(tibble::tibble())
      }
      
      surface_to_panel(
        mx_surface = obj$mx_fc,
        model_name = mn
      )
    })
  }) |>
    dplyr::rename(m_hat = m)
  
  list(
    outputs = rolling_outputs,
    prediction_panel = prediction_panel
  )
}

make_accuracy_table <- function(panel, model_col = "model") {
  panel |>
    dplyr::group_by(.data[[model_col]]) |>
    dplyr::summarise(
      MSE = mean(error_m^2, na.rm = TRUE),
      RMSE = sqrt(mean(error_m^2, na.rm = TRUE)),
      MAE = mean(abs(error_m), na.rm = TRUE),
      Bias = mean(error_m, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(MSE)
}
make_ensemble_panel <- function(wide_df, weights, method_name) {
  players <- names(weights)
  y_hat <- as.numeric(as.matrix(wide_df[, players, drop = FALSE]) %*% weights)
  
  tibble::tibble(
    year = wide_df$year,
    age = wide_df$age,
    m_obs = wide_df$m_obs,
    m_hat = y_hat,
    method = method_name
  ) |>
    dplyr::mutate(
      error_m = m_hat - m_obs,
      sq_error = error_m^2,
      abs_error = abs(error_m)
    )
}
