## Basic utilities from section 5 of the original analysis script.
## These definitions do not execute model estimation.

clean_mx <- function(mx) {
  mx <- as.matrix(mx)
  mx[!is.finite(mx) | mx <= 0] <- NA_real_
  mx
}

interpolate_log_mx_rows <- function(log_mx, years) {
  for (i in seq_len(nrow(log_mx))) {
    nas <- is.na(log_mx[i, ])
    
    if (all(nas)) {
      log_mx[i, ] <- median(log_mx, na.rm = TRUE)
    } else if (any(nas)) {
      log_mx[i, nas] <- approx(
        x = years[!nas],
        y = log_mx[i, !nas],
        xout = years[nas],
        rule = 2
      )$y
    }
  }
  log_mx
}

get_observed_mx_surface <- function(DataStMoMo, ages_fit, years_vec) {
  idx_age  <- match(ages_fit, DataStMoMo$ages)
  idx_year <- match(years_vec, DataStMoMo$years)
  
  if (any(is.na(idx_age))) stop("Some requested ages are not available.")
  if (any(is.na(idx_year))) stop("Some requested years are not available.")
  
  Dxt <- DataStMoMo$Dxt[idx_age, idx_year, drop = FALSE]
  Ext <- DataStMoMo$Ext[idx_age, idx_year, drop = FALSE]
  
  if (any(Ext <= 0, na.rm = TRUE)) {
    stop("Exposure contains non-positive values.")
  }
  
  mx <- Dxt / Ext
  
  dimnames(mx) <- list(
    as.character(ages_fit),
    as.character(years_vec)
  )
  
  clean_mx(mx)
}

extract_forecast_matrix <- function(fc_obj, ages_fit, fc_years) {
  rates_fc <- as.matrix(fc_obj$rates)
  
  out <- matrix(
    as.numeric(rates_fc),
    nrow = length(ages_fit),
    ncol = length(fc_years),
    dimnames = list(
      as.character(ages_fit),
      as.character(fc_years)
    )
  )
  
  clean_mx(out)
}

surface_to_panel <- function(mx_surface, model_name = NULL) {
  out <- tibble::as_tibble(mx_surface, rownames = "age") |>
    tidyr::pivot_longer(
      cols = -age,
      names_to = "year",
      values_to = "m"
    ) |>
    dplyr::mutate(
      age = as.integer(age),
      year = as.integer(year)
    )
  
  if (!is.null(model_name)) {
    out <- out |>
      dplyr::mutate(model = model_name) |>
      dplyr::select(year, age, model, m)
  }
  
  out
}

safe_fit <- function(expr, name) {
  tryCatch(
    expr,
    error = function(e) {
      message("[FAILED] ", name, ": ", e$message)
      NULL
    }
  )
}

combine_mx_surfaces <- function(mx_list, weights) {
  players <- intersect(names(weights), names(mx_list))
  
  if (length(players) < 2) {
    stop("At least two models are required for the ensemble.")
  }
  
  weights <- weights[players]
  weights <- weights / sum(weights)
  
  ref_dim <- dim(mx_list[[players[1]]])
  ref_dn  <- dimnames(mx_list[[players[1]]])
  
  mx_ens <- matrix(
    0,
    nrow = ref_dim[1],
    ncol = ref_dim[2],
    dimnames = ref_dn
  )
  
  for (mn in players) {
    if (!all(dim(mx_list[[mn]]) == ref_dim)) {
      stop(paste("Incompatible dimensions for model:", mn))
    }
    
    if (!identical(dimnames(mx_list[[mn]]), ref_dn)) {
      stop(paste("Incompatible dimnames for model:", mn))
    }
    
    mx_ens <- mx_ens + weights[mn] * mx_list[[mn]]
  }
  
  clean_mx(mx_ens)
}

## ==============================================================
