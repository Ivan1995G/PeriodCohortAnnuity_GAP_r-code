## Financial helpers used by the paper.
## EIOPA workbook ingestion replaces only the dynamic download step;
## curve definitions, interpolation, compounding, and valuation are unchanged.

# Read an annual EIOPA zero-coupon curve from the exact workbook used in the
# paper. The workbook stores maturities in the first column and countries or
# currency areas in subsequent columns.
read_eiopa_curve <- function(workbook, sheet, country = "Euro", k_max = 33L) {
  if (!file.exists(workbook)) {
    stop("Missing EIOPA workbook: ", workbook, call. = FALSE)
  }

  raw <- readxl::read_excel(
    workbook,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  country_col <- which(as.character(raw[1, ]) == country)
  if (length(country_col) != 1L) {
    stop("Could not identify the EIOPA column for ", country, call. = FALSE)
  }

  maturity <- suppressWarnings(as.integer(raw[[1]]))
  zero_rate <- suppressWarnings(as.numeric(raw[[country_col]]))
  keep <- is.finite(maturity) & maturity >= 1L & is.finite(zero_rate)

  curve <- tibble::tibble(
    maturity = maturity[keep],
    zero_rate = zero_rate[keep]
  )

  if (!all(seq_len(k_max) %in% curve$maturity)) {
    stop("The EIOPA sheet does not contain all required maturities.", call. = FALSE)
  }

  curve[curve$maturity %in% seq_len(k_max), , drop = FALSE]
}

load_paper_discount_curves <- function(workbook, k_max, flat_rate = 0.03) {
  sheets <- c(
    eiopa_spot_no_VA = "RFR_spot_no_VA",
    eiopa_spot_no_VA_up = "Spot_NO_VA_shock_UP",
    eiopa_spot_no_VA_down = "Spot_NO_VA_shock_DOWN"
  )

  labels <- c(
    eiopa_spot_no_VA = "spot curve without VA",
    eiopa_spot_no_VA_up = "interest-rate up",
    eiopa_spot_no_VA_down = "interest-rate down"
  )

  stress <- c(
    eiopa_spot_no_VA = "base",
    eiopa_spot_no_VA_up = "up",
    eiopa_spot_no_VA_down = "down"
  )

  curves <- lapply(names(sheets), function(scenario) {
    x <- read_eiopa_curve(workbook, sheets[[scenario]], k_max = k_max)
    x$scenario <- scenario
    x$scenario_label <- labels[[scenario]]
    x$stress_type <- stress[[scenario]]
    x
  })

  flat <- tibble::tibble(
    maturity = seq_len(k_max),
    zero_rate = rep(flat_rate, k_max),
    scenario = "flat_3pct",
    scenario_label = "flat 3%",
    stress_type = "benchmark"
  )

  out <- dplyr::bind_rows(curves, list(flat))
  out$discount_factor <- (1 + out$zero_rate)^(-out$maturity)
  out
}


extract_mortality_vector <- function(mx_surface, valuation_year, x_ret,
                                     max_age, mortality_type) {
  ages_needed <- x_ret:(max_age - 1)

  if (mortality_type == "period") {
    return(as.numeric(mx_surface[as.character(ages_needed), as.character(valuation_year)]))
  }

  as.numeric(sapply(seq_along(ages_needed), function(k) {
    age_k <- ages_needed[k]
    year_k <- valuation_year + k - 1
    mx_surface[as.character(age_k), as.character(year_k)]
  }))
}

annuity_metrics_zero_curve <- function(mx_surface, valuation_year, x_ret,
                                       max_age, discount_curve,
                                       benefit = 1,
                                       mortality_type = c("period", "cohort")) {
  mortality_type <- match.arg(mortality_type)

  k_vec <- seq_len(max_age - x_ret)
  dc <- discount_curve |>
    dplyr::filter(.data$maturity %in% k_vec) |>
    dplyr::arrange(.data$maturity)

  if (nrow(dc) != length(k_vec)) {
    stop("The discount curve must contain all annual maturities 1,...,max_age-x_ret.")
  }

  m_vec <- extract_mortality_vector(
    mx_surface = mx_surface,
    valuation_year = valuation_year,
    x_ret = x_ret,
    max_age = max_age,
    mortality_type = mortality_type
  )

  if (any(!is.finite(m_vec) | m_vec <= 0)) {
    return(list(value = NA_real_))
  }

  survival <- exp(-cumsum(m_vec))
  pv_contrib <- benefit * dc$discount_factor * survival

  list(
    value = sum(pv_contrib),
    survival = survival,
    pv_contrib = pv_contrib
  )
}
