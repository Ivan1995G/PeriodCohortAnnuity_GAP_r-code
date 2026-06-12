## Model specifications and fit/forecast dispatch corresponding to
## sections 4 and 6 of the original script.

models <- list(
  LC = list(
    type = "stmomo",
    model = lc(link = "log"),
    scale = "mx"
  ),
  APC = list(
    type = "stmomo",
    model = apc(link = "log"),
    scale = "mx"
  ),
  RH = list(
    type = "stmomo",
    model = rh(link = "log"),
    scale = "mx"
  ),
  CBD = list(
    type = "stmomo",
    model = cbd(link = "log"),
    scale = "mx"
  ),
  M6 = list(
    type = "stmomo",
    model = m6(link = "log"),
    scale = "mx"
  ),
  PLAT = list(
    type = "stmomo",
    model = plat(link = "log"),
    scale = "mx"
  ),
  HU = list(
    type = "hu",
    scale = "mx",
    order = 6,
    method = "classical"
  ),
  CPSPLINE = list(
    type = "cpspline",
    scale = "mx",
    infant = FALSE,
    lambda_lower = c(-4, 1),
    lambda_upper = c(0, 5),
    lambda_ngrid = 15,
    lambda_logscale = TRUE,
    cps_kappas = c(10^4, 10^4)
  )
)

model_names <- names(models)
## 6) MODEL FIT + FORECAST DISPATCH
## ==============================================================n
fit_forecast_stmomo <- function(spec,
                                DataStMoMo,
                                ages_fit,
                                fit_years,
                                forecast_end_year,
                                model_name) {
  fit_end <- max(fit_years)
  h <- forecast_end_year - fit_end
  
  if (h < 1) stop("Forecast horizon must be positive.")
  
  fit_obj <- fit(
    object    = spec$model,
    data      = DataStMoMo,
    ages.fit  = ages_fit,
    years.fit = fit_years
  )
  
  fc_obj <- forecast(fit_obj, h = h)
  fc_years <- (fit_end + 1):forecast_end_year
  
  mx_fc <- extract_forecast_matrix(
    fc_obj   = fc_obj,
    ages_fit = ages_fit,
    fc_years = fc_years
  )
  
  list(
    fit      = fit_obj,
    forecast = fc_obj,
    mx_fc    = mx_fc,
    years    = fc_years,
    ages     = ages_fit
  )
}

## --------------------------------------------------------------
## HU: standard Hyndman-Ullah functional demographic model
## Implemented with demography::fdm() and forecast.fdm()
## --------------------------------------------------------------

fit_forecast_hu <- function(MorData,
                            sex_series,
                            ages_fit,
                            fit_years,
                            forecast_end_year,
                            order = 6,
                            method = "classical") {
  fit_end <- max(fit_years)
  h <- forecast_end_year - fit_end
  
  if (h < 1) stop("Forecast horizon must be positive.")
  
  demog_fit <- demography::extract.years(
    data = MorData,
    years = fit_years
  )
  
  demog_fit_smooth <- demography::smooth.demogdata(demog_fit)
  
  hu_fit <- demography::fdm(
    data = demog_fit_smooth,
    series = sex_series,
    order = order,
    ages = ages_fit,
    max.age = max(ages_fit),
    method = method
  )
  
  hu_fc <- forecast(
    hu_fit,
    h = h
  )
  
  fc_years <- (fit_end + 1):forecast_end_year
  rates_fc <- as.matrix(hu_fc$rate[[sex_series]])
  
  mx_fc <- rates_fc[
    as.character(ages_fit),
    as.character(fc_years),
    drop = FALSE
  ]
  
  list(
    fit = hu_fit,
    forecast = hu_fc,
    mx_fc = clean_mx(mx_fc),
    years = fc_years,
    ages = ages_fit
  )
}

## --------------------------------------------------------------
## Camarda CP-splines: original source functions
## Uses PSinfant(), deltasFUN(), CPSfunction()
## --------------------------------------------------------------

fit_forecast_cpspline_camarda <- function(DataStMoMo,
                                          ages_fit,
                                          fit_years,
                                          forecast_end_year,
                                          infant = FALSE,
                                          lambda_lower = c(-4, 1),
                                          lambda_upper = c(0, 5),
                                          lambda_ngrid = 5,
                                          lambda_logscale = TRUE,
                                          cps_kappas = c(10^4, 10^4),
                                          verbose = FALSE) {
  fit_end <- max(fit_years)
  h <- forecast_end_year - fit_end
  
  if (h < 1) stop("Forecast horizon must be positive.")
  
  fc_years <- (fit_end + 1):forecast_end_year
  years_all <- fit_years[1]:forecast_end_year
  
  m <- length(ages_fit)
  n1 <- length(fit_years)
  n <- length(years_all)
  
  Dxt <- DataStMoMo$Dxt[
    match(ages_fit, DataStMoMo$ages),
    match(fit_years, DataStMoMo$years),
    drop = FALSE
  ]
  
  Ext <- DataStMoMo$Ext[
    match(ages_fit, DataStMoMo$ages),
    match(fit_years, DataStMoMo$years),
    drop = FALSE
  ]
  
  Y1 <- as.matrix(Dxt)
  E1 <- as.matrix(Ext)
  
  Y1[!is.finite(Y1) | Y1 < 0] <- 0
  E1[!is.finite(E1) | E1 <= 0] <- 0
  
  Y <- matrix(10, m, n)
  E <- matrix(10, m, n)
  Y[, seq_len(n1)] <- Y1
  E[, seq_len(n1)] <- E1
  
  WEI1 <- matrix(1, m, n1)
  WEI1[E1 <= 0] <- 0
  WEI <- cbind(WEI1, matrix(0, m, n - n1))
  
  BICinf <- function(par) {
    FITinf <- PSinfant(
      Y = Y1,
      E = E1,
      lambdas = par,
      WEI = WEI1,
      infant = infant,
      verbose = FALSE
    )
    FITinf$bic
  }
  
  OPTinf <- cleversearch(
    BICinf,
    lower = lambda_lower,
    upper = lambda_upper,
    ngrid = lambda_ngrid,
    logscale = lambda_logscale,
    verbose = FALSE
  )
  
  FITinf <- PSinfant(
    Y = Y1,
    E = E1,
    lambdas = OPTinf$par,
    WEI = WEI1,
    infant = infant,
    verbose = verbose
  )
  
  deltas <- deltasFUN(FITinf)
  
  S <- matrix(1, m, n)
  S[, seq_len(n1)] <- 0
  
  FITcon <- CPSfunction(
    Y = Y,
    E = E,
    lambdas = OPTinf$par,
    WEI = WEI,
    kappas = cps_kappas,
    deltas = deltas,
    S = S,
    infant = infant,
    verbose = verbose
  )
  
  ETA_hat <- FITcon$ETA
  mx_all <- clean_mx(exp(ETA_hat))
  dimnames(mx_all) <- list(as.character(ages_fit), as.character(years_all))
  
  mx_fc <- mx_all[
    as.character(ages_fit),
    as.character(fc_years),
    drop = FALSE
  ]
  
  list(
    fit = list(
      initial = FITinf,
      constrained = FITcon,
      optimal_lambdas = OPTinf,
      deltas = deltas,
      S = S
    ),
    forecast = NULL,
    mx_fc = clean_mx(mx_fc),
    years = fc_years,
    ages = ages_fit
  )
}

fit_and_forecast_model <- function(spec,
                                   DataStMoMo,
                                   MorData,
                                   sex_series,
                                   ages_fit,
                                   fit_years,
                                   forecast_end_year,
                                   model_name = NULL) {
  if (spec$type == "stmomo") {
    fit_forecast_stmomo(
      spec = spec,
      DataStMoMo = DataStMoMo,
      ages_fit = ages_fit,
      fit_years = fit_years,
      forecast_end_year = forecast_end_year,
      model_name = model_name
    )
  } else if (spec$type == "hu") {
    fit_forecast_hu(
      MorData = MorData,
      sex_series = sex_series,
      ages_fit = ages_fit,
      fit_years = fit_years,
      forecast_end_year = forecast_end_year,
      order = spec$order,
      method = spec$method
    )
  } else if (spec$type == "cpspline") {
    fit_forecast_cpspline_camarda(
      DataStMoMo = DataStMoMo,
      ages_fit = ages_fit,
      fit_years = fit_years,
      forecast_end_year = forecast_end_year,
      infant = spec$infant,
      lambda_lower = spec$lambda_lower,
      lambda_upper = spec$lambda_upper,
      lambda_ngrid = spec$lambda_ngrid,
      lambda_logscale = spec$lambda_logscale,
      cps_kappas = spec$cps_kappas,
      verbose = FALSE
    )
  } else {
    stop("Unknown model type.")
  }
}
