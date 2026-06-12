## HMD data block corresponding to section 3 of the original script.
## This block is executed by main.R after configuration and helper loading.

HMD_USER <- Sys.getenv("HMD_USER")
HMD_PASS <- Sys.getenv("HMD_PASS")

if (HMD_USER == "" || HMD_PASS == "") {
  stop("Set HMD_USER and HMD_PASS as environment variables.")
}

MorData <- hmd.mx(
  country  = country_code,
  username = HMD_USER,
  password = HMD_PASS,
  label    = country_code
)

DataStMoMo <- StMoMoData(MorData, series = sex_series)

all_years <- as.integer(DataStMoMo$years)
all_ages  <- as.integer(DataStMoMo$ages)

missing_ages <- setdiff(ages_fit, all_ages)

if (length(missing_ages) > 0) {
  warning(
    paste0(
      "These ages are not available and will be removed: ",
      paste(missing_ages, collapse = ", ")
    )
  )
  ages_fit <- intersect(ages_fit, all_ages)
}

if (!x_ret %in% ages_fit) {
  stop("x_ret must be included in ages_fit.")
}

if (!all(years_start:max(years_test) %in% all_years)) {
  stop("Some fitting/validation/test years are not available in HMD.")
}

cat("HMD observed years:", min(all_years), "-", max(all_years), "\n")
cat("Validation years:", min(years_valid), "-", max(years_valid), "\n")
cat("Test years:", min(years_test), "-", max(years_test), "\n")
cat("Best models used in ensemble:", n_best_models, "\n")
