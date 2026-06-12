# Data Availability and Provenance

## Human Mortality Database

The mortality analysis uses Italian central death rates and the underlying
death and exposure matrices from the Human Mortality Database (HMD):
<https://www.mortality.org/>.

- Population: Italy (`ITA`)
- Series: male, female, and total
- Ages fitted and tested: 60-100
- Historical years used: 1960-2019
- Liability valuation years: 2020-2050
- Cohort projection horizon: sufficient to follow age-67 cohorts to age 100
- Access method: `demography::hmd.mx()`
- Redistribution: not included in this repository

HMD requires registration and acceptance of its terms of use. Each user must
obtain credentials and define the `HMD_USER` and `HMD_PASS` environment
variables. The repository never writes credentials to disk.

Without HMD access, the full mortality-model estimation, validation, test,
and liability results cannot be regenerated. Unit tests for deterministic
utilities and checks of the public EIOPA input can still be run.

## EIOPA Risk-Free Term Structures

The paper uses the EIOPA euro annual zero-coupon spot-rate term structures
without volatility adjustment, including the official interest-rate up and
down stresses, with reference date 31 December 2019.

Included file:

`data/eiopa/EIOPA_RFR_20191231_Term_Structures.xlsx`

Source cited in the paper: European Insurance and Occupational Pensions
Authority, "Risk-free rate previous releases: Monthly technical information,
December 2019".

SHA-256:

`7E940457DBA4FDFDF92C53A52BAE7AA7D199C598D471B4AC057DDA152CE49C1F`

The code reads the `Euro` column from:

- `RFR_spot_no_VA`
- `Spot_NO_VA_shock_UP`
- `Spot_NO_VA_shock_DOWN`

The other EIOPA workbooks supplied during repository preparation
(`PD_Cod`, `Qb_SW`, and `VA_portfolios`) are not used in the paper and are
therefore excluded.

## Derived Data

The rounded manuscript values under `data/reference/` are verification
inputs transcribed from the proof; they are not generated results.

All files under `output/<population>/` and `graph/<population>/` are derived
from HMD and EIOPA inputs by `main.R`. The repository includes lightweight
male verification outputs. The complete RDS object is generated locally but
is not distributed because of its size.
