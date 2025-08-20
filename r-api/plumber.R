library(plumber)
library(jsonlite)

source("Inventory_SW-5.R")  # pastikan file ini ada di folder r-api

# Helper kecil
`%||%` <- function(a, b) if (is.null(a)) b else a

num <- function(x, default=NA_real_) {
  if (is.null(x) || is.na(suppressWarnings(as.numeric(x)))) return(default)
  as.numeric(x)
}

# Helper: safely call a function if it exists, matching only supported args
safe_call <- function(fname, args, fallback = NULL){
  if (!exists(fname, mode = "function")) return(fallback)
  fun <- get(fname, mode = "function")
  fx <- names(formals(fun))
  matched <- args[intersect(names(args), fx)]
  tryCatch(do.call(fun, matched), error = function(e) fallback)
}

# CORS filter so Next.js/Flutter can call from different origins
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

# Healthcheck
#* @get /health
function(){ list(status="ok", time=as.character(Sys.time())) }

# Kalkulasi total (enteric + manure)
#* @serializer unboxedJSON
#* @post /calculate/total
function(req, res){
  payload <- tryCatch(jsonlite::fromJSON(req$postBody), error=function(e) NULL)
  if (is.null(payload)) { res$status <- 400; return(list(error="Invalid JSON")) }

  # Ambil input dasar
  LW   <- num(payload$LW, 250)
  ADG  <- num(payload$ADG, 0.3)
  DMD  <- num(payload$DMD_percent, 57.25)
  DEp  <- num(payload$DE_percent_for_IPCC, 54.4682)
  CP   <- num(payload$CP_percent_diet, 7.3)
  milk <- num(payload$milk_kg_day, 0)
  prod <- payload$productionSystem %||% "Intensive"
  catg <- payload$animalCategory   %||% "Adult Male"

  # TODO: panggil fungsi di Inventory_SW-5.R (enteric & manure) jika sudah tersedia
  # ---- ENTERIC (sementara: hitung GEI & EF mendekati rumus R) ----
  GE_density <- 18.45 # MJ/kg DM
  MJ_per_kg_CH4 <- 55.65
  # Try to use original R functions if available; otherwise fallback to simple approximation
  arglist <- list(
    LW = LW,
    ADG = ADG,
    DMD_percent = DMD,
    DE_percent_for_IPCC = DEp,
    CP_percent_diet = CP,
    milk_kg_day = milk,
    productionSystem = prod,
    animalCategory = catg
  )

  DMI <- safe_call("calculate_dmi", arglist, fallback = (0.02*LW + 4.0*ADG))
  GEI <- safe_call("calculate_ge_intake_local", c(list(DMI_kg_day = DMI), arglist), fallback = DMI * GE_density)
  Ym  <- if (identical(catg, "Imported Cattle")) 4.0 else 6.5
  EF_enteric <- (GEI * (Ym/100) / MJ_per_kg_CH4) * 365
  # Track which method is used (helps verify if R functions were found)
  has_calc_dmi <- exists("calculate_dmi", mode = "function")
  has_calc_gei <- exists("calculate_ge_intake_local", mode = "function")
  dmi_method <- if (has_calc_dmi) "R:calculate_dmi" else "fallback:0.02*LW+4*ADG"
  gei_method <- if (has_calc_gei) "R:calculate_ge_intake_local" else "fallback:DMI*GE_density"
  # Log to server console for quick debugging
  message(sprintf("[plumber] DMI=%.6f (method=%s) | GEI=%.6f (method=%s)", DMI, dmi_method, GEI, gei_method))

  # ---- MANURE (dynamic placeholders; ganti ke IPCC Tier 2 saat siap) ----
  # Perkiraan cepat: ~2.5 g CH4 per kg LW per hari
  manure_ch4_kg_year <- round(0.0025 * as.numeric(LW) * 365, 3)
  # Perkiraan sederhana N2O: skala terhadap CP% dan LW
  manure_n2o_kg_year <- round(0.0003 * as.numeric(CP) * as.numeric(LW), 3)

  # Konversi CO2e (IPCC AR6 GWP100)
  gwp_ch4 <- 27.2
  gwp_n2o <- 273
  co2e_ton <- (
    EF_enteric * gwp_ch4 + manure_ch4_kg_year * gwp_ch4 + manure_n2o_kg_year * gwp_n2o
  ) / 1000

  list(
  input = list(
    LW = unbox(LW), ADG = unbox(ADG), DMD_percent = unbox(DMD),
    DE_percent_for_IPCC = unbox(DEp), CP_percent_diet = unbox(CP),
    milk_kg_day = unbox(milk),
    productionSystem = unbox(prod), animalCategory = unbox(catg)
  ),
  enteric = list(
    ef_enteric_kg_ch4_year = unbox(round(EF_enteric, 5)),
    ym_percent = unbox(Ym),
    dmi_kg_day = unbox(round(DMI, 5)),
    gei_mj_day = unbox(round(GEI, 3)),
    methods = list(dmi = unbox(dmi_method), gei = unbox(gei_method))
  ),
  manure = list(
    ch4_kg_year = unbox(manure_ch4_kg_year),
    n2o_kg_year = unbox(manure_n2o_kg_year)
  ),
  total = list(
    co2e_ton_year = unbox(round(co2e_ton, 3))
  ),
  factor_version = unbox("2025-08-20")
)
}