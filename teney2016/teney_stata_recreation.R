# =============================================================================
# Teney (2016) -- recreation of the ORIGINAL Stata pipeline, run on the
# 2013-2019 Eurobarometer data used by the SCORE replication.
#
# Sources translated (in analyses/teney2016/original_script/):
#   eb_all.do      -> Part 1 (pool waves, harmonise variables, country codes,
#                     eb / eb_ctr identifiers, merge country-level data)
#   eb_ml_ESR.do   -> Parts 2-3 (DV construction, recodes, grand-mean
#                     centring, mixed models)
# The SCORE replication scripts (TENEY.code.r, teney*.R) are NOT used. Only the
# raw GESIS .sav files and the raw Eurostat .sav extracts are read.
#
# Run from analyses/teney2016/:   Rscript teney_stata_recreation.R
# Output goes to analyses/teney2016/stata_recreation_output/
# =============================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(lme4)
})

# ---- Settings ---------------------------------------------------------------
eb_dir   <- "data/Eurobarometer data"
es_dir   <- file.path(eb_dir, "EUROSTAT")
out_dir  <- "stata_recreation_output"
dir.create(out_dir, showWarnings = FALSE)

# Stata treats missing (.) as larger than any number, so Teney's line
#   replace edu3=3 if edu>19
# puts every respondent with missing education ("no full-time education" and
# refusals/DK) into high_edu. TRUE reproduces that behaviour exactly; FALSE
# leaves them missing (what was presumably intended).
STATA_MISSING_EDU_IS_HIGH <- TRUE

# Teney centred on the grand mean of HER sample, hard-coding the constants
# (munemp = unemp_rate - 8.76, mgrowth = growth - 1.41, mage = age - 48).
# "sample": centre on the grand mean of the new analysis sample (same procedure).
# "teney" : subtract her 2004-2013 constants instead.
CENTRING <- "sample"

# =============================================================================
# PART 1. eb_all.do: pool waves and harmonise
# =============================================================================

# Fieldwork month of each wave, coded the way Teney codes `eb` (YYMM:
# 1305 = May 2013, 1211 = Nov 2012, ...).
wave_info <- data.frame(
  survey = c(801, 812, 814, 823, 833, 843, 852, 862, 873, 883, 891, 903, 912, 915),
  eb     = c(1311, 1403, 1405, 1411, 1505, 1511, 1605, 1611, 1705, 1711,
             1803, 1811, 1903, 1906)
)

# Teney's 13 framing items, identified by their GESIS variable label so the
# question number (qa4/qa9/qa10/qa11/qa12/qa13) does not matter. "Euro",
# "Other" and "DK" exist in the battery but are not used in eb_ml_ESR.do.
item_labels <- c(
  peace    = "EU MEANING: PEACE",
  eco_pros = "EU MEANING: ECONOMIC PROSPERITY",
  demo     = "EU MEANING: DEMOCRACY",
  soc_prot = "EU MEANING: SOCIAL PROTECTION",
  trav     = "EU MEANING: TRAV[AE]L/STUDY/WORK ABROAD",  # 80.1 label has a typo
  div      = "EU MEANING: CULTURAL DIVERSITY",
  say      = "EU MEANING: STRONGER SAY IN THE WORLD",
  euro     = "EU MEANING: EURO",
  unemp    = "EU MEANING: UNEMPLOYMENT",
  bur      = "EU MEANING: BUREAUCRACY",
  waste    = "EU MEANING: WASTE OF MONEY",
  loss_id  = "EU MEANING: LOSS OF CULTURAL IDENTITY",
  crime    = "EU MEANING: MORE CRIME",
  border   = "EU MEANING: NOT ENOUGH FRONTIER CONTROL"
)

# Teney's country codes (`cty` in eb_all.do). Croatia has no code there and
# is therefore not in her sample; it is dropped here as well.
cty_codes <- c("FR" = 1, "BE" = 2, "NL" = 3, "DE-W" = 4, "DE-E" = 4, "IT" = 5,
               "LU" = 6, "DK" = 7, "IE" = 8, "GB-GBN" = 9, "GB-NIR" = 9, "GB" = 9,
               "GR" = 11, "ES" = 12, "PT" = 13, "FI" = 16, "SE" = 17, "AT" = 18,
               "CY" = 19, "CZ" = 20, "EE" = 21, "HU" = 22, "LV" = 23, "LT" = 24,
               "MT" = 25, "PL" = 26, "SK" = 27, "SI" = 28, "BG" = 29, "RO" = 30)
cty_lab <- c("1" = "FR", "2" = "BE", "3" = "NL", "4" = "DE", "5" = "IT", "6" = "LU",
             "7" = "DK", "8" = "IE", "9" = "GB", "11" = "GR", "12" = "ES",
             "13" = "PT", "16" = "FI", "17" = "SE", "18" = "AT", "19" = "CY",
             "20" = "CZ", "21" = "EE", "22" = "HU", "23" = "LV", "24" = "LT",
             "25" = "MT", "26" = "PL", "27" = "SK", "28" = "SI", "29" = "BG",
             "30" = "RO")
# Eurostat GEO code for each Teney country
cty_geo <- c("1" = "FR", "2" = "BE", "3" = "NL", "4" = "DE", "5" = "IT", "6" = "LU",
             "7" = "DK", "8" = "IE", "9" = "UK", "11" = "EL", "12" = "ES",
             "13" = "PT", "16" = "FI", "17" = "SE", "18" = "AT", "19" = "CY",
             "20" = "CZ", "21" = "EE", "22" = "HU", "23" = "LV", "24" = "LT",
             "25" = "MT", "26" = "PL", "27" = "SK", "28" = "SI", "29" = "BG",
             "30" = "RO")

read_wave <- function(f) {
  d <- read_sav(f, user_na = FALSE)   # SPSS user-missing (refusal/DK) -> NA
  labs <- vapply(d, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }, "")
  pick <- function(pattern) {
    hit <- names(d)[grepl(paste0("^", pattern, "$"), labs)]
    if (length(hit) != 1) stop(f, ": found ", length(hit), " variables for '", pattern, "'")
    as.numeric(d[[hit]])
  }
  items <- as.data.frame(lapply(item_labels, pick))
  num <- function(v) as.numeric(zap_labels(d[[v]]))
  data.frame(
    survey   = num("survey"),
    isocntry = as.character(zap_labels(d$isocntry)),
    d10      = num("d10"),      # gender
    d8       = num("d8"),       # age when stopped full-time education (exact)
    d11      = num("d11"),      # age (exact)
    d15a_r2  = num("d15a_r2"),  # occupation, 8 categories
    d25      = num("d25"),      # type of community
    items
  )
}

files <- list.files(eb_dir, pattern = "\\.sav$", full.names = TRUE)
eb <- bind_rows(lapply(files, function(f) { message("reading ", basename(f)); read_wave(f) }))
eb <- left_join(eb, wave_info, by = "survey")
stopifnot(!anyNA(eb$eb))

# gen female=0 / replace female=1 if d10==2
eb$female <- as.numeric(!is.na(eb$d10) & eb$d10 == 2)

# Education: d8 uses the same codes Teney produced for later waves
# (97 = no full-time education, 98 = still studying), so no recode is needed.
eb$edu <- eb$d8

# ses: d15a_r2 is coded exactly as Teney's ses (1 self-employed ... 8 students)
eb$ses <- eb$d15a_r2

eb$age <- eb$d11

# urban: 1 rural, 2 town, 3 city; DK (8) -> missing (as for z_d25 / k_d25 / j_d25)
eb$urban <- ifelse(eb$d25 %in% 1:3, eb$d25, NA)

# cty
eb$cty <- unname(cty_codes[eb$isocntry])
cat("\nRespondents dropped because country is outside Teney's 27:\n")
print(table(eb$isocntry[is.na(eb$cty)]))
eb <- eb[!is.na(eb$cty), ]

# eb_ctr = cty*10000 + eb  (country-wave identifier, level 2)
eb$eb_ctr <- eb$cty * 10000 + eb$eb

# joinby id using var_cty.dta: Teney's country-wave macro file is not
# available, so unemployment and real GDP growth are taken from Eurostat
# (une_rt_a, tec00115) for the survey's calendar year.
read_es <- function(f, name) {
  x <- read_sav(f)
  x <- data.frame(year = as.numeric(as.character(zap_labels(x$TIME))),
                  geo  = trimws(as.character(zap_labels(x$GEO))),
                  val  = suppressWarnings(as.numeric(as.character(zap_labels(x$value)))))
  names(x)[3] <- name
  x
}
macro <- full_join(read_es(file.path(es_dir, "Eurostat.une_rt_a.sav"), "unemp_rate"),
                   read_es(file.path(es_dir, "Eurostat.tec00115.sav"), "growth"),
                   by = c("year", "geo"))

eb$year_cal <- 2000 + eb$eb %/% 100
eb$geo      <- unname(cty_geo[as.character(eb$cty)])
eb <- left_join(eb, macro, by = c("year_cal" = "year", "geo"))
cat("\nRespondents without macro data:", sum(is.na(eb$unemp_rate) | is.na(eb$growth)), "\n")

# =============================================================================
# PART 2. eb_ml_ESR.do: variables
# =============================================================================

# gen year = ... (index used for the descriptive graphs only)
eb$year <- eb$year_cal - 2000

# sumeu: all 13 framing items (euro excluded). The wave-specific exceptions in
# the .do file (eb 203/303/906) do not apply: every 2013-19 wave has all items.
eb <- eb %>% mutate(
  sumeu = peace + eco_pros + demo + soc_prot + trav + div + say + unemp +
          bur + waste + loss_id + crime + border,
  cos = (peace + demo + trav + div + say) / sumeu,
  uti = (eco_pros + soc_prot) / sumeu,
  com = (unemp + loss_id + crime + border) / sumeu,
  lib = (waste + bur) / sumeu,
  # robustness check: framing shares without "unemployment" in the denominator
  sumeu_un = sumeu - unemp,
  cos_un = (peace + demo + trav + div + say) / sumeu_un,
  uti_un = (eco_pros + soc_prot) / sumeu_un,
  com_un = (loss_id + crime + border) / sumeu_un,
  lib_un = (waste + bur) / sumeu_un
)
# Stata: x/0 is missing (no item mentioned -> DV missing)
for (v in c("cos", "uti", "com", "lib", "cos_un", "uti_un", "com_un", "lib_un"))
  eb[[v]][!is.finite(eb[[v]])] <- NA

# replace edu=age if edu==98 / replace edu=. if edu==97
eb$edu[eb$edu %in% 98] <- eb$age[eb$edu %in% 98]
eb$edu[eb$edu %in% 97] <- NA

# gen edu3
eb$edu3 <- NA
eb$edu3[!is.na(eb$edu) & eb$edu < 16]               <- 1
eb$edu3[!is.na(eb$edu) & eb$edu >= 16 & eb$edu <= 19] <- 2
eb$edu3[!is.na(eb$edu) & eb$edu > 19]               <- 3
if (STATA_MISSING_EDU_IS_HIGH) eb$edu3[is.na(eb$edu)] <- 3
cat("\nedu3 (1 low, 2 mid, 3 high); missing edu coded high:", STATA_MISSING_EDU_IS_HIGH, "\n")
print(table(eb$edu3, useNA = "ifany"))

eb$edu3  <- factor(eb$edu3, levels = 1:3, labels = c("low_edu", "mid_edu", "high_edu"))
eb$ses   <- relevel(factor(eb$ses, levels = 1:8,
                           labels = c("self-emp", "manager", "white collar", "manual",
                                      "house person", "unemp", "retired", "student")),
                    ref = "manual")                       # b4.ses
eb$urban <- relevel(factor(eb$urban, levels = 1:3, labels = c("rural", "town", "city")),
                    ref = "city")                         # b3.urban
eb$cty_f    <- factor(cty_lab[as.character(eb$cty)])
eb$eb_ctr_f <- factor(eb$eb_ctr)

# Grand-mean centring (Teney: munemp = unemp_rate-8.76, mgrowth = growth-1.41,
# mage = age-48). Means are taken over respondents in the main analysis sample.
main_vars <- c("cos", "female", "age", "edu3", "ses", "urban", "unemp_rate", "growth")
cc <- complete.cases(eb[, main_vars])
if (CENTRING == "sample") {
  ctr <- c(unemp = mean(eb$unemp_rate[cc]), growth = mean(eb$growth[cc]),
           age = mean(eb$age[cc]))
} else {
  ctr <- c(unemp = 8.76, growth = 1.41, age = 48)
}
cat("\nCentring constants (", CENTRING, "): ", sep = "")
print(round(ctr, 3))
eb$munemp  <- eb$unemp_rate - ctr["unemp"]
eb$mgrowth <- eb$growth - ctr["growth"]
eb$mage    <- eb$age - ctr["age"]

saveRDS(eb, file.path(out_dir, "eb_stata_recreation.rds"))

# =============================================================================
# PART 3. eb_ml_ESR.do: mixed models
#   mixed y ... || cty: || eb_ctr: , covariance(unstructured)
#   = random intercepts for country and country-wave (nested), ML estimation,
#   no weights. covariance(unstructured) has no effect with intercepts only.
# =============================================================================

re   <- "(1 | cty_f) + (1 | eb_ctr_f)"
indiv <- "female + mage + edu3 + ses + urban"

# Model blocks from the .do file (sections *mod1, *mod2, *mod3 /
# "*model pr RR ESR"). Squared terms are DV-specific, as in the original:
# uti gets c.mgrowth##c.mgrowth, com gets c.munemp##c.munemp.
quad <- c(cos = "", uti = " + I(mgrowth^2)", com = " + I(munemp^2)", lib = "")
specs <- list()
for (dv in c("cos", "uti", "com", "lib")) {
  specs[[paste0("mod1_", dv)]] <- paste(dv, "~", indiv)
  specs[[paste0("mod2_", dv)]] <- paste0(dv, " ~ ", indiv, " + mgrowth + munemp", quad[dv])
  specs[[paste0("mod3_", dv)]] <- paste0(dv, " ~ ", indiv,
                                         " + mgrowth + munemp + mgrowth:edu3 + munemp:edu3",
                                         quad[dv])
}
# Robustness checks in the .do file: mod3 without ses; mod3 with the *_un DVs
specs[["mod3_cos_noses"]] <- "cos ~ female + mage + edu3 + urban + mgrowth + munemp + mgrowth:edu3 + munemp:edu3"
specs[["mod3_cos_un"]]    <- sub("^cos", "cos_un", specs[["mod3_cos"]])

fit_one <- function(f) lmer(as.formula(paste(f, "+", re)), data = eb, REML = FALSE)

tidy_fixef <- function(m, model) {
  s <- summary(m)$coefficients
  data.frame(model = model, term = rownames(s), b = s[, 1], se = s[, 2], z = s[, 3],
             p = 2 * pnorm(-abs(s[, 3])), row.names = NULL)
}

fits <- list(); coefs <- list(); info <- list()
for (nm in names(specs)) {
  message("fitting ", nm)
  m <- fit_one(specs[[nm]])
  fits[[nm]]  <- m
  coefs[[nm]] <- tidy_fixef(m, nm)
  vc <- as.data.frame(VarCorr(m))
  info[[nm]] <- data.frame(model = nm, N = nobs(m),
                           n_country = ngrps(m)[["cty_f"]],
                           n_country_wave = ngrps(m)[["eb_ctr_f"]],
                           sd_country = vc$sdcor[vc$grp == "cty_f"],
                           sd_country_wave = vc$sdcor[vc$grp == "eb_ctr_f"],
                           sd_resid = vc$sdcor[vc$grp == "Residual"],
                           logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m))
}
coefs <- bind_rows(coefs)
info  <- bind_rows(info)
write.csv(coefs, file.path(out_dir, "fixed_effects.csv"), row.names = FALSE)
write.csv(info,  file.path(out_dir, "model_info.csv"), row.names = FALSE)
saveRDS(fits, file.path(out_dir, "models.rds"))

# ---- Focal claim: Table 3, Model 1 (cosmopolitan), "Unempl. rate" -----------
# With c.munemp##edu3 the munemp coefficient is the slope for low_edu at the
# grand mean. Original (2004-2013): b = -0.00340, t = -4.03.
# Also report the unemployment slope in each education group (the quantity
# Teney plots with margins/coefplot).
slopes <- function(m, x = "munemp") {
  b <- fixef(m); V <- as.matrix(vcov(m))
  out <- lapply(c("low_edu", "mid_edu", "high_edu"), function(g) {
    w <- setNames(rep(0, length(b)), names(b)); w[x] <- 1
    if (g != "low_edu") {
      int <- intersect(c(paste0("edu3", g, ":", x), paste0(x, ":edu3", g)), names(b))
      stopifnot(length(int) == 1)
      w[int] <- 1
    }
    est <- sum(w * b); se <- sqrt(drop(t(w) %*% V %*% w))
    data.frame(edu3 = g, slope = est, se = se, z = est / se, p = 2 * pnorm(-abs(est / se)))
  })
  bind_rows(out)
}

sink(file.path(out_dir, "results.txt"), split = TRUE)
cat("Teney (2016) original Stata pipeline, recreated in R, on EB 80.1-91.5 (2013-2019)\n")
cat("Missing education coded high (Stata behaviour):", STATA_MISSING_EDU_IS_HIGH,
    " | centring:", CENTRING, "\n\n")
print(info, digits = 4)

cat("\n== Focal: mod3_cos ('model pr RR ESR', m1) ==\n")
foc <- coefs[coefs$model == "mod3_cos", ]
print(foc, digits = 4, row.names = FALSE)
cat("\nOriginal (2004-2013): munemp b = -0.00340, t = -4.03\n")
cat("\nUnemployment slope by education group, mod3_cos:\n")
print(slopes(fits[["mod3_cos"]]), digits = 4, row.names = FALSE)
cat("\nUnemployment slope by education group, mod3_cos_noses:\n")
print(slopes(fits[["mod3_cos_noses"]]), digits = 4, row.names = FALSE)
cat("\nUnemployment slope by education group, mod3_cos_un:\n")
print(slopes(fits[["mod3_cos_un"]]), digits = 4, row.names = FALSE)

cat("\n== Macro terms, all models ==\n")
print(coefs[grepl("munemp|mgrowth", coefs$term), ], digits = 4, row.names = FALSE)
sink()
