set.seed(1234)
#!/usr/bin/env Rscript
#
# 03-model-fit.R
#
# Two analyses saved to results/:
#
#   1. results/model_fit_stats.csv
#      Goodness-of-fit statistics (correctclass, APRE, GMP) of the human
#      W-NOMINATE fit, one row per chamber (data/derived/*_humans.rds).
#
#   2. results/ideal_point_correlations.csv
#      Spearman rank correlations between the human legislator
#      ideal points from our models (house_combined.rds / senate_combined.rds)
#      and the corresponding DW-NOMINATE scores from Voteview, for both
#      dimension 1 and dimension 2.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ---------------------------------------------------------------------------
# 0. Paths
# ---------------------------------------------------------------------------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f) == 1) return(dirname(normalizePath(f)))
  tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
}

script_dir  <- get_script_dir()
data_dir    <- file.path(dirname(script_dir), "data")
results_dir <- file.path(dirname(script_dir), "results")
dir.create(results_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Goodness-of-fit statistics of the human W-NOMINATE fit (one per chamber)
# ---------------------------------------------------------------------------
message("=== Part 1: Goodness-of-fit statistics ===\n")

fit_out <- bind_rows(lapply(c("house", "senate"), function(ch) {
  f <- as.list(readRDS(file.path(data_dir, "derived", paste0(ch, "_humans.rds")))$nom$fits)
  data.frame(
    chamber        = ch,                 # 10-results.R selects rows by chamber
    correctclass1D = f$correctclass1D,
    correctclass2D = f$correctclass2D,
    apre1D         = f$apre1D,
    apre2D         = f$apre2D,
    gmp1D          = f$gmp1D,
    gmp2D          = f$gmp2D
  )
}))

out_fit <- file.path(results_dir, "model_fit_stats.csv")
write.csv(fit_out, out_fit, row.names = FALSE)
message(sprintf("\nSaved %d rows to %s", nrow(fit_out), out_fit))
print(fit_out)

# ---------------------------------------------------------------------------
# 2. Rank correlations vs Voteview DW-NOMINATE ideal points
# ---------------------------------------------------------------------------
message("\n=== Part 2: Rank correlations vs Voteview ===\n")

# Load combined data (humans from the single 01 fit)
house_combined  <- readRDS(file.path(data_dir, "derived", "house_combined.rds"))
senate_combined <- readRDS(file.path(data_dir, "derived", "senate_combined.rds"))

# Load voteview member files (congresses 117-119, both chambers)
vv_dir <- file.path(dirname(dirname(script_dir)), "data", "voteview")   # repo-root data/voteview/ (from 01)
vv_member_files <- list.files(vv_dir, pattern = "_members\\.csv$", full.names = TRUE)
if (length(vv_member_files) == 0) stop("No voteview member CSVs found in: ", vv_dir)

vv_members <- bind_rows(lapply(vv_member_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)
  df[, intersect(c("congress", "chamber", "icpsr",
                   "nominate_dim1", "nominate_dim2"), colnames(df))]
}))

# Average DW-NOMINATE scores per legislator across congresses (within chamber)
vv_house <- vv_members %>%
  filter(chamber == "House") %>%
  mutate(icpsr = as.integer(icpsr)) %>%
  group_by(icpsr) %>%
  summarise(vv_dim1 = mean(nominate_dim1, na.rm = TRUE),
            vv_dim2 = mean(nominate_dim2, na.rm = TRUE),
            .groups = "drop")

vv_senate <- vv_members %>%
  filter(chamber == "Senate") %>%
  mutate(icpsr = as.integer(icpsr)) %>%
  group_by(icpsr) %>%
  summarise(vv_dim1 = mean(nominate_dim1, na.rm = TRUE),
            vv_dim2 = mean(nominate_dim2, na.rm = TRUE),
            .groups = "drop")

# Join and compute Spearman correlations
compute_corr <- function(combined, vv_ref, chamber_label) {
  humans <- combined %>%
    filter(!is_llm) %>%
    mutate(icpsr = as.integer(icpsr)) %>%
    inner_join(vv_ref, by = "icpsr") %>%
    filter(!is.na(coord1D), !is.na(coord2D),
           !is.na(vv_dim1),  !is.na(vv_dim2))

  n <- nrow(humans)
  r1 <- cor(humans$coord1D, humans$vv_dim1, method = "spearman")
  r2 <- cor(humans$coord2D, humans$vv_dim2, method = "spearman")

  message(sprintf(
    "%s: n=%d  rho_dim1=%.4f  rho_dim2=%.4f", chamber_label, n, r1, r2))

  data.frame(
    chamber         = chamber_label,
    n_legislators   = n,
    spearman_dim1   = r1,
    spearman_dim2   = r2,
    stringsAsFactors = FALSE
  )
}

corr_house  <- compute_corr(house_combined,  vv_house,  "House")
corr_senate <- compute_corr(senate_combined, vv_senate, "Senate")
corr_out    <- bind_rows(corr_house, corr_senate)

out_corr <- file.path(results_dir, "ideal_point_correlations.csv")
write.csv(corr_out, out_corr, row.names = FALSE)
message(sprintf("\nSaved to %s", out_corr))

invisible(list(fit_stats = fit_out, correlations = corr_out))
