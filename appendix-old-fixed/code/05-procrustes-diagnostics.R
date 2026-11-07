set.seed(1234)
#!/usr/bin/env Rscript
#
# 05-procrustes-diagnostics.R
#
# How well does each chamber's human W-NOMINATE fit (01-fit-humans.R) line
# up with DW-NOMINATE, before vs. after the Procrustes rotation?
#   n_matched          humans matched to DW-NOMINATE by icpsr
#   angle_deg          rotation angle; reflected = TRUE if an axis was flipped
#   ss_* / rmse_*      squared / RMS distance to DW-NOMINATE, before and after
#   improvement_ratio  1 - ss_after / ss_before (share of discrepancy removed)
#   procrustes_corr    shape agreement with DW-NOMINATE (uncentered, 0-1)
#   spearman_dim*      rank correlation with DW-NOMINATE after alignment
#   dem_share_right    share of Democrats with coord1D > 0 (fold check; ~0 = fine)
#
# The old inter-model agreement check (one fit per LLM) no longer applies:
# there is one human fit per chamber.
#   -> results/procrustes_fit_stats.csv

suppressPackageStartupMessages(library(dplyr))

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f) == 1) return(dirname(normalizePath(f)))
  tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
}
script_dir   <- get_script_dir()
data_dir     <- file.path(dirname(script_dir), "data")
RESULTS_PATH <- paste0(file.path(dirname(script_dir), "results"), "/")

fit_stats <- bind_rows(lapply(c("house", "senate"), function(ch) {
  fit <- readRDS(file.path(data_dir, "derived", paste0(ch, "_humans.rds")))
  leg <- fit$legislators                                    # aligned; has dw1/dw2
  ok  <- complete.cases(leg[, c("coord1D", "coord2D", "dw1", "dw2")])
  dem <- leg$coord1D[ok & leg$party_code == 100]

  cbind(
    chamber = ch,
    fit$procrustes_diag,
    spearman_dim1   = cor(leg$coord1D[ok], leg$dw1[ok], method = "spearman"),
    spearman_dim2   = cor(leg$coord2D[ok], leg$dw2[ok], method = "spearman"),
    dem_share_right = mean(dem > 0)
  )
}))

out_fit <- paste0(RESULTS_PATH, "procrustes_fit_stats.csv")
write.csv(fit_stats, out_fit, row.names = FALSE)
message("Saved ", out_fit)
print(fit_stats)
