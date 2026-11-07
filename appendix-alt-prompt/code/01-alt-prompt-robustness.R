set.seed(1234)
#!/usr/bin/env Rscript
#
# 01-alt-prompt-robustness.R
# Usage: Rscript 01-alt-prompt-robustness.R
#
# Compares every model's votes under the alternative prompt (repo-root data/Alt-Prompt-Neutral/)
# with its votes under the original prompt (data/Neutral/). Two analyses saved to results/:
#
#   1. results/vote_agreement.csv
#      Vote-level agreement (share of roll calls with the same yea/nay vote), Cohen's kappa,
#      yea rates and switches, by chamber. Reference: the model's median agreement with every
#      other Neutral model under the original prompt.
#
#   2. results/placement_shift.csv
#      Places the alternative-prompt votes in replication-fixed's human W-NOMINATE space (same
#      likelihood and roll-call bootstrap as replication-fixed/code/02-place-llms.R) and compares
#      with the model's Neutral placement there:
#        - z test of the shift on each dimension: (alt - orig) / sqrt(se_orig^2 + se_alt^2);
#        - shift on Dim 1 as a share of the gap between the Dem and Rep human medians;
#        - rank on Dim 1 among the Neutral models, original vs alt;
#        - whether the 90% interval excludes the Dem/Rep median midpoint (as in 04-results.R).
#
# Needs replication-fixed/data/derived/{house,senate}_{humans,combined}.rds (run replication-fixed 01 and 02 first).

suppressPackageStartupMessages({
  library(dplyr)                 # I know, I know. (T.T)
  library(parallel)
})

N_BOOT    <- 200   # bootstrap resamples per LLM (as in replication-fixed 02)
MIN_VOTES <- 20    # skip an LLM with fewer yea/nay votes in a chamber
LLM_ICPSR <- 99999L
Z_CRIT    <- qnorm(0.95)   # 90% intervals, as in 04-results.R

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
repo_dir    <- dirname(dirname(script_dir))
votes_dir   <- file.path(repo_dir, "data")                                 # repo-root data/: LLM votes (read only)
derived_dir <- file.path(repo_dir, "replication-fixed", "data", "derived")  # human fit + Neutral placements (read only)
results_dir <- file.path(dirname(script_dir), "results")
dir.create(results_dir, showWarnings = FALSE)

## Alt-prompt vote files and their original-prompt counterparts
alt_files  <- list.files(file.path(votes_dir, "Alt-Prompt-Neutral"), pattern = "^votes-.*\\.csv$", full.names = TRUE)
orig_files <- file.path(votes_dir, "Neutral", basename(alt_files))
if (length(alt_files) == 0) stop("No votes-*.csv files under ", file.path(votes_dir, "Alt-Prompt-Neutral"))
if (any(!file.exists(orig_files))) stop("No Neutral counterpart for: ", paste(basename(alt_files[!file.exists(orig_files)]), collapse = ", "))
neutral_files <- list.files(file.path(votes_dir, "Neutral"), pattern = "^votes-.*\\.csv$", full.names = TRUE)

model_name <- function(path) sub("^votes-", "", sub("\\.csv$", "", basename(path)))


# ---------------------------------------------------------------------------
# 1. W-NOMINATE likelihood for one legislator, roll-call parameters fixed
#    Poole, Lewis, Lo & Carroll (2011, JSS 42(14)), Eqs. 2-6, pp. 3-4:
#    yea outcome = midpoint - spread, nay = midpoint + spread (Eq. 6);
#    logit choice probability (Eq. 4), as in wnominate.f90.
#    Checked by recovering humans' own W-NOMINATE estimates (r >= .997 Dim 1, >= .98 Dim 2)
# ---------------------------------------------------------------------------
neg_loglik <- function(x, y, rc, beta, w) {
  ## Squared weighted distance from x to an outcome point
  # u_yea and u_nay are reconstructions of the yea and nay policy utilities
  # We know which is which (+ or - the spread) based on the original Fortran 
  # of the wnominate R package:
  # SUMY=SUMY+(BBBB(K)*(XDATA(I,K)-ZMID(J,K)+DYN(J,K)))**2   | Yea: x - (mid - spread)
  # SUMN=SUMN+(BBBB(K)*(XDATA(I,K)-ZMID(J,K)-DYN(J,K)))**2   | Nay: x - (mid + spread)
  
  dist2 <- function(o1, o2) w[1]^2 * (x[1] - o1)^2 + w[2]^2 * (x[2] - o2)^2 # Eq. 2 numerator / Eq. 3
  u_yea <- beta * exp(-0.5 * dist2(rc$midpoint1D - rc$spread1D, rc$midpoint2D - rc$spread2D)) # Eq. 2
  u_nay <- beta * exp(-0.5 * dist2(rc$midpoint1D + rc$spread1D, rc$midpoint2D + rc$spread2D)) # Eq. 2
  p_yea <- plogis(u_yea - u_nay)            # logit: exp(u_y) / (exp(u_y) + exp(u_n)) Eq. 4
  p_obs <- ifelse(y == 1, p_yea, 1 - p_yea) # Cijl in Eq. 5
  -sum(log(pmax(p_obs, 1e-12)))             # Eq. 5 / negative log likelihood
}

## Smooth map from the plane onto the open unit disk (keeps x inside the circle)
## Enforce distance from origin < 1 (unit disk).
## Inputs coordinate pair v and returns point within unit disk.
to_disk   <- function(v)
{
  # Computes point on disk that is in the same direction as the 
  # unconstrained point v. The max(.,1e-12) avoids division by 0.
  v * tanh(sqrt(sum(v^2))) / max(sqrt(sum(v^2)), 1e-12)
} 

from_disk <- function(x) 
{ 
  # Computes the inverse of to_disk(.)
  # Takes a point x on the unit disk and finds the value v such that 
  # to_disk(v) = x. 
  r <- min(sqrt(sum(x^2)), 0.999)
  if (r < 1e-12) { 
    return(c(0, 0))
  } else {
    return(x * atanh(r) / r)
  } 
}

## MLE position; multistart unless a warm start is given (bootstrap)
place <- function(y, rc, beta, w, start = NULL) {
  # y: LLM's votes per roll call, 1=yea, 0=nay
  # rc: matching rows, fit$nom$rollcalls (the midpoints and spreads)
  # beta and w: the human fit's beta and dimension weights
  # start: optional starting ideal point
  
  # Defines negative loglikelihood
  # function of unconstrained point v \in (-inf, inf)
  # maps v to constrained x, returns negloglik of y at x (constrained)
  f <- function(v) neg_loglik(to_disk(v), y, rc, beta, w)
  
  # pick starting values of x
  starts <- if (is.null(start)){
    rbind(c(0, 0), 
          as.matrix(expand.grid(c(-0.6, 0, 0.6), 
                                c(-0.6, 0, 0.6))))
  }  else {
    matrix(start, 1)
  }
  
  # loops over starting values of x
  # minimizes f, negloglik
  fits <- lapply(seq_len(nrow(starts)), 
                 function(i) optim(from_disk(starts[i, ]), 
                                   f, 
                                   method = "BFGS"))
  # picks best likelihood values of v (unconstrained)
  # maps those values to x (constrained to unit disk)
  to_disk(fits[[which.min(sapply(fits, `[[`, "value"))]]$par)
}

# ---------------------------------------------------------------------------
# 2. Place one LLM in one chamber
# ---------------------------------------------------------------------------
place_llm <- function(path, fit) {
  rc   <- fit$nom$rollcalls
  beta <- as.numeric(fit$nom$beta)
  w    <- as.numeric(fit$nom$weights)

  ## LLM votes lined up with the human fit's roll calls (1 = yea, 0 = nay)
  v <- read.csv(path, stringsAsFactors = FALSE)
  y <- ifelse(v$vote == "yea", 
              1, 
              ifelse(v$vote == "nay", 
                     0, 
                     NA))[match(fit$rollcall_ids, 
                                v$vote_id)]
  used <- !is.na(y) & !is.na(rc$midpoint1D)          # roll calls W-NOMINATE kept
  if (sum(used) < MIN_VOTES) return(NULL)
  y  <- y[used]
  rc <- rc[used, ]

  x <- place(y, rc, beta, w)

  ## Bootstrap over roll calls, warm-started at x
  boot <- t(replicate(N_BOOT, {
    b <- sample(length(y), replace = TRUE)
    place(y[b], rc[b, ], beta, w, start = x)
  }))

  ## Into the DW-NOMINATE-aligned space (same rotation as the humans)
  xy     <- matrix(x, 1) %*% fit$R
  boot_r <- boot %*% fit$R

  ## Fit stats, same definitions as wnominate()'s legislator columns
  dist2 <- function(o1, o2) w[1]^2 * (x[1] - o1)^2 + w[2]^2 * (x[2] - o2)^2
  p_yea <- plogis(beta * exp(-0.5 * dist2(rc$midpoint1D - rc$spread1D, rc$midpoint2D - rc$spread2D)) -
                 beta * exp(-0.5 * dist2(rc$midpoint1D + rc$spread1D, rc$midpoint2D + rc$spread2D)))
  pred_yea <- p_yea > 0.5

  model <- sub("^votes-", "", sub("\\.csv$", "", basename(path)))
  data.frame(
    icpsr       = LLM_ICPSR,
    bioname     = model,
    party_code  = 0L,
    chamber     = "LLM",
    coord1D     = xy[1],
    coord2D     = xy[2],
    se1D        = sd(boot_r[, 1]),
    se2D        = sd(boot_r[, 2]),
    correctYea  = sum( pred_yea & y == 1),   # predicted yea, voted yea
    wrongYea    = sum( pred_yea & y == 0),   # predicted yea, voted nay
    wrongNay    = sum(!pred_yea & y == 1),   # predicted nay, voted yea
    correctNay  = sum(!pred_yea & y == 0),   # predicted nay, voted nay
    GMP         = exp(mean(log(ifelse(y == 1, p_yea, 1 - p_yea)))),
    CC          = mean(pred_yea == (y == 1)),
    affiliation = basename(dirname(path)),
    llm_model   = model,
    is_llm      = TRUE,
    n_models    = 1L,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 3. Vote-level agreement with the original prompt
# ---------------------------------------------------------------------------
message("=== Part 1: Vote-level agreement ===\n")

read_votes <- function(path) read.csv(path, stringsAsFactors = FALSE)[, c("vote_id", "vote")]

## Cohen's kappa for two yea/nay vectors
kappa <- function(a, b) {
  po <- mean(a == b)
  pe <- mean(a == "yea") * mean(b == "yea") + mean(a == "nay") * mean(b == "nay")
  (po - pe) / (1 - pe)
}

## Agreement stats on the roll calls both files voted yea/nay on
compare_votes <- function(orig, alt) {
  m <- inner_join(orig, alt, by = "vote_id", suffix = c("_orig", "_alt"))
  n_matched <- nrow(m)
  m <- m[m$vote_orig %in% c("yea", "nay") & m$vote_alt %in% c("yea", "nay"), ]
  data.frame(
    n_matched  = n_matched,
    n_valid    = nrow(m),
    agree      = mean(m$vote_orig == m$vote_alt),
    kappa      = kappa(m$vote_orig, m$vote_alt),
    yea_orig   = mean(m$vote_orig == "yea"),
    yea_alt    = mean(m$vote_alt == "yea"),
    yea_to_nay = sum(m$vote_orig == "yea" & m$vote_alt == "nay"),
    nay_to_yea = sum(m$vote_orig == "nay" & m$vote_alt == "yea")
  )
}

chambers <- c(all = "^R", house = "^RH", senate = "^RS")   # vote_id prefixes

agreement <- bind_rows(lapply(seq_along(alt_files), function(i) {
  orig   <- read_votes(orig_files[i])
  alt    <- read_votes(alt_files[i])
  others <- lapply(setdiff(neutral_files, orig_files[i]), read_votes)
  bind_rows(lapply(names(chambers), function(k) {
    keep <- function(v) v[grepl(chambers[[k]], v$vote_id), ]
    ## Reference: agreement with each other Neutral model under the original prompt
    ref <- sapply(others, function(o) compare_votes(keep(orig), keep(o))$agree)
    data.frame(model = model_name(alt_files[i]), chamber = k,
               compare_votes(keep(orig), keep(alt)),
               agree_other_models_median = median(ref))
  }))
}))

out_agree <- file.path(results_dir, "vote_agreement.csv")
write.csv(agreement, out_agree, row.names = FALSE)
message(sprintf("Saved %d rows to %s", nrow(agreement), out_agree))
print(agreement %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)

## LaTeX table: share of each prompt's votes that match the model's ORIGINAL No Affiliation (Neutral)
## votes, by model and chamber, on roll calls with a yea/nay vote under both prompts.
## "--" = no vote file for that model and prompt (e.g. a model run under the Neutral prompt only).
agree_with_orig <- function(orig, path, prefix) {
  if (!file.exists(path)) return(NA_real_)
  keep <- function(v) v[grepl(prefix, v$vote_id), ]
  compare_votes(keep(orig), keep(read_votes(path)))$agree
}

accuracy <- bind_rows(lapply(seq_along(alt_files), function(i) {
  orig <- read_votes(orig_files[i])
  bind_rows(lapply(c(House = "^RH", Senate = "^RS"), function(prefix) {
    data.frame(
      Model      = sub("__", ":", sub("^[^_]+__", "", model_name(alt_files[i]))),   # e.g. deepseek-reasoner, gemma4:26b
      Republican = agree_with_orig(orig, file.path(votes_dir, "Republican", basename(alt_files[i])), prefix),
      Democrat   = agree_with_orig(orig, file.path(votes_dir, "Democrat",   basename(alt_files[i])), prefix),
      `Alternative No Affiliation` = agree_with_orig(orig, alt_files[i], prefix),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }), .id = "Chamber")
})) %>%
  select(Model, Chamber, everything())

options(knitr.kable.NA = "--")
out_acc <- file.path(results_dir, "accuracy.tex")
writeLines(
  knitr::kable(accuracy, format = "latex", booktabs = TRUE, digits = 3, linesep = "",
               caption = "Proportion of each prompt's votes that agree with the model's votes under the original No Affiliation prompt."),
  out_acc)
message(sprintf("Saved %d rows to %s", nrow(accuracy), out_acc))
print(accuracy %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Placement shift: alt-prompt placement vs Neutral placement, same human space
# ---------------------------------------------------------------------------
message("\n=== Part 2: Placement shift ===\n")

RNGkind("L'Ecuyer-CMRG"); set.seed(1234)   # reproducible bootstrap across parallel workers

shift <- bind_rows(lapply(c("house", "senate"), function(k) {
  fit  <- readRDS(file.path(derived_dir, paste0(k, "_humans.rds")))
  comb <- readRDS(file.path(derived_dir, paste0(k, "_combined.rds")))

  ## Alt-prompt placements, one per vote file, in parallel
  alt <- mclapply(alt_files, place_llm, fit = fit, mc.cores = min(16, detectCores()),
                  mc.preschedule = FALSE)   # one RNG stream per vote file: same SEs on any number of cores
  bad <- sapply(alt, inherits, "try-error")
  if (any(bad)) stop("Placement failed for: ", paste(basename(alt_files[bad]), collapse = ", "))
  alt <- bind_rows(alt)

  ## Reference points: Dem and Rep human medians and their midpoint
  hum <- comb[!comb$is_llm, ]
  dem <- median(hum$coord1D[which(hum$party_code == 100)])
  rep <- median(hum$coord1D[which(hum$party_code == 200)])
  mid <- (dem + rep) / 2

  neutral  <- comb[comb$is_llm & comb$affiliation == "Neutral", ]
  excludes <- function(x, se) x - Z_CRIT * se > mid | x + Z_CRIT * se < mid

  bind_rows(lapply(alt$llm_model[alt$llm_model %in% neutral$llm_model], function(m) {
    o <- neutral[neutral$llm_model == m, ]
    a <- alt[alt$llm_model == m, ]
    ## Rank on Dim 1 among the Neutral models, with this model's alt placement swapped in
    swapped <- neutral$coord1D
    swapped[neutral$llm_model == m] <- a$coord1D
    data.frame(
      chamber       = k,
      model         = m,
      coord1D_orig  = o$coord1D, se1D_orig = o$se1D,
      coord1D_alt   = a$coord1D, se1D_alt  = a$se1D,
      coord2D_orig  = o$coord2D, se2D_orig = o$se2D,
      coord2D_alt   = a$coord2D, se2D_alt  = a$se2D,
      z1D           = (a$coord1D - o$coord1D) / sqrt(o$se1D^2 + a$se1D^2),
      z2D           = (a$coord2D - o$coord2D) / sqrt(o$se2D^2 + a$se2D^2),
      shift1D_party_gap = (a$coord1D - o$coord1D) / (rep - dem),
      rank1D_orig   = rank(neutral$coord1D)[neutral$llm_model == m],
      rank1D_alt    = rank(swapped)[neutral$llm_model == m],
      n_neutral     = nrow(neutral),
      dem_median    = dem,
      rep_median    = rep,
      midpoint      = mid,
      excl_mid_orig = excludes(o$coord1D, o$se1D),
      excl_mid_alt  = excludes(a$coord1D, a$se1D),
      stringsAsFactors = FALSE
    )
  }))
}))
shift$p1D <- 2 * pnorm(-abs(shift$z1D))
shift$p2D <- 2 * pnorm(-abs(shift$z2D))

out_shift <- file.path(results_dir, "placement_shift.csv")
write.csv(shift, out_shift, row.names = FALSE)
message(sprintf("Saved %d rows to %s", nrow(shift), out_shift))
print(shift %>% select(chamber, model, coord1D_orig, coord1D_alt, z1D, p1D, z2D, p2D, shift1D_party_gap,
                       rank1D_orig, rank1D_alt, excl_mid_orig, excl_mid_alt) %>%
        mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)
