set.seed(1234)
#!/usr/bin/env Rscript
#
# 02-place-llms.R
# Usage: Rscript 02-place-llms.R
#
# APPENDIX (old models): places every LLM in repo-root data/Old-<affiliation>/votes-*.csv
# (plus data/Synthetic/) in this appendix's human W-NOMINATE space
# (data/derived/*_humans.rds, from 01-fit-humans.R): maximizes the likelihood of
# the LLM's yea/nay votes over its (Dim 1, Dim 2) position, holding every
# roll-call parameter, beta and the dimension weights at the human fit.
# SEs: bootstrap over roll calls (conditional on the human fit).
#
# Writes data/derived/{house,senate}_combined.rds. 
# Rerun whenever vote files change.

suppressPackageStartupMessages({
  library(dplyr)                 # I know, I know. (T.T)
  library(parallel)
})

N_BOOT    <- 200   # bootstrap resamples per LLM
MIN_VOTES <- 20    # skip an LLM with fewer yea/nay votes in a chamber
LLM_ICPSR <- 99999L

## Complicated mess to set the appropriate directories
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f) == 1) return(dirname(normalizePath(f)))
  tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
}
script_dir  <- get_script_dir()
data_dir    <- file.path(dirname(script_dir), "data")   # appendix-old-fixed/data/: voteview cache, derived output
votes_dir   <- file.path(dirname(dirname(script_dir)), "data")   # repo-root data/: LLM votes (read only)
derived_dir <- file.path(data_dir, "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

## Get all LLM vote files (+ Synthetic: random / majority / minority voters, placed the same way)
vote_files <- list.files(file.path(votes_dir, c("Old-Democrat", "Old-Neutral", "Old-Republican", "Synthetic")),  # old models only
                         pattern = "^votes-.*\\.csv$", full.names = TRUE)
## Catch errors
if (length(vote_files) == 0) stop("No votes-*.csv files under ", votes_dir)

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
    affiliation = sub("^Old-", "", basename(dirname(path))),   # Old-Democrat -> Democrat, so 04/11 work unchanged
    llm_model   = model,
    is_llm      = TRUE,
    n_models    = 1L,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 3. Each chamber: humans from 01 + every LLM; save
# ---------------------------------------------------------------------------
RNGkind("L'Ecuyer-CMRG"); set.seed(1234)   # reproducible bootstrap across parallel workers

for (k in c("house", "senate")) {
  fit <- readRDS(file.path(derived_dir, paste0(k, "_humans.rds")))

  ## Humans: drop members W-NOMINATE excluded (fewer than MINVOTES votes)
  humans <- fit$legislators[!is.na(fit$legislators$coord1D), ]
  humans <- humans %>%
    select(icpsr, bioname, party_code, chamber, coord1D, coord2D, se1D, se2D,
           correctYea, wrongYea, wrongNay, correctNay, GMP, CC) %>%
    mutate(affiliation = NA_character_, llm_model = NA_character_, is_llm = FALSE, n_models = 1L)

  ## LLMs, one per vote file, in parallel
  llms <- mclapply(vote_files, place_llm, fit = fit, mc.cores = min(16, detectCores()),
                   mc.preschedule = FALSE)   # one RNG stream per vote file: same SEs on any number of cores
  bad  <- sapply(llms, inherits, "try-error")
  if (any(bad)) stop("Placement failed for: ", paste(basename(vote_files[bad]), collapse = ", "))
  llms <- bind_rows(llms)

  combined <- bind_rows(humans, llms)
  out <- file.path(derived_dir, paste0(k, "_combined.rds"))
  saveRDS(combined, out)
  message(sprintf("%s: %d humans, %d LLMs -> %s", k, nrow(humans), nrow(llms), out))
}
