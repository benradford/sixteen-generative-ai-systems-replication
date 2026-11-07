set.seed(1234)
#!/usr/bin/env Rscript
#
# 01-fit-humans.R
# Usage: Rscript 01-fit-humans.R [TRIALS]   (default TRIALS = 50; use 1 for a quick test)
#
# One W-NOMINATE fit per chamber on HUMAN legislators only (congresses 117-119),
# over every roll call any LLM voted on. 02-place-llms.R then places each LLM
# in this fixed space, so every LLM is measured against the same human fit.
#
# Replaces the old per-LLM fits (code/old/06-nominate.R): refitting the humans
# once per LLM let the House solution fall into a "folded" local optimum
# (Democrats squeezed right of center) for some roll-call sets and not others.
#
# Starting dimension weights (uweights): one 1-trial fit per value in UW_GRID, keep the value
# whose humans line up best with DW-NOMINATE (Procrustes correlation, both dims), then refit with
# TRIALS. Every other wnominate() argument stays as below. Same process as
# appendix-old-fixed/code/01-fit-humans.R and appendix-single-model-fixed/code/01-fit-all.R.
#
# Saves data/derived/{house,senate}_humans.rds and results/uweights_grid.csv.

suppressPackageStartupMessages({
  library(pscl)
  library(wnominate)
  library(dplyr)
  library(tidyr)
  library(parallel)
})

# ---------------------------------------------------------------------------
# 0. Settings
# ---------------------------------------------------------------------------
args   <- commandArgs(trailingOnly = TRUE)
TRIALS <- if (length(args) >= 1) as.integer(args[1]) else 50L   # wnominate() bootstrap trials (SEs); JSS 2011 p. 4 recommends >= 50

LOP      <- 0.025   # wnominate default: drop roll calls with < 2.5% in the minority
MINVOTES <- 20      # wnominate default: drop members with < 20 votes
UW_GRID  <- seq(0.25, 2, by = 0.25)   # uweights tried (one trial each); 0.5 = wnominate default

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f) == 1) return(dirname(normalizePath(f)))
  tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
}
script_dir  <- get_script_dir()
data_dir    <- file.path(dirname(script_dir), "data")   # replication-fixed/data/: derived output
votes_dir   <- file.path(dirname(dirname(script_dir)), "data")   # repo-root data/: LLM votes (read only)
cache_dir   <- file.path(votes_dir, "voteview")   # repo-root data/voteview/: shared voteview cache
derived_dir <- file.path(data_dir, "derived")
results_dir <- file.path(dirname(script_dir), "results")
dir.create(cache_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Roll calls: every vote_id in any LLM vote file
# ---------------------------------------------------------------------------
# Get list of all vote file paths
vote_files <- list.files(file.path(votes_dir, c("Democrat", "Neutral", "Republican")),
                         pattern = "^votes-.*\\.csv$", full.names = TRUE)
# Stop if no vote files present in expected directories
if (length(vote_files) == 0) stop("No votes-*.csv files under ", votes_dir)

# Print number of files and number of unique roll call votes
all_ids <- unique(unlist(lapply(vote_files, function(f) read.csv(f, stringsAsFactors = FALSE)$vote_id)))
message(sprintf("%d vote files, %d roll calls", length(vote_files), length(all_ids)))

# ---------------------------------------------------------------------------
# 2. Voteview votes and members (downloaded once, cached in repo-root data/voteview/)
# ---------------------------------------------------------------------------
vv_base <- "https://voteview.com/static/data/out"

download_vv <- function(type, ch, congress) {
  fname <- sprintf("%s%d_%s.csv", ch, congress, type)
  local <- file.path(cache_dir, fname)
  if (!file.exists(local))
    download.file(sprintf("%s/%s/%s", vv_base, type, fname), local, quiet = TRUE)
  read.csv(local, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# 3. Procrustes: rotate/reflect the fit onto DW-NOMINATE (no translation, no scaling)
#    Disallow translation / scaling to maintain (-1,1) range.
# ---------------------------------------------------------------------------
procrustes_align <- function(leg) {
  # Expects leg: dataframe
  #         leg$coord1D, leg$coord2D: new model coordinates to be rotated
  #         leg$dw1, leg$dw2: existing model coordinates to be aligned to
  ok <- complete.cases(leg[, c("coord1D", "coord2D", "dw1", "dw2")])
  Y  <- as.matrix(leg[ok, c("coord1D", "coord2D")])   # this fit
  X  <- as.matrix(leg[ok, c("dw1", "dw2")])           # DW-NOMINATE
  sv <- svd(crossprod(Y, X))
  R  <- sv$u %*% t(sv$v)                              # orthogonal rotation

  ## Rotate coordinates
  xy <- as.matrix(leg[, c("coord1D", "coord2D")]) %*% R
  ## Rotate SEs (diagonal covariance through R; total variance preserved)
  se1 <- leg$se1D; se2 <- leg$se2D
  aligned <- leg
  aligned$coord1D <- xy[, 1]
  aligned$coord2D <- xy[, 2]
  aligned$se1D    <- sqrt(R[1, 1]^2 * se1^2 + R[2, 1]^2 * se2^2)
  aligned$se2D    <- sqrt(R[1, 2]^2 * se1^2 + R[2, 2]^2 * se2^2)

  ## Diagnostics (read by 05-procrustes-diagnostics.R)
  n <- nrow(Y)
  ss_before <- sum((Y - X)^2)
  ss_after  <- sum((Y %*% R - X)^2)
  diag <- data.frame(
    n_matched         = n,
    angle_deg         = atan2(R[2, 1], R[1, 1]) * 180 / pi,
    reflected         = det(R) < 0,
    ss_before         = ss_before,
    ss_after          = ss_after,
    rmse_before       = sqrt(ss_before / (2 * n)),
    rmse_after        = sqrt(ss_after  / (2 * n)),
    improvement_ratio = if (ss_before > 0) 1 - ss_after / ss_before else NA_real_,
    procrustes_corr   = sum(sv$d) / sqrt(sum(Y^2) * sum(X^2))
  )

  list(legislators = aligned, R = R, diag = diag)
}

# ---------------------------------------------------------------------------
# 4. Build one chamber's rollcall object (once; reused by every fit below)
# ---------------------------------------------------------------------------
prep_chamber <- function(ch) {
  ## Input: ch, one of "H" (House) or "S" (Senate)
  ch_long <- if (ch == "H") "House" else "Senate"

  ## Votes: yea = 1, nay = 6, everything else missing
  votes <- bind_rows(lapply(117:119, function(k)
    download_vv("votes", ch, k)[, c("congress", "rollnumber", "icpsr", "cast_code")]))
  votes$vote_id   <- sprintf("R%s%d%04d", ch, votes$congress, votes$rollnumber)
  votes           <- votes[votes$vote_id %in% all_ids, ]
  votes$vote_code <- ifelse(votes$cast_code %in% 1:3, 1L,
                     ifelse(votes$cast_code %in% 4:6, 6L, NA_integer_))

  ## Member x roll call matrix (members pooled across congresses by icpsr)
  ## Ugh: Magrittr pipes. I'm sorry to anyone trying to read this.
  ## Builds the roll call votes matrix by member.
  wide <- votes %>%                                                        # start with one row per member per vote
    group_by(icpsr, vote_id) %>%                                           # create table: member by vote
    summarise(vote_code = first(na.omit(vote_code)), .groups = "drop") %>% # take first non-missing vote code per member-vote
    pivot_wider(names_from = vote_id, values_from = vote_code)             # reshape long to wide table
  vote_mat <- as.matrix(wide[, -1])                                        # drop the icpsr column
  storage.mode(vote_mat) <- "integer"                                      # strictly type matrix to avoid future issues
  icpsr <- as.integer(wide$icpsr)                                          # keep member icpsr numbers as a seperate vector

  ## Member info + DW-NOMINATE (averaged across congresses)
  members <- bind_rows(lapply(117:119, function(k) download_vv("members", ch, k))) %>%
    filter(chamber == ch_long) %>%                             # keep only members in house currently being fit
    mutate(icpsr = as.integer(icpsr)) %>%                      # convert icpsr id to integer to match above
    group_by(icpsr) %>%                                        # look within member
    summarise(bioname    = first(bioname),                     # get name associated with icpsr number
              party_code = first(party_code),                  # get party code
              chamber    = first(chamber),                     # get chamber
              dw1        = mean(nominate_dim1, na.rm = TRUE),  # get pre-estimated dw-nom dim 1, avg. over session
              dw2        = mean(nominate_dim2, na.rm = TRUE),  # get pre-estimated dw-nom dim 2, avg. over session
              .groups    = "drop")                             # drop grouping factor column
  legis <- left_join(data.frame(icpsr = icpsr), members, by = "icpsr") # join with our icpsr, match order to vote_mat

  ## Polarity: highest DW1 / DW2 among members W-NOMINATE keeps (>= MINVOTES votes)
  n_votes  <- rowSums(!is.na(vote_mat))                        # num votes per member
  eligible <- which(n_votes >= MINVOTES & !is.na(legis$dw1))   # which members have enough votes?
  polarity <- c(eligible[which.max(legis$dw1[eligible])],      # get max member in dw1 
                eligible[which.max(legis$dw2[eligible])])      # get max member in dw2
  message(sprintf("%s: %d members x %d roll calls; polarity %s / %s", ch_long,
                  nrow(vote_mat), ncol(vote_mat), legis$bioname[polarity[1]], legis$bioname[polarity[2]]))

  ## Create the rollcall object wnominate requires
  rc <- rollcall(
    data        = vote_mat,
    yea         = 1L,
    nay         = 6L,
    missing     = NA,
    notInLegis  = 9L,
    legis.names = paste0("ICPSR_", icpsr),
    legis.data  = legis[, c("icpsr", "bioname", "party_code", "chamber")],
    desc        = sprintf("Humans, %s 117-119", ch_long)
  )

  list(rc = rc, legis = legis, polarity = polarity, ch_long = ch_long, rollcall_ids = colnames(vote_mat))
}

# ---------------------------------------------------------------------------
# 5. Fit one chamber at one uweights value
# ---------------------------------------------------------------------------
fit_chamber <- function(pr, uw, trials) {
  ## Input: pr, output of prep_chamber(); uw, starting dimension weights; trials, bootstrap trials
  ch_long <- pr$ch_long

  ## Estimate our human W-NOMINATE model
  nom <- wnominate(
    rcObject = pr$rc,
    dims     = 2,                              # wnominate default
    ubeta    = 15,                             # wnominate default
    polarity = pr$polarity,
    minvotes = MINVOTES,
    lop      = LOP,
    trials   = trials,
    uweights = uw,                             # only argument tuned (section 6)
    verbose  = FALSE
  )

  ## Align to DW-NOMINATE; keep the raw fit too
  raw <- nom$legislators
  leg <- cbind(raw, pr$legis[, c("dw1", "dw2")])
  aligned <- procrustes_align(leg)

  ## Check for bad local optimum: Democrats should sit left of center on Dim 1
  dem <- aligned$legislators$coord1D[aligned$legislators$party_code == 100]
  message(sprintf("%s (uweights %.2f, trials %d): Dem median coord1D %.2f (share > 0: %.2f), Procrustes corr %.3f",
                  ch_long, uw, trials, median(dem, na.rm = TRUE), mean(dem > 0, na.rm = TRUE), aligned$diag$procrustes_corr))
  if (mean(dem > 0, na.rm = TRUE) > 0.5) warning(ch_long, " fit looks bad: most Democrats are right of center")

  list(
    nom             = nom,                     # rollcalls, beta, weights, fits (02 needs these)
    legislators     = aligned$legislators,     # humans, Procrustes-aligned
    raw_legislators = raw,                     # humans, before alignment
    R               = aligned$R,               # rotation; 02 applies it to the LLMs
    procrustes_diag = aligned$diag,            # procrustes transform diagnostics
    rollcall_ids    = pr$rollcall_ids,         # order of nom$rollcalls rows
    settings        = list(lop = LOP, minvotes = MINVOTES, trials = trials, uweights = uw)
  )
}

# ---------------------------------------------------------------------------
# 6. Pick uweights: one 1-trial fit per chamber x UW_GRID value (in parallel)
# ---------------------------------------------------------------------------
RNGkind("L'Ecuyer-CMRG")  # make seed robust across mclapply cores
set.seed(1234)            # reproducible random streams across parallel workers
preps <- mclapply(c("H", "S"), prep_chamber, mc.cores = 2)
names(preps) <- c("house", "senate")
## Stop if either chamber fails
for (k in names(preps)) if (inherits(preps[[k]], "try-error") || is.null(preps[[k]])) stop(k, " prep failed: ", preps[[k]])

grid <- expand.grid(uweights = UW_GRID, chamber = names(preps), stringsAsFactors = FALSE)  # one row per chamber x uweights
grid_fits <- mclapply(seq_len(nrow(grid)),
                      function(i) fit_chamber(preps[[grid$chamber[i]]], grid$uweights[i], trials = 1L),
                      mc.cores = min(nrow(grid), detectCores()),
                      mc.preschedule = FALSE)   # one RNG stream per grid fit: same results on any number of cores
## Stop if any grid fit fails
bad <- sapply(grid_fits, function(f) inherits(f, "try-error") || is.null(f))
if (any(bad)) stop("Grid fit failed: ", paste(grid$chamber[bad], grid$uweights[bad], collapse = ", "))

grid$procrustes_corr <- sapply(grid_fits, function(f) f$procrustes_diag$procrustes_corr)     # humans vs DW-NOMINATE, both dims
grid$gmp2D           <- sapply(grid_fits, function(f) as.list(f$nom$fits)$gmp2D)             # fit of the human model
grid$selected        <- FALSE
for (k in names(preps)) {                                                                  # best per chamber (first if tied)
  i <- which(grid$chamber == k)
  grid$selected[i[which.max(grid$procrustes_corr[i])]] <- TRUE
}
rm(grid_fits)                                                                              # free memory before the full fits

## Report the selected arguments
out <- file.path(results_dir, "uweights_grid.csv")
write.csv(grid, out, row.names = FALSE)
print(grid)
for (k in names(preps))
  message(sprintf("%s: selected uweights = %.2f (Procrustes corr %.3f); other arguments: dims = 2, minvotes = %d, lop = %.3f, ubeta = 15 (default)",
                  k, grid$uweights[grid$chamber == k & grid$selected], max(grid$procrustes_corr[grid$chamber == k]), MINVOTES, LOP))
message("Saved ", out)

# ---------------------------------------------------------------------------
# 7. Full fits (TRIALS) at the selected uweights, and save
# ---------------------------------------------------------------------------
set.seed(1234)            # same streams whatever the grid did
fits <- mclapply(names(preps),
                 function(k) fit_chamber(preps[[k]], grid$uweights[grid$chamber == k & grid$selected], trials = TRIALS),
                 mc.cores = 2)
names(fits) <- names(preps)
## Stop the fit if either chamber fails
for (k in names(fits)) if (inherits(fits[[k]], "try-error") || is.null(fits[[k]])) stop(k, " fit failed: ", fits[[k]])

## Output to the derived data folder
for (k in names(fits)) {
  out <- file.path(derived_dir, paste0(k, "_humans.rds"))
  saveRDS(fits[[k]], out)
  message("Saved ", out)
}
