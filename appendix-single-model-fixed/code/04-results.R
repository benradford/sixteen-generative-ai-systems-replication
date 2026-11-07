set.seed(1234)

if (!requireNamespace("e1071", quietly = TRUE))
  install.packages("e1071", repos = "https://cloud.r-project.org")
if (!requireNamespace("ggrepel", quietly = TRUE))
  install.packages("ggrepel", repos = "https://cloud.r-project.org")
if (!requireNamespace("patchwork", quietly = TRUE))
  install.packages("patchwork", repos = "https://cloud.r-project.org")

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(e1071)
library(ggrepel)
library(patchwork)
library(viridisLite)
library(knitr)
library(pBrackets)
library(clubSandwich)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f) == 1) return(dirname(normalizePath(f)))
  tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
}

script_dir   <- get_script_dir()
DATA_PATH    <- paste0(file.path(dirname(script_dir), "data"), "/")            # derived output
VV_PATH      <- paste0(file.path(dirname(dirname(script_dir)), "data", "voteview"), "/")   # repo-root data/voteview/ (from 01)
VOTES_PATH   <- paste0(file.path(dirname(dirname(script_dir)), "data"), "/")   # repo-root data/: LLM votes (read only)
RESULTS_PATH <- paste0(file.path(dirname(script_dir), "results"), "/")

################################################################################
### COLOR PALETTE
################################################################################
lgreen  <- "#B2FF8C"; green  <- "#32FF00"
lred    <- "#FF99BF"; red    <- "#E51932"
lblue   <- "#A5EDFF"; blue   <- "#19B2FF"
orange  <- "#FF7F00"; lorange <- "#FFBF7F"
purple  <- "#654CFF"; yellow <- "#FFFF32"
lgray   <- "#DDDDDD"; gray   <- "#888888"

################################################################################
### HELPER FUNCTIONS
################################################################################

to_tex <- function(x) {
  nm <- deparse(substitute(x))
  paste0("\\newcommand{\\", nm, "}{", x, "}")
}

modal <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

short_name <- function(model) {
  lookup <- c(
    "deepseek__deepseek-chat"                  = "DS-C",
    "deepseek__deepseek-reasoner"              = "DS-R",
    "gemini__gemini-3-flash-preview"           = "Gemini-3.0",
    "gemini__gemini-3.1-flash-lite-preview"    = "Gemini-3.1",
    "grok__grok-4-1-fast-non-reasoning"        = "Grok4.1-NR",
    "grok__grok-4-1-fast-reasoning"            = "Grok4.1-R",
    "grok__grok-4.20-0309-non-reasoning"       = "Grok4.2-NR",
    "grok__grok-4.20-0309-reasoning"           = "Grok4.2-R",
    "ollama__gemma4__26b-a4b-it-q4_K_M"        = "Gemma4",
    "ollama__gemma4__26b"                      = "Gemma4",
    "ollama__gpt-oss-safeguard__20b"           = "GPT-OSS-S",
    "ollama__gpt-oss__20b"                     = "GPT-OSS",
    "ollama__llama3.3__70b"                    = "Llama3.3",
    "ollama__llama4__16x17b"                   = "Llama4",
    "openai__gpt-5.4"                          = "GPT-5.4",
    "openai__gpt-5.4-mini"                     = "GPT-5.4m",
    "openai__gpt-5.4-nano"                     = "GPT-5.4n",
    "random"                                  = "Random",
    "synthetic__random"                        = "Random",
    "synthetic__majority"                      = "Majority",
    "synthetic__minority"                      = "Minority"
  )
  ifelse(model %in% names(lookup), unname(lookup[model]), model)
}

get_provider <- function(model) {
  case_when(
    str_starts(model, "deepseek__") ~ "DeepSeek",
    str_starts(model, "gemini__")   ~ "Gemini",
    str_starts(model, "grok__")     ~ "Grok",
    str_starts(model, "ollama__")   ~ "Ollama",
    str_starts(model, "openai__")   ~ "OpenAI",
    model == "random"              ~ "Random",
    TRUE                           ~ "Other"
  )
}

## Maps each model to the company that *produced* it (not the API service used).
## Ollama-hosted models are attributed to their original creator.
get_company <- function(model) {
  case_when(
    str_starts(model, "deepseek__")                                        ~ "DeepSeek",
    str_starts(model, "gemini__")                                          ~ "Google",
    str_starts(model, "grok__")                                            ~ "xAI",
    str_starts(model, "ollama__gemma4")                                    ~ "Google",
    model %in% c("ollama__gpt-oss-safeguard__20b", "ollama__gpt-oss__20b") ~ "OpenAI",
    model %in% c("ollama__llama3.3__70b", "ollama__llama4__16x17b")        ~ "Meta",
    str_starts(model, "openai__")                                          ~ "OpenAI",
    model == "random"                                                     ~ "Random",
    TRUE                                                                  ~ "Other"
  )
}

################################################################################
### LOAD COMBINED WNOMINATE MODELS
################################################################################
HOUSE  <- readRDS(paste0(DATA_PATH, "derived/house_combined.rds"))
SENATE <- readRDS(paste0(DATA_PATH, "derived/senate_combined.rds"))

HOUSE  <- HOUSE[!(HOUSE$is_llm  & HOUSE$llm_model  == "random"), ]
SENATE <- SENATE[!(SENATE$is_llm & SENATE$llm_model == "random"), ]

## Synthetic voters (random / majority / minority): set aside for the two figures only
SYN_HOUSE  <- HOUSE[HOUSE$affiliation   %in% "Synthetic", ]     # keep synthetic rows for nominate + coord1D plots
SYN_SENATE <- SENATE[SENATE$affiliation %in% "Synthetic", ]
HOUSE      <- HOUSE[!(HOUSE$affiliation   %in% "Synthetic"), ]  # everything else runs on humans + LLMs as before
SENATE     <- SENATE[!(SENATE$affiliation %in% "Synthetic"), ]

model_fit <- read.csv(paste0(RESULTS_PATH, "model_fit_stats.csv"))
ip_corr   <- read.csv(paste0(RESULTS_PATH, "ideal_point_correlations.csv"))

################################################################################
### LOAD RAW VOTE DATA
## Vote files live in data/Democrat/, data/Neutral/, data/Republican/
## as votes-[model-name].csv.
##
## Format differences:
##   Neutral     – 5 cols: X, vote_id, model, yea_first, vote  (named header)
##   Democrat/   – 4 cols: vote_id, model, yea_first, vote      (unnamed header)
##   Republican/
##
## Each (model, affiliation-folder) is a distinct LLM legislator.
## Multiple rows per vote_id within a file reflect re-presentations of the same
## bill (varying yea/nay ordering); de-duplicate by taking the first occurrence.
################################################################################

load_vote_files <- function() {
  affiliations <- c("Democrat", "Neutral", "Republican")

  rows <- lapply(affiliations, function(affil) {
    dir      <- paste0(VOTES_PATH, affil, "/")
    csvfiles <- list.files(dir, pattern = "^votes-.*\\.csv$", full.names = TRUE)

    lapply(csvfiles, function(f) {
      model_name <- sub("^votes-", "", sub("\\.csv$", "", basename(f)))
      d <- read.csv(f, stringsAsFactors = FALSE)

      if (ncol(d) == 5) {
        # Neutral: named columns  X | vote_id | model | yea_first | vote
        d <- d[, c("vote_id", "vote")]
      } else {
        # Democrat / Republican: unnamed  vote_id | model | yea_first | vote
        d <- d[, c(1L, 4L)]
        names(d) <- c("vote_id", "vote")
      }

      d <- d[d$vote %in% c("yea", "nay"), ]
      d <- d[!duplicated(d$vote_id), ]          # keep first occurrence per bill
      d$vote_binary  <- as.integer(d$vote == "yea")
      d$model        <- model_name
      d$affiliation  <- affil
      d$label        <- paste0(short_name(model_name), "-",
                                substr(affil, 1L, 1L))  # e.g. "DS-R1-D"
      d
    })
  })

  do.call(rbind, unlist(rows, recursive = FALSE))
}

## voteview cast_code: 1 = Yea, 6 = Nay
load_human_votes <- function(chamber) {
  code <- if (chamber == "House") "H" else "S"
  vl <- lapply(c(117, 118, 119), function(cong) {
    v <- read.csv(sprintf("%s%s%d_votes.csv",   VV_PATH, code, cong))
    m <- read.csv(sprintf("%s%s%d_members.csv", VV_PATH, code, cong))
    v <- v[v$cast_code %in% c(1, 6), ]
    v$vote_binary <- ifelse(v$cast_code == 1, 1L, 0L)
    v$congress    <- cong
    merge(v, m[, c("icpsr", "party_code")], by = "icpsr", all.x = TRUE)
  })
  do.call(rbind, vl)
}

## Initialize variables.tex
writeLines(paste0("%% Generated on: ", Sys.Date(), " ", Sys.time()),
           paste0(RESULTS_PATH, "variables.tex"))

################################################################################
################################################################################
## SECTION 1: NOMINATE SCATTER PLOTS  (with correct SVM decision boundary)
##
## SVM formula uses  ~ coord2D + coord1D  so that:
##   w[1] = weight for coord2D (y-axis)
##   w[2] = weight for coord1D (x-axis)
## and abline(rho/w[1], -w[2]/w[1]) correctly draws
##   coord2D = rho/w[coord2D]  -  (w[coord1D]/w[coord2D]) * coord1D
################################################################################
################################################################################

PROVIDER_PCH <- c(
  "DeepSeek" = 21, "Gemini" = 22, "Grok" = 23,
  "Ollama"   = 24, "OpenAI" = 25, "Random" = 4
)
AFFIL_FILLS <- c("Democrat" = blue, "Neutral" = yellow, "Republican" = red)

nominate_plot <- function(data, syn = data[0, ], show_legend = FALSE) {
  ## syn: synthetic voters (SYN_SENATE / SYN_HOUSE); drawn as labelled black crosses
  syn$label <- short_name(syn$llm_model)                   # Random / Majority / Minority
  humans <- data[!data$is_llm, ]
  llms   <- data[data$is_llm, ]
  llms$label       <- short_name(llms$llm_model)
  llms$provider    <- factor(get_provider(llms$llm_model), levels = names(PROVIDER_PCH))
  llms$affiliation <- factor(llms$affiliation,
                             levels = c("Democrat", "Neutral", "Republican"))

  ## Unit circle
  theta     <- seq(0, 2 * pi, length.out = 200)
  circle_df <- data.frame(x = cos(theta), y = sin(theta))

  ## SVM decision boundary (human Dem vs Rep only)
  ## Use complete cases only: svm() drops NAs internally so svm_fit$index
  ## addresses the complete-cases frame, not the original humans_2p.
  humans_2p <- humans[humans$party_code %in% c(100, 200), ]
  humans_2p <- humans_2p[complete.cases(humans_2p[, c("coord1D", "coord2D")]), ]
  svm_fit   <- svm(as.factor(party_code) ~ coord2D + coord1D,
                   data = humans_2p, kernel = "linear", scale = FALSE,
                   cost=1e6)   # hard margin: humans are separable, so no human falls inside the margin
  w   <- colSums(c(svm_fit$coefs) * as.matrix(svm_fit$SV))
  b   <- svm_fit$rho
  svm_int  <- b / w[1]           # coord2D intercept (decision boundary)
  svm_slp  <- -w[2] / w[1]      # slope in coord1D
  ## w[1]'s sign is an arbitrary artifact of the SVM's class encoding and can
  ## flip chamber-to-chamber (e.g. once coord2D is Procrustes-aligned to
  ## DW-NOMINATE, which is not a clean party axis on Dim 2 the way Dim 1 is).
  ## pmax/pmin here guarantee svm_int_pos is always the upper hyperplane and
  ## svm_int_neg the lower one, regardless of that sign -- otherwise, when
  ## w[1] < 0, (b+1)/w[1] ends up BELOW (b-1)/w[1], and the margin ribbon
  ## below (ymin from svm_int_neg, ymax from svm_int_pos) silently ends up
  ## empty (ymin > ymax everywhere), dropping the shading with no warning.
  svm_hyper_a <- (b + 1) / w[1] # class-wise hyperplane: w·x - b = +1
  svm_hyper_b <- (b - 1) / w[1] # class-wise hyperplane: w·x - b = -1
  svm_int_pos <- pmax(svm_hyper_a, svm_hyper_b)
  svm_int_neg <- pmin(svm_hyper_a, svm_hyper_b)

  ## Support vector points with full-name labels ("First Last" from "LAST, First")
  sv_df <- humans_2p[svm_fit$index, ]
  sv_df$sv_label <- {
    last  <- sub(",.*", "", sv_df$bioname)
    first <- trimws(sub("^[^,]+,\\s*", "", sv_df$bioname))
    paste0(substr(first, 1, 1), ". ", tools::toTitleCase(tolower(last)))
  }

  ## Named politicians to always label (if present in this chamber's data)
  pol_patterns <- paste0("^(", paste(c("VANCE", "RUBIO", "WARREN", "WARNER", "PELOSI", "JORDAN"), collapse = "|"), "),")
  pol_df <- humans[grepl(pol_patterns, humans$bioname), ]
  pol_df <- pol_df[!duplicated(pol_df$bioname), ]
  pol_df$pol_label <- {
    last  <- sub(",.*", "", pol_df$bioname)
    first <- trimws(sub("^[^,]+,\\s*", "", pol_df$bioname))
    paste0(substr(first, 1, 1), ". ", tools::toTitleCase(tolower(last)))
  }

  ## Margin shading: band between the two class hyperplanes, clipped to unit disk
  x_seq      <- seq(-1, 1, length.out = 500)
  circle_cap <- sqrt(1 - x_seq^2)
  margin_df  <- data.frame(
    x    = x_seq,
    ymin = pmax(svm_int_neg + svm_slp * x_seq, -circle_cap),
    ymax = pmin(svm_int_pos + svm_slp * x_seq,  circle_cap)
  )
  margin_df <- margin_df[margin_df$ymin < margin_df$ymax, ]

  ## Clipped line paths: sample points along each line, keep those inside disk
  clip_line <- function(intercept, slope) {
    xs <- seq(-1, 1, length.out = 500)
    ys <- intercept + slope * xs
    ok <- xs^2 + ys^2 <= 1
    data.frame(x = xs[ok], y = ys[ok])
  }
  line_mid  <- clip_line(svm_int,     svm_slp)
  line_pos  <- clip_line(svm_int_pos, svm_slp)
  line_neg  <- clip_line(svm_int_neg, svm_slp)

  ## Democrat- and Republican-prompted LLMs are labelled; force-repel applied below
  llms_lab <- llms[llms$affiliation %in% c("Democrat", "Republican"), ]  # was Neutral only

  ## Dummy data for human legend entries (nothing plotted; legend only)
  human_leg_df <- data.frame(
    party_label = factor(c("Rep. Human", "Dem. Human", "Ind. Human"),
                         levels = c("Rep. Human", "Dem. Human", "Ind. Human")),
    x = NA_real_, y = NA_real_
  )

  p <- ggplot() +
    coord_fixed(ratio = 1, xlim = c(-1.05, 1.05), ylim = c(-1.2, 1.1),
                expand = FALSE) +
    theme_void() +
    theme(
      legend.position      = if (show_legend) "left" else "none",
      legend.justification = "center",
      legend.key.size      = unit(0.42, "cm"),
      legend.text          = element_text(size = 8),
      legend.title         = element_text(size = 8.5, face = "bold"),
      legend.background    = element_rect(fill = NA, color = NA),
      legend.spacing.y     = unit(0.1, "cm")
    ) +
    ## Unit circle and reference lines
    geom_path(data = circle_df, aes(x = x, y = y),
              linewidth = 0.4, color = "black") +
    ## Margin interior shading (clipped to unit circle)
    geom_ribbon(data = margin_df, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "gray93", alpha = 1, inherit.aes = FALSE) +
    geom_path(data = circle_df, aes(x = x, y = y),
              linewidth = 0.4, color = "black") +
    geom_path(data = line_mid,  aes(x = x, y = y),
              linetype = "dashed",  linewidth = 0.6, color = "black") +
    geom_path(data = line_pos,  aes(x = x, y = y),
              linetype = "dotted",  linewidth = 0.6, color = "black") +
    geom_path(data = line_neg,  aes(x = x, y = y),
              linetype = "dotted",  linewidth = 0.6, color = "black") +
    ## Human background points (fixed color/fill; not in auto-legend)
    geom_point(data = humans[humans$party_code == 200, ],
               aes(x = coord1D, y = coord2D),
               color = red,   fill = lred,   shape = 21,
               size = 2.5, stroke = 0.3, show.legend = FALSE) +
    geom_point(data = humans[humans$party_code == 100, ],
               aes(x = coord1D, y = coord2D),
               color = blue,  fill = lblue,  shape = 23,
               size = 2.5, stroke = 0.3, show.legend = FALSE) +
    geom_point(data = humans[humans$party_code == 328, ],
               aes(x = coord1D, y = coord2D),
               color = green, fill = lgreen, shape = 25,
               size = 2.5, stroke = 0.3, show.legend = FALSE) +
    ## Dummy layer: drives the "Humans" legend section only (nothing plotted)
    geom_point(data = human_leg_df,
               aes(x = x, y = y, color = party_label),
               size = 0,
               show.legend = c(colour = TRUE, fill = FALSE,
                               shape = FALSE, size = FALSE)) +
    scale_color_manual(
      "Humans",
      values = c("Rep. Human" = red, "Dem. Human" = blue, "Ind. Human" = green)
    ) +
    ## LLM points: shape = provider, fill = affiliation
    geom_point(data = llms,
               aes(x = coord1D, y = coord2D,
                   shape = provider, fill = affiliation),
               color = "black", size = 3, stroke = 0.5) +
    scale_shape_manual("AI Provider",   values = PROVIDER_PCH) +
    scale_fill_manual("AI Affiliation", values = AFFIL_FILLS) +
    guides(
      ## Humans: show the correct shape/fill/color for each party
      color = guide_legend(
        override.aes = list(
          shape  = c(21,    23,    25),
          fill   = c(lred,  lblue, lgreen),
          color  = c(red,   blue,  green),
          size   = 2.5,
          stroke = 0.5
        )
      ),
      ## LLM affiliation: filled circles in affiliation color
      fill  = guide_legend(override.aes = list(shape = 21, color = "black",
                                               size = 3)),
      ## LLM provider: gray-filled shapes
      shape = guide_legend(override.aes = list(fill  = "gray80",
                                               color = "black", size = 3))
    ) +
    ## Support vector labels
    geom_text_repel(
      data          = sv_df,
      aes(x = coord1D, y = coord2D, label = sv_label),
      size          = 2.5,
      box.padding   = 0.3,
      point.padding = 0.2,
      segment.size  = 0.25,
      segment.color = "gray30",
      # max.overlaps  = Inf,
      seed          = 42
    ) +
    ## Named politician labels
    geom_text_repel(
      data          = pol_df,
      aes(x = coord1D, y = coord2D, label = pol_label),
      size          = 2.5,
      box.padding   = 0.3,
      point.padding = 0.2,
      segment.size  = 0.25,
      segment.color = "gray30",
      max.overlaps  = Inf,
      seed          = 123
    ) +
    ## Force-projected labels for Democrat / Republican LLMs
    geom_text_repel(
      data          = llms_lab,
      aes(x = coord1D, y = coord2D, label = label),
      size          = 2.5,
      box.padding   = 0.3,
      point.padding = 0.2,
      segment.size  = 0.25,
      segment.color = "gray50",
      max.overlaps  = Inf,
      seed          = 42
    ) +
    ## Synthetic voters: black crosses + labels (no legend entry)
    geom_point(data = syn, aes(x = coord1D, y = coord2D),
               shape = 4, color = "black", size = 3, stroke = 1) +
    geom_text_repel(
      data          = syn,
      aes(x = coord1D, y = coord2D, label = label),
      size          = 2.5,
      fontface      = "italic",                             # italic to set apart from LLM labels
      box.padding   = 0.3,
      point.padding = 0.2,
      segment.size  = 0.25,
      segment.color = "gray50",
      max.overlaps  = Inf,
      seed          = 42
    ) +
    ## Axis arrows
    annotate("segment", x = -0.9, xend = -0.7, y = -0.9, yend = -0.9,
             arrow = arrow(length = unit(0.07, "in"), type = "open")) +
    annotate("segment", x = -0.9, xend = -0.9, y = -0.9, yend = -0.7,
             arrow = arrow(length = unit(0.07, "in"), type = "open")) +
    annotate("text", x = -0.66, y = -0.90, label = "Dim 1",
             hjust = 0, size = 3) +
    annotate("text", x = -0.90, y = -0.66, label = "Dim 2",
             vjust = 0, size = 3)

  p
}

p_senate <- nominate_plot(SENATE, syn = SYN_SENATE, show_legend = TRUE) +
  labs(title = "Senate") +
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold"))

p_house  <- nominate_plot(HOUSE,  syn = SYN_HOUSE,  show_legend = TRUE) +
  labs(title = "House") +
  theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold"))

combined_nominate <- (p_senate / p_house) +
  plot_layout(guides = "collect") &
  theme(legend.position   = "left",
        legend.justification = "center")

ggsave(paste0(RESULTS_PATH, "nominate_combined.pdf"),
       combined_nominate, width = 6.5, height = 8.5)


################################################################################
## DISTANCE BETWEEN AVERAGE HUMAN PARTIES
################################################################################
RepMinusDemSenate <- sprintf(
  mean(SENATE$coord1D[!SENATE$is_llm & SENATE$party_code == 200], na.rm = TRUE) -
    mean(SENATE$coord1D[!SENATE$is_llm & SENATE$party_code == 100], na.rm = TRUE),
  fmt = "%1.2f")
RepMinusDemHouse <- sprintf(
  mean(HOUSE$coord1D[!HOUSE$is_llm & HOUSE$party_code == 200], na.rm = TRUE) -
    mean(HOUSE$coord1D[!HOUSE$is_llm & HOUSE$party_code == 100], na.rm = TRUE),
  fmt = "%1.2f")

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(RepMinusDemSenate), fileConn)
writeLines(to_tex(RepMinusDemHouse),  fileConn)
close(fileConn)

################################################################################
################################################################################
## SECTION 2: SVM PARTY PREDICTION MATRIX
################################################################################
################################################################################

svm_senate <- svm(as.factor(party_code) ~ coord2D + coord1D,
                  data   = SENATE[!SENATE$is_llm & SENATE$party_code %in% c(100, 200), ],
                  kernel = "linear", scale = FALSE,
                  cost=1e6)   # hard margin: humans are separable, so no human falls inside the margin
svm_house  <- svm(as.factor(party_code) ~ coord2D + coord1D,
                  data   = HOUSE[!HOUSE$is_llm & HOUSE$party_code %in% c(100, 200), ],
                  kernel = "linear", scale = FALSE,
                  cost=1e6)   # hard margin: humans are separable, so no human falls inside the margin

senate_llm <- SENATE[SENATE$is_llm, ]
house_llm  <- HOUSE[HOUSE$is_llm, ]
senate_pred_dv       <- predict(svm_senate,
                                newdata        = senate_llm[, c("coord2D", "coord1D")],
                                decision.values = TRUE)
senate_llm$svm_pred  <- ifelse(as.character(senate_pred_dv) == "100", "Dem", "Rep")
senate_llm$in_margin <- abs(as.numeric(attr(senate_pred_dv, "decision.values"))) < 1

house_pred_dv        <- predict(svm_house,
                                newdata        = house_llm[, c("coord2D", "coord1D")],
                                decision.values = TRUE)
house_llm$svm_pred   <- ifelse(as.character(house_pred_dv) == "100", "Dem", "Rep")
house_llm$in_margin  <- abs(as.numeric(attr(house_pred_dv, "decision.values"))) < 1

cat("Senate LLMs inside SVM margin:", sum(senate_llm$in_margin, na.rm = TRUE),
    "of", nrow(senate_llm), "\n")
cat("House LLMs inside SVM margin:",  sum(house_llm$in_margin,  na.rm = TRUE),
    "of", nrow(house_llm),  "\n")

senate_llm$short <- short_name(senate_llm$llm_model)
house_llm$short  <- short_name(house_llm$llm_model)

senate_llm$pred_fill <- paste0(senate_llm$svm_pred,
                                ifelse(senate_llm$in_margin, "_light", "_dark"))
house_llm$pred_fill  <- paste0(house_llm$svm_pred,
                                ifelse(house_llm$in_margin,  "_light", "_dark"))

make_pred_matrix <- function(df) {
  pivot_wider(df[, c("short", "affiliation", "pred_fill")],
              names_from  = "affiliation",
              values_from = "pred_fill")
}

PRED_FILL_COLORS <- c(
  "Dem_light" = lblue, "Dem_dark" = blue,
  "Rep_light" = lred,  "Rep_dark" = red
)
PRED_FILL_LABELS <- c(
  "Dem_light" = "Dem (in margin)",  "Dem_dark" = "Dem (out of margin)",
  "Rep_light" = "Rep (in margin)",  "Rep_dark" = "Rep (out of margin)"
)

plot_pred_matrix <- function(wide_df, title = NULL) {
  long_df <- pivot_longer(wide_df, cols = -short,
                          names_to = "Affiliation", values_to = "PredFill")
  long_df$Affiliation <- factor(long_df$Affiliation,
                                levels = c("Democrat", "Neutral", "Republican"))
  long_df$short      <- factor(long_df$short, levels = rev(sort(unique(long_df$short))))
  long_df$Prediction <- sub("_.*", "", long_df$PredFill)

  ggplot(long_df, aes(x = Affiliation, y = short, fill = PredFill)) +
    geom_tile(color = "white", linewidth = 0.5, na.rm = TRUE) +
    geom_text(aes(label = Prediction), size = 3, na.rm = TRUE) +
    scale_fill_manual(values   = PRED_FILL_COLORS,
                      labels   = PRED_FILL_LABELS,
                      na.value = "#eeeeee", name = "Predicted\nParty") +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid  = element_blank(),
          legend.position = "right")
}

ggsave(paste0(RESULTS_PATH, "senate_party_predictions.pdf"),
       plot  = plot_pred_matrix(make_pred_matrix(senate_llm), "Senate - SVM Party Prediction"),
       width = 5, height = 4.5)
ggsave(paste0(RESULTS_PATH, "house_party_predictions.pdf"),
       plot  = plot_pred_matrix(make_pred_matrix(house_llm), "House - SVM Party Prediction"),
       width = 5, height = 4.5)

################################################################################
################################################################################
## SECTION 3: LINEAR MODELS
##
## Regression uses all legislators (human + LLM) from the combined WNOMINATE
## model.  Binary indicators are 0 for the irrelevant group:
##   human_dem / human_rep / human_ind  – human party membership
##   company_*                          – producing company for LLMs
##   affil_dem / affil_rep              – partisan prompt (Neutral is baseline)
## Baseline = a Neutral-prompted LLM from Random/Other (no human, no company).
##
## Standard errors are CR2 (Bell-McCaffrey) cluster-robust, clustered by LLM
## model so that the Democrat/Neutral/Republican prompts of one model are not
## treated as independent; each human legislator is their own cluster. Tests
## use per-coefficient Satterthwaite degrees of freedom (clubSandwich), which
## reflect how many model clusters identify each coefficient.
################################################################################
################################################################################

prep_reg_df <- function(data) {
  df <- data

  ## Human party indicators (0 for all LLM rows)
  df$human_dem <- as.integer(!df$is_llm & !is.na(df$party_code) & df$party_code == 100)
  df$human_rep <- as.integer(!df$is_llm & !is.na(df$party_code) & df$party_code == 200)
  df$human_ind <- as.integer(!df$is_llm & !is.na(df$party_code) & df$party_code == 328)

  ## LLM company indicators (0 for human rows; baseline = Random / Other)
  company <- ifelse(df$is_llm, get_company(df$llm_model), "none")
  df$company_xai      <- as.integer(company == "xAI")
  df$company_openai   <- as.integer(company == "OpenAI")
  df$company_meta     <- as.integer(company == "Meta")
  df$company_google   <- as.integer(company == "Google")
  df$company_deepseek <- as.integer(company == "DeepSeek")

  ## Local model indicator: 1 if run via Ollama, 0 otherwise (including humans)
  df$local_model <- as.integer(df$is_llm & !is.na(df$llm_model) &
                                  str_starts(df$llm_model, "ollama__"))

  ## LLM prompt affiliation indicators (0 for humans and Neutral-prompted LLMs)
  df$affil_dem <- as.integer(df$is_llm & !is.na(df$affiliation) & df$affiliation == "Democrat")
  df$affil_rep <- as.integer(df$is_llm & !is.na(df$affiliation) & df$affiliation == "Republican")

  ## Cluster: LLM model for LLM rows, the legislator for human rows
  df$cluster <- ifelse(df$is_llm, paste0("llm_", df$llm_model), paste0("icpsr_", df$icpsr))

  df
}

## Fits the OLS model and attaches CR2 cluster-robust tests with Satterthwaite
## df as m$cr2 (columns beta, SE, tstat, df_Satt, p_Satt). Regressors that are
## all zero in the estimation sample (e.g. human_ind in the House) are dropped
## first, because clubSandwich cannot handle aliased coefficients.
fit_cr2 <- function(formula, data) {
  vars <- all.vars(formula)
  data <- data[complete.cases(data[, c(vars, "cluster")]), ]
  rhs  <- attr(terms(formula), "term.labels")
  empty <- rhs[sapply(rhs, function(v) all(data[[v]] == 0))]
  if (length(empty) > 0)
    formula <- update(formula, as.formula(paste(". ~ . -", paste(empty, collapse = " - "))))
  m <- lm(formula, data = data)
  m$cr2 <- as.data.frame(coef_test(m, vcov = "CR2", cluster = data$cluster,
                                   test = "Satterthwaite"))
  m$n_llm_clusters <- length(unique(data$cluster[data$is_llm]))
  m
}

print_cr2 <- function(m) {
  ct <- m$cr2
  print(data.frame(term = rownames(ct), estimate = round(ct$beta, 4),
                   SE_CR2 = round(ct$SE, 4), df_Satt = round(ct$df_Satt, 1),
                   p = signif(ct$p_Satt, 3), row.names = NULL))
  cat(sprintf("N = %d, LLM clusters = %d, R^2 = %.3f\n",
              nobs(m), m$n_llm_clusters, summary(m)$r.squared))
}

senate_reg_df <- prep_reg_df(SENATE)
house_reg_df  <- prep_reg_df(HOUSE)

reg_formula <- coord1D ~ human_dem + human_rep + human_ind +
                         company_xai + company_openai + company_meta +
                         company_google + company_deepseek +
                         local_model +
                         affil_dem + affil_rep

## Reference point for Dim 1: midpoint between Dem and Rep human medians.
## 0 is arbitrary (W-NOMINATE's origin); this is "halfway between a typical Democrat
## and a typical Republican", unaffected by party seat shares or a few outlying members.
## Treated as fixed (its uncertainty, from hundreds of human estimates, is ignored).
ref_point <- function(df) {
  hu <- df[!df$is_llm, ]                                         # humans only
  (median(hu$coord1D[hu$party_code == 100], na.rm = TRUE) +      # typical Democrat
   median(hu$coord1D[hu$party_code == 200], na.rm = TRUE)) / 2   # typical Republican
}
senate_reg_df$mu <- ref_point(senate_reg_df)
house_reg_df$mu  <- ref_point(house_reg_df)

## Dim 1 relative to mu: each coefficient = distance from the reference point,
## so p-values test "is this group at the political center?" (differences between groups unchanged)
senate_lm_1D <- fit_cr2(update(reg_formula, I(coord1D - mu) ~ 0 + .), senate_reg_df)
senate_lm_2D <- fit_cr2(update(reg_formula, coord2D ~ 0 +.), senate_reg_df)
house_lm_1D  <- fit_cr2(update(reg_formula, I(coord1D - mu) ~ 0 + .), house_reg_df)
house_lm_2D  <- fit_cr2(update(reg_formula, coord2D ~ 0 +.), house_reg_df)

cat("\n=== Senate Dim 1 (CR2, Satterthwaite df) ===\n"); print_cr2(senate_lm_1D)
cat("\n=== House Dim 1 (CR2, Satterthwaite df) ===\n");  print_cr2(house_lm_1D)
cat("\n=== Senate Dim 2 (CR2, Satterthwaite df) ===\n"); print_cr2(senate_lm_2D)
cat("\n=== House Dim 2 (CR2, Satterthwaite df) ===\n");  print_cr2(house_lm_2D)

## Produce a publication-style regression table with guaranteed decimal
## alignment.  Each numeric column is implemented as an r@{}l column PAIR:
##   r sub-column: right-aligned, holds sign + integer digits + "."
##   l sub-column: left-aligned,  holds decimal digits + stars / parenthesis
## Because the r sub-column always ends with ".", every decimal point in the
## column is at the same horizontal position regardless of sign or star count.
## No extra LaTeX packages required.
##
## Stars: * p<0.1  ** p<0.05  *** p<0.01
make_reg_table <- function(models, col_names, caption, outfile) {

  star_str <- function(p)
    ifelse(p < 0.01, "^{***}", ifelse(p < 0.05, "^{**}", ifelse(p < 0.1, "^{*}", "")))

  ## Split sprintf("%.3f", abs(x)) at the decimal point.
  split_dec <- function(x, digits = 3)
    strsplit(sprintf(paste0("%.", digits, "f"), abs(x)), "\\.")[[1]]

  ## Two cells for an estimate: "$sign int.$ & $dec stars$"
  cell_est <- function(x, p) {
    sign  <- if (x >= 0) "\\phantom{-}" else "-"
    parts <- split_dec(x)
    paste0("$", sign, parts[1], ".$ & $", parts[2], star_str(p), "$")
  }

  ## Two cells for an SE in parentheses: "$(int.$ & $dec)$"
  cell_se <- function(x) {
    parts <- split_dec(x)
    paste0("$(", parts[1], ".$ & $", parts[2], ")$")
  }

  ## Two cells for a footer statistic (R², etc.): "$sign int.$ & $dec$"
  cell_stat <- function(x, digits = 3) {
    sign  <- if (x >= 0) "\\phantom{-}" else "-"
    parts <- split_dec(x, digits)
    paste0("$", sign, parts[1], ".$ & $", parts[2], "$")
  }

  coef_list <- lapply(models, function(m) {
    s <- m$cr2
    list(est   = s$beta,
         se    = s$SE,
         p     = s$p_Satt,
         terms = rownames(s))
  })
  all_terms <- unique(unlist(lapply(coef_list, `[[`, "terms")))

  ## Coefficient body: two LaTeX rows per term (estimate row, SE row)
  body_lines <- unlist(lapply(all_terms, function(term) {
    label <- gsub("_", "\\_", term, fixed = TRUE)

    est_cells <- sapply(coef_list, function(cd) {
      i <- match(term, cd$terms)
      if (is.na(i)) return(" & ") else cell_est(cd$est[i], cd$p[i])
    })
    se_cells <- sapply(coef_list, function(cd) {
      i <- match(term, cd$terms)
      if (is.na(i)) return(" & ") else cell_se(cd$se[i])
    })

    c(paste(c(label, est_cells), collapse = " & "),
      paste(c("",    se_cells),  collapse = " & "))
  }))
  body_lines <- paste0(body_lines, " \\\\")

  ## Footer rows
  ## N has no decimal: span both sub-columns with \multicolumn{2}{c}{...}
  n_cells <- sapply(models, function(m)
    paste0("\\multicolumn{2}{c}{$", format(nobs(m), big.mark = "\\,"), "$}"))
  r2_cells  <- sapply(models, function(m) cell_stat(summary(m)$r.squared))
  ar2_cells <- sapply(models, function(m) cell_stat(summary(m)$adj.r.squared))
  cl_cells  <- sapply(models, function(m)
    paste0("\\multicolumn{2}{c}{$", m$n_llm_clusters, "$}"))

  footer_lines <- paste0(c(
    paste(c("$N$",        n_cells),   collapse = " & "),
    paste(c("LLM clusters", cl_cells), collapse = " & "),
    paste(c("$R^2$",      r2_cells),  collapse = " & "),
    paste(c("Adj. $R^2$", ar2_cells), collapse = " & ")),
    " \\\\")

  ## Column spec: l (label) + r@{}l per model — no vertical bars
  col_spec <- paste0("l", paste(rep("r@{}l", length(models)), collapse = ""))

  ## Header: model names span their r@{}l pair
  header_cells <- paste0("\\multicolumn{2}{c}{", col_names, "}")
  header_line  <- paste0(paste(c("", header_cells), collapse = " & "), " \\\\")

  tbl <- c(
    "\\begin{table}",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\hline\\hline",
    header_line,
    "\\hline",
    body_lines,
    "\\hline",
    footer_lines,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(tbl, outfile)
}

## Table 1: Dim 1 — Senate and House
make_reg_table(
  models    = list(senate_lm_1D, house_lm_1D),
  col_names = c("Senate", "House"),
  caption   = paste0("Linear models for ideal points --- Dimension 1 (combined WNOMINATE). ",
                     sprintf("Outcome is Dimension 1 minus the midpoint between the Democratic and Republican human medians (Senate %.2f, House %.2f), so coefficients are distances from that point. ",
                             senate_reg_df$mu[1], house_reg_df$mu[1]),
                     "CR2 cluster-robust standard errors (clustered by LLM model; each human is a cluster) in parentheses; tests use Satterthwaite degrees of freedom. * $p<0.1$, ** $p<0.05$, *** $p<0.01$."),
  outfile   = paste0(RESULTS_PATH, "linear_models.tex"))

## Table 2: Dim 2 — Senate and House (separate file)
make_reg_table(
  models    = list(senate_lm_2D, house_lm_2D),
  col_names = c("Senate", "House"),
  caption   = "Linear models for ideal points --- Dimension 2 (combined WNOMINATE). CR2 cluster-robust standard errors (clustered by LLM model; each human is a cluster) in parentheses; tests use Satterthwaite degrees of freedom. * $p<0.1$, ** $p<0.05$, *** $p<0.01$.",
  outfile   = paste0(RESULTS_PATH, "linear_models_dim2.tex"))

################################################################################
################################################################################
## SECTION 4: PREDICTIVE ACCURACY (CC) DENSITY PLOTS
################################################################################
################################################################################

plot_cc_density <- function(data, chamber_label) {
  humans_rep <- data$CC[!data$is_llm & data$party_code == 200]
  humans_dem <- data$CC[!data$is_llm & data$party_code == 100]
  llms_all   <- data$CC[data$is_llm]
  humans_rep <- humans_rep[is.finite(humans_rep)]
  humans_dem <- humans_dem[is.finite(humans_dem)]
  llms_all   <- llms_all[is.finite(llms_all)]

  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 30),
       yaxt = "n", ylab = "", xlab = "Classification Correct",
       bty = "n", main = chamber_label)
  polygon(density(humans_rep)$x, density(humans_rep)$y + 0.1,
          col = paste0(red,  "ff"), border = red,  angle =  45, density = 15)
  polygon(density(humans_dem)$x, density(humans_dem)$y + 0.1,
          col = paste0(blue, "ff"), border = blue, angle = 135, density = 25)
  polygon(density(llms_all)$x,   density(llms_all)$y   + 0.1,
          col = paste0(orange, "33"), border = orange)
  llm_mean <- mean(llms_all)
  segments(llm_mean, 0, llm_mean, max(density(llms_all)$y), col = orange, lwd = 2)
  text(llm_mean, max(density(llms_all)$y),
       sprintf(llm_mean, fmt = "%.2f"), pos = 3)
  legend("topleft", legend = c("Rep. Humans", "Dem. Humans", "LLMs"),
         angle = c(45, 135, NA), density = c(15, 25, NA),
         fill  = c(red, blue, paste0(orange, "33")), bty = "n")
}

pdf(paste0(RESULTS_PATH, "senate_predictive_accuracy.pdf"), width = 5, height = 1.8)
par(mfrow = c(1, 1), mar = c(2.5, 0.5, 1.5, 0.5))
plot_cc_density(SENATE, "Senate")
dev.off()

pdf(paste0(RESULTS_PATH, "house_predictive_accuracy.pdf"), width = 5, height = 1.8)
par(mfrow = c(1, 1), mar = c(2.5, 0.5, 1.5, 0.5))
plot_cc_density(HOUSE, "House")
dev.off()

## Accuracy LaTeX variables
acc_s_llm <- SENATE[SENATE$is_llm, ]
acc_h_llm <- HOUSE[HOUSE$is_llm, ]
acc_s_hu  <- SENATE[!SENATE$is_llm, ]
acc_h_hu  <- HOUSE[!HOUSE$is_llm, ]

tot_s <- with(acc_s_llm, correctYea + wrongYea + wrongNay + correctNay)
tot_h <- with(acc_h_llm, correctYea + wrongYea + wrongNay + correctNay)

SenateLlmAccuracy <- sprintf(100 * sum(acc_s_llm$correctYea + acc_s_llm$correctNay) / sum(tot_s), fmt = "%1.0f")
HouseLlmAccuracy  <- sprintf(100 * sum(acc_h_llm$correctYea + acc_h_llm$correctNay) / sum(tot_h), fmt = "%1.0f")
SenateRepAccuracy <- sprintf(100 * mean(acc_s_hu$CC[acc_s_hu$party_code == 200], na.rm = TRUE), fmt = "%1.0f")
SenateDemAccuracy <- sprintf(100 * mean(acc_s_hu$CC[acc_s_hu$party_code == 100], na.rm = TRUE), fmt = "%1.0f")
HouseRepAccuracy  <- sprintf(100 * mean(acc_h_hu$CC[acc_h_hu$party_code  == 200], na.rm = TRUE), fmt = "%1.0f")
HouseDemAccuracy  <- sprintf(100 * mean(acc_h_hu$CC[acc_h_hu$party_code  == 100], na.rm = TRUE), fmt = "%1.0f")

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(SenateLlmAccuracy), fileConn); writeLines(to_tex(SenateDemAccuracy), fileConn)
writeLines(to_tex(SenateRepAccuracy), fileConn); writeLines(to_tex(HouseLlmAccuracy),  fileConn)
writeLines(to_tex(HouseDemAccuracy),  fileConn); writeLines(to_tex(HouseRepAccuracy),  fileConn)
close(fileConn)

################################################################################
################################################################################
## SECTION 5: MODEL FIT STATISTICS (APRE, GMP)
################################################################################
################################################################################

## One joint W-NOMINATE fit per chamber, humans + LLMs + synthetic (03-model-fit.R)
avg_senate <- model_fit[model_fit$chamber == "senate", ]
avg_house  <- model_fit[model_fit$chamber == "house", ]

SenateApreA <- sprintf(avg_senate$apre1D, fmt = "%.2f")
SenateApreB <- sprintf(avg_senate$apre2D, fmt = "%.2f")
SenateGmpA  <- sprintf(avg_senate$gmp1D,  fmt = "%.2f")
SenateGmpB  <- sprintf(avg_senate$gmp2D,  fmt = "%.2f")
HouseApreA  <- sprintf(avg_house$apre1D,  fmt = "%.2f")
HouseApreB  <- sprintf(avg_house$apre2D,  fmt = "%.2f")
HouseGmpA   <- sprintf(avg_house$gmp1D,   fmt = "%.2f")
HouseGmpB   <- sprintf(avg_house$gmp2D,   fmt = "%.2f")

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(SenateApreA), fileConn); writeLines(to_tex(SenateApreB), fileConn)
writeLines(to_tex(HouseApreA),  fileConn); writeLines(to_tex(HouseApreB),  fileConn)
writeLines(to_tex(SenateGmpA),  fileConn); writeLines(to_tex(SenateGmpB),  fileConn)
writeLines(to_tex(HouseGmpA),   fileConn); writeLines(to_tex(HouseGmpB),   fileConn)
close(fileConn)

################################################################################
################################################################################
## SECTION 6: IDEAL POINT CORRELATION TABLE
################################################################################
################################################################################

writeLines(
  knitr::kable(ip_corr, format = "latex", booktabs = TRUE, digits = 3,
               caption = "Spearman rank correlation between LLM and human ideal points."),
  paste0(RESULTS_PATH, "ideal_point_correlations.tex"))

SenateSpearman1D <- sprintf(ip_corr$spearman_dim1[ip_corr$chamber == "Senate"], fmt = "%.2f")
HouseSpearman1D  <- sprintf(ip_corr$spearman_dim1[ip_corr$chamber == "House"],  fmt = "%.2f")

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(SenateSpearman1D), fileConn)
writeLines(to_tex(HouseSpearman1D),  fileConn)
close(fileConn)

################################################################################
################################################################################
## SECTION 7: AI-AI VOTE AGREEMENT CONFUSION MATRIX
##
## Each (model, affiliation-folder) is a distinct legislator.
## For each pair, compute the fraction of shared bills where both cast the same vote.
################################################################################
################################################################################

cat("Loading raw vote files...\n")
all_votes <- load_vote_files()

## Number of roll call votes per chamber and total
nRollCallSenate <- length(unique(all_votes$vote_id[substr(all_votes$vote_id, 2, 2) == "S"]))
nRollCallHouse  <- length(unique(all_votes$vote_id[substr(all_votes$vote_id, 2, 2) == "H"]))
nRollCallTotal  <- nRollCallSenate + nRollCallHouse

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(nRollCallSenate), fileConn)
writeLines(to_tex(nRollCallHouse),  fileConn)
writeLines(to_tex(nRollCallTotal),  fileConn)
close(fileConn)

## Wide matrix: one column per (model, affiliation) label, rows = vote_ids
ai_wide <- pivot_wider(
  all_votes[, c("vote_id", "label", "vote_binary")],
  names_from  = "label",
  values_from = "vote_binary")

legislator_labels <- names(ai_wide)[-1]           # drop vote_id
ai_mat            <- as.matrix(ai_wide[, -1])      # numeric matrix for fast indexing

n_leg  <- length(legislator_labels)
agree  <- matrix(NA_real_, n_leg, n_leg,
                 dimnames = list(legislator_labels, legislator_labels))

for (i in seq_len(n_leg)) {
  for (j in seq_len(n_leg)) {
    both <- !is.na(ai_mat[, i]) & !is.na(ai_mat[, j])
    if (sum(both) > 0)
      agree[i, j] <- mean(ai_mat[both, i] == ai_mat[both, j])
  }
}

## Plot: order rows/cols by first principal component of agreement matrix
ord <- order(agree[, 1], na.last = TRUE)   # simple ordering by first column
agree_ord <- agree[ord, ord]

agree_df <- as.data.frame(agree_ord) %>%
  tibble::rownames_to_column("Model1") %>%
  pivot_longer(-Model1, names_to = "Model2", values_to = "Agreement")

model_order <- rownames(agree_ord)
agree_df$Model1 <- factor(agree_df$Model1, levels = rev(model_order))
agree_df$Model2 <- factor(agree_df$Model2, levels = model_order)

p_ai_ai <- ggplot(agree_df, aes(x = Model2, y = Model1, fill = Agreement)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Agreement)), size = 1.6, na.rm = TRUE) +
  scale_fill_gradientn(colors = rev(mako(10)), limits = c(0, 1), name = "Agreement") +
  labs(x = NULL, y = NULL, title = "AI-AI Vote Agreement") +
  theme_minimal(base_size = 7) +
  theme(axis.text.x    = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y    = element_text(size = 6),
        panel.grid     = element_blank(),
        legend.position = "right")

ggsave(paste0(RESULTS_PATH, "ai-ai-confusion-matrix.pdf"),
       plot = p_ai_ai, width = 12, height = 10)

################################################################################
################################################################################
## SECTION 8: AI-HUMAN VOTE ALIGNMENT MATRICES
##
## For each (model, affiliation) pair, compute fraction of votes agreeing with
## the modal Republican or modal Democrat vote, per chamber.
## Rows = model names; columns = affiliations (Democrat / Neutral / Republican).
## One separate figure for each of {Rep, Dem} × {Senate, House}.
################################################################################
################################################################################

build_ai_human_matrices <- function(chamber) {
  chamber_code <- if (chamber == "House") "H" else "S"

  hv <- load_human_votes(chamber)
  hv <- hv[!is.na(hv$party_code), ]

  ## Modal vote per party per bill
  rep_mode <- hv %>%
    filter(party_code == 200) %>%
    group_by(congress, rollnumber) %>%
    summarise(rep_mode = modal(vote_binary), .groups = "drop")

  dem_mode <- hv %>%
    filter(party_code == 100) %>%
    group_by(congress, rollnumber) %>%
    summarise(dem_mode = modal(vote_binary), .groups = "drop")

  ## Keep only AI votes from this chamber; attach human modal votes
  ai_ch <- all_votes %>%
    filter(substr(vote_id, 2, 2) == chamber_code) %>%
    mutate(congress   = as.integer(substr(vote_id, 3, 5)),
           rollnumber = as.integer(substr(vote_id, 6, nchar(vote_id)))) %>%
    left_join(rep_mode, by = c("congress", "rollnumber")) %>%
    left_join(dem_mode, by = c("congress", "rollnumber")) %>%
    filter(!is.na(rep_mode), !is.na(dem_mode))

  models       <- sort(unique(ai_ch$model))
  affiliations <- c("Democrat", "Neutral", "Republican")
  short_models <- short_name(models)

  ## +1 row for the human baseline
  row_labels <- c(short_models, "Human")
  col_labels <- affiliations

  n_mod <- length(models)
  ai_rep <- matrix(NA_real_, n_mod + 1, 3, dimnames = list(row_labels, col_labels))
  ai_dem <- matrix(NA_real_, n_mod + 1, 3, dimnames = list(row_labels, col_labels))

  for (m_idx in seq_along(models)) {
    m   <- models[m_idx]
    lbl <- short_models[m_idx]
    for (a in affiliations) {
      sub <- ai_ch[ai_ch$model == m & ai_ch$affiliation == a, ]
      if (nrow(sub) == 0) next
      ai_rep[lbl, a] <- mean(sub$vote_binary == sub$rep_mode)
      ai_dem[lbl, a] <- mean(sub$vote_binary == sub$dem_mode)
    }
  }

  ## Human baseline: average per-member agreement with their own party's modal vote
  rep_hv <- hv %>%
    filter(party_code == 200) %>%
    left_join(rep_mode, by = c("congress", "rollnumber")) %>%
    filter(!is.na(rep_mode))

  dem_hv <- hv %>%
    filter(party_code == 100) %>%
    left_join(dem_mode, by = c("congress", "rollnumber")) %>%
    filter(!is.na(dem_mode))

  ## Fill "Neutral" column for Human (displayed once)
  ai_rep["Human", "Neutral"] <- mean(
    tapply(rep_hv$vote_binary == rep_hv$rep_mode, rep_hv$icpsr, mean, na.rm = TRUE),
    na.rm = TRUE)
  ai_dem["Human", "Neutral"] <- mean(
    tapply(dem_hv$vote_binary == dem_hv$dem_mode, dem_hv$icpsr, mean, na.rm = TRUE),
    na.rm = TRUE)

  list(rep = ai_rep, dem = ai_dem)
}

plot_ai_human_matrix <- function(mat, color_fn = rocket, title = NULL) {
  long <- as.data.frame(mat) %>%
    tibble::rownames_to_column("Model") %>%
    pivot_longer(-Model, names_to = "Affiliation", values_to = "Agreement")

  long$Affiliation <- factor(long$Affiliation,
                              levels = c("Democrat", "Neutral", "Republican"))
  long$Model <- factor(long$Model, levels = rev(rownames(mat)))

  ggplot(long, aes(x = Affiliation, y = Model, fill = Agreement)) +
    geom_tile(color = "white", na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(Agreement), "",
                                 sprintf("%.2f", Agreement))),
              size = 2.5, na.rm = TRUE) +
    scale_fill_gradientn(colors    = rev(color_fn(10)),
                         limits    = c(0, 1),
                         na.value  = "#eeeeee",
                         name      = "Agreement") +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x   = element_text(angle = 0, hjust = 0.5),
          panel.grid    = element_blank(),
          legend.position = "right")
}

cat("Building Senate AI-Human vote alignment matrices...\n")
senate_mat <- build_ai_human_matrices("Senate")
cat("Building House AI-Human vote alignment matrices...\n")
house_mat  <- build_ai_human_matrices("House")

ggsave(paste0(RESULTS_PATH, "ai_rep_senate.pdf"),
       plot_ai_human_matrix(senate_mat$rep, rocket, "AI vs. Republican Modal Vote (Senate)"),
       width = 5, height = 5)
ggsave(paste0(RESULTS_PATH, "ai_dem_senate.pdf"),
       plot_ai_human_matrix(senate_mat$dem, mako,   "AI vs. Democrat Modal Vote (Senate)"),
       width = 5, height = 5)
ggsave(paste0(RESULTS_PATH, "ai_rep_house.pdf"),
       plot_ai_human_matrix(house_mat$rep, rocket,  "AI vs. Republican Modal Vote (House)"),
       width = 5, height = 5)
ggsave(paste0(RESULTS_PATH, "ai_dem_house.pdf"),
       plot_ai_human_matrix(house_mat$dem, mako,    "AI vs. Democrat Modal Vote (House)"),
       width = 5, height = 5)

################################################################################
################################################################################
## SECTION 9: HOW OFTEN DO AIs VOTE WITH THE HUMAN MAJORITY?
## Uses Neutral-affiliation votes (closest analogue to the original "neu" prompt).
################################################################################
################################################################################

cat("Computing vote-with-majority statistics...\n")

for (chamber in c("Senate", "House")) {
  chamber_code <- if (chamber == "House") "H" else "S"

  hv       <- load_human_votes(chamber)
  maj_mode <- hv %>%
    group_by(congress, rollnumber) %>%
    summarise(majority = ifelse(mean(vote_binary) == 0.5, NA,   # tie (e.g. 50-50 Senate): no majority, dropped below
                                modal(vote_binary)), .groups = "drop")

  ai_neu <- all_votes %>%
    filter(affiliation == "Neutral",
           substr(vote_id, 2, 2) == chamber_code) %>%
    mutate(congress   = as.integer(substr(vote_id, 3, 5)),
           rollnumber = as.integer(substr(vote_id, 6, nchar(vote_id)))) %>%
    left_join(maj_mode, by = c("congress", "rollnumber")) %>%
    filter(!is.na(majority))

  props <- ai_neu %>%
    group_by(model) %>%
    summarise(prop_with_maj = mean(vote_binary == majority),
              n_votes       = n(),
              .groups       = "drop") %>%
    mutate(display = short_name(model))

  min_row <- props[which.min(props$prop_with_maj), ]
  max_row <- props[which.max(props$prop_with_maj), ]

  min_name <- min_row$display
  max_name <- max_row$display
  min_val  <- sprintf(min_row$prop_with_maj * 100, fmt = "%1.0f")
  max_val  <- sprintf(max_row$prop_with_maj * 100, fmt = "%1.0f")

  if (chamber == "House") {
    HouseVoteWithMajMin     <- min_val;   HouseVoteWithMajMinName <- min_name
    HouseVoteWithMajMax     <- max_val;   HouseVoteWithMajMaxName <- max_name
  } else {
    SenateVoteWithMajMin    <- min_val;   SenateVoteWithMajMinName <- min_name
    SenateVoteWithMajMax    <- max_val;   SenateVoteWithMajMaxName <- max_name
  }

  props$display <- factor(props$display,
                           levels = props$display[order(props$prop_with_maj)])

  p_maj <- ggplot(props, aes(x = display, y = prop_with_maj * 100)) +
    geom_col(fill = orange, width = 0.7) +
    geom_hline(yintercept = 50, linetype = "dashed",
               color = gray, linewidth = 0.5) +
    coord_flip() +
    labs(x = NULL, y = "% votes with human majority",
         title = paste(chamber, "- AI Agreement with Human Majority Vote (Neutral prompt)")) +
    theme_minimal(base_size = 9)

  ggsave(paste0(RESULTS_PATH, tolower(chamber), "-vote-with-majority.pdf"),
         plot = p_maj, width = 6, height = 4)
}

fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
writeLines(to_tex(HouseVoteWithMajMin),      fileConn)
writeLines(to_tex(HouseVoteWithMajMinName),  fileConn)
writeLines(to_tex(HouseVoteWithMajMax),      fileConn)
writeLines(to_tex(HouseVoteWithMajMaxName),  fileConn)
writeLines(to_tex(SenateVoteWithMajMin),     fileConn)
writeLines(to_tex(SenateVoteWithMajMinName), fileConn)
writeLines(to_tex(SenateVoteWithMajMax),     fileConn)
writeLines(to_tex(SenateVoteWithMajMaxName), fileConn)
close(fileConn)

################################################################################
################################################################################
## SECTION 10: ABSTENTION RATES PER AI MODEL
##
## Abstention = model did not return "yea" or "nay" for a given roll call.
## Rates computed per model (pooled across affiliations and congresses),
## separately for each chamber, then averaged across Senate and House.
################################################################################
################################################################################

load_vote_files_all <- function() {
  affiliations <- c("Democrat", "Neutral", "Republican")

  rows <- lapply(affiliations, function(affil) {
    dir      <- paste0(VOTES_PATH, affil, "/")
    csvfiles <- list.files(dir, pattern = "^votes-.*\\.csv$", full.names = TRUE)

    lapply(csvfiles, function(f) {
      model_name <- sub("^votes-", "", sub("\\.csv$", "", basename(f)))
      d <- read.csv(f, stringsAsFactors = FALSE)

      if (ncol(d) == 5) {
        d <- d[, c("vote_id", "vote")]
      } else {
        d <- d[, c(1L, 4L)]
        names(d) <- c("vote_id", "vote")
      }

      d <- d[!duplicated(d$vote_id), ]          # one row per roll call
      d$valid       <- d$vote %in% c("yea", "nay")
      d$model       <- model_name
      d$affiliation <- affil
      d$chamber     <- ifelse(substr(d$vote_id, 2L, 2L) == "H", "House", "Senate")
      d
    })
  })

  do.call(rbind, unlist(rows, recursive = FALSE))
}

## Convert a short model name to a valid LaTeX command suffix (letters only)
tex_safe_name <- function(nm) {
  nm <- gsub("-", "", nm, fixed = TRUE)
  nm <- gsub(".", "", nm, fixed = TRUE)
  nm <- gsub("0", "Zero",  nm)
  nm <- gsub("1", "One",   nm)
  nm <- gsub("2", "Two",   nm)
  nm <- gsub("3", "Three", nm)
  nm <- gsub("4", "Four",  nm)
  nm <- gsub("5", "Five",  nm)
  nm <- gsub("6", "Six",   nm)
  nm <- gsub("7", "Seven", nm)
  nm <- gsub("8", "Eight", nm)
  nm <- gsub("9", "Nine",  nm)
  nm
}

cat("Computing abstention rates per AI model...\n")
all_votes_full <- load_vote_files_all()

## Per-model abstention rate by chamber (pooled across affiliations)
abstention_by_chamber <- all_votes_full %>%
  group_by(model, chamber) %>%
  summarise(
    n_total        = n(),
    n_valid        = sum(valid),
    abstention_pct = 100 * (1 - n_valid / n_total),
    .groups        = "drop"
  )

## Average abstention rate across the two chambers
abstention_by_model <- abstention_by_chamber %>%
  group_by(model) %>%
  summarise(
    abstention_avg_pct = 100 * (1 - (sum(n_valid) / sum(n_total))),#mean(abstention_pct),
    .groups            = "drop"
  ) %>%
  mutate(
    short    = short_name(model),
    tex_sfx  = tex_safe_name(short)
  ) %>%
  arrange(short)

## Share of all LLM votes (every model, prompt and roll call) that are not yea/nay
AbstentionMean <- sprintf("%.2f",
                          100*mean(!all_votes_full$valid[all_votes_full$model != "random"]))
  #sprintf("%.1f",
  #mean(abstention_by_model$abstention_avg_pct[abstention_by_model$short != "Random"]))

## Write one \newcommand per model: \AbstentionXxx{value}, plus the mean
fileConn <- file(paste0(RESULTS_PATH, "variables.tex"), "a")
for (i in seq_len(nrow(abstention_by_model))) {
  row     <- abstention_by_model[i, ]
  varname <- paste0("Abstention", row$tex_sfx)
  val     <- sprintf("%.1f", row$abstention_avg_pct)
  writeLines(paste0("\\newcommand{\\", varname, "}{", val, "}"), fileConn)
}
writeLines(to_tex(AbstentionMean), fileConn)
close(fileConn)

cat("Abstention rates written to variables.tex\n")
print(abstention_by_model[, c("short", "abstention_avg_pct")])

################################################################################
#                      PLOT coord1D FOR LLMS AND HUMANS                        #
################################################################################

## Row layout per chamber:
##   [Democrat LLMs sorted by name]
##   [Human Democrat boxplot — separator]
##   [Neutral LLMs sorted by name]
##   [Human Republican boxplot — separator]
##   [Republican LLMs sorted by name]
##   [Synthetic voters sorted by name]
build_coord1D_df <- function(data) {
  affils    <- c("Democrat", "Neutral", "Republican", "Synthetic")
  sep_hu_pc <- c("Democrat" = 100L, "Neutral" = 200L)  # human party inserted after each non-last group

  chunks <- list()
  for (i in seq_along(affils)) {
    a       <- affils[i]
    llm_sub <- data[data$is_llm & !is.na(data$affiliation) & data$affiliation == a, ]
    se1d    <- if ("se1D" %in% names(llm_sub)) llm_sub$se1D else rep(NA_real_, nrow(llm_sub))
    chunk   <- data.frame(
      family      = short_name(llm_sub$llm_model),
      affiliation = a,
      coord1D     = llm_sub$coord1D,
      se1D        = se1d,
      row_type    = "llm",
      hu_pc       = NA_integer_,
      stringsAsFactors = FALSE
    )
    chunks <- c(chunks, list(chunk[order(chunk$family), ]))

    if (a %in% names(sep_hu_pc)) {                         # human separator only after Democrat / Neutral
      chunks <- c(chunks, list(data.frame(
        family = "", affiliation = a, coord1D = NA_real_, se1D = NA_real_,
        row_type = "human", hu_pc = sep_hu_pc[[a]], stringsAsFactors = FALSE
      )))
    }
  }

  df   <- do.call(rbind, chunks)
  df$y <- seq_len(nrow(df))
  df
}

plot_coord1D_panel <- function(data, df, n_rows, panel_title) {
  affil_col  <- c("Democrat" = blue,  "Neutral" = orange,  "Republican" = red,  "Synthetic" = "gray30")
  affil_fill <- c("Democrat" = lblue, "Neutral" = lorange, "Republican" = lred, "Synthetic" = "gray80")
  affil_pch  <- c("Democrat" = 23,    "Neutral" = 25,     "Republican" = 21,   "Synthetic" = 4)  # 4 = cross, as in nominate plot
  hu_data    <- data[!data$is_llm, ]

  ## Both panels share identical margins so plot regions are the same width.
  par(mar = c(4.1, 0.5, 1.5, 0.5))
  plot(0, 0, type = "n", frame = FALSE, main = panel_title,
       xlim = c(-1.1, 1.1), ylim = c(n_rows + 0.5, 0.5), xaxt="n",
       xlab = "Estimated Ideal Points", yaxt = "n", xaxs = "i", yaxs = "i")
  
  u <- par("usr") # Get plotting area coordinates
  rect(-1, u[3], 1, u[4], col = "#EEEEEE", border = NA)
  
  axis(1, at=c(-1,-.5,0,.5,1))
  
  abline(v = 0, lty = 1)
  mid <- ref_point(data)                 # midpoint of Dem/Rep human medians (same reference point as the Dim 1 models)
  abline(v = mid, lty = 2)

  abline(v=c(1,0.5,-0.5,-1), lwd=0.5, col="gray")
  
  ## LLM confidence intervals and points
  ai_llm   <- df[df$row_type == "llm" & !is.na(df$coord1D), ]
  seg_col  <- affil_col[ai_llm$affiliation]
  seg_fill <- affil_fill[ai_llm$affiliation]
  lwr      <- ai_llm$coord1D - ai_llm$se1D * 1.96
  upr      <- ai_llm$coord1D + ai_llm$se1D * 1.96
  lwr90    <- ai_llm$coord1D - ai_llm$se1D * 1.645
  upr90    <- ai_llm$coord1D + ai_llm$se1D * 1.645

  # segments(lwr,   ai_llm$y,         upr,   ai_llm$y,         col = seg_col, lwd = 2)
  segments(lwr90, ai_llm$y,         upr90, ai_llm$y,         col = seg_col, 
           lwd = ifelse((upr90>mid) & (lwr90<mid), 2, 2), lend=1)
  segments(lwr90, ai_llm$y - 0.25,  lwr90, ai_llm$y + 0.25,  col = seg_col, lwd = 2)
  segments(upr90, ai_llm$y - 0.25,  upr90, ai_llm$y + 0.25,  col = seg_col, lwd = 2)
  # segments(lwr,   ai_llm$y - 0.25,  lwr,   ai_llm$y + 0.25,  col = seg_col, lwd = 1)
  # segments(upr,   ai_llm$y - 0.25,  upr,   ai_llm$y + 0.25,  col = seg_col, lwd = 1)
  ## Black fill = 90% interval excludes the midpoint (dashed line); affiliation fill = includes it
  points(ai_llm$coord1D, ai_llm$y,
         bg = ifelse((upr90>mid) & (lwr90<mid), seg_fill, "black"), pch = affil_pch[ai_llm$affiliation], col = "black", cex = 1)

  ## Label left of bar when coord1D > 0; right of bar when coord1D < 0.
  is_pos <- ai_llm$coord1D > 0
  text(ifelse(is_pos, 
              sapply(lwr,FUN=function(x)min(x,0)), 
              sapply(upr,FUN=function(x)max(0,x))), ai_llm$y, ai_llm$family,
       pos = ifelse(is_pos, 2, 4), cex = 0.85)

  ## Human separator boxplots (between affiliation groups)
  ai_sep        <- df[df$row_type == "human", ]
  hu_sep_labels <- c("100" = "Human (Dem)", "200" = "Human (Rep)")
  for (k in seq_len(nrow(ai_sep))) {
    pc    <- ai_sep$hu_pc[k]
    row_y <- ai_sep$y[k]
    vals  <- na.omit(hu_data$coord1D[hu_data$party_code == pc])
    if (length(vals) == 0) next
    boxplot(vals, xaxt = "n", yaxt = "n", frame = FALSE, horizontal = TRUE,
            add = TRUE, col = if (pc == 100L) lblue else lred, at = row_y, width = 1.5)
    x_anch <- if (pc == 200L) min(vals,0) else max(vals,0)
    text(x_anch, row_y + 0.2, hu_sep_labels[as.character(pc)],
         pos = if (pc == 200L) 2 else 4, cex = 0.85, font = 2)
  }
}

senate_df  <- build_coord1D_df(rbind(SENATE, SYN_SENATE))  # synthetic voters go in the last group
house_df   <- build_coord1D_df(rbind(HOUSE,  SYN_HOUSE))
n_rows     <- max(nrow(senate_df), nrow(house_df))
grp_labels <- c("Democrat" = "Democrat prompt", "Neutral" = "No Affiliation", "Republican" = "Republican prompt")

pdf(paste0(RESULTS_PATH, "coord1D.pdf"), width = 6, height = 7)

## layout(): narrow label strip + two equal-width data panels.
## Matching top/bottom margins across all three panels keeps y-coordinates aligned.
layout(matrix(1:3, 1, 3), widths = c(1.2, 6, 6))

## Panel 1: shared affiliation labels (same y-scale as data panels, no data drawn)
par(mar = c(4.1, 0, 1.5, 0))

plot(0, 0, type = "n", frame = FALSE, ann = FALSE,
     xlim = c(0, 1), ylim = c(n_rows + 0.5, 0.5),
     xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
text(0.5, 5, "Democrat prompt",  cex = 1, srt=90)
text(0.5, 18.5, "No affiliation",  cex = 1, srt=90)
text(0.5, 32, "Republican prompt",  cex = 1, srt=90)
brackets(1, 1, 1, 9, h=0.15)
brackets(1, 11, 1, 26, h=0.15)
brackets(1, 28, 1, 36, h=0.15)
text(0.5, 38, "Synthetic",  cex = 1, srt=90)              # rows 37-39: random / majority / minority
brackets(1, 37, 1, 39, h=0.15)

# text(0.5, 1:36, 1:36, srt=90)
# for (a in names(grp_labels)) {
#   rows_a <- senate_df$y[senate_df$affiliation == a & senate_df$row_type == "llm"]
#   if (length(rows_a) > 0)
#     text(0.5, mean(range(rows_a)), grp_labels[a], adj = c(1, 0.5), cex = 0.85, srt=90)
# }

## Panels 2 & 3: Senate and House (identical margins → identical plot-region widths)
plot_coord1D_panel(SENATE, senate_df, n_rows, "Senate")
plot_coord1D_panel(HOUSE,  house_df,  n_rows, "House")
dev.off()

################################################################################