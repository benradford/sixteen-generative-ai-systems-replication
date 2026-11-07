#!/usr/bin/env bash
# Recomputes every analysis from the vote files in data/.
# Usage: bash run-all.sh
set -euo pipefail
cd "$(dirname "$0")"

## Main analysis (must run first: appendix-alt-prompt reads its 01 and 02 output)
cd replication-fixed/code
Rscript 01-fit-humans.R
Rscript 02-place-llms.R
Rscript 03-model-fit.R
Rscript 04-results.R
Rscript 05-procrustes-diagnostics.R
cd ../..

## Appendix: alternative prompt
cd appendix-alt-prompt/code
Rscript 01-alt-prompt-robustness.R
cd ../..

## Appendix: old models
cd appendix-old-fixed/code
Rscript 01-fit-humans.R
Rscript 02-place-llms.R
Rscript 03-model-fit.R
Rscript 04-results.R
Rscript 05-procrustes-diagnostics.R
cd ../..

## Appendix: one joint W-NOMINATE fit
cd appendix-single-model-fixed/code
Rscript 01-fit-all.R
Rscript 03-model-fit.R
Rscript 04-results.R
Rscript 05-procrustes-diagnostics.R
cd ../..
