# Replication Archive for: Sixteen generative AI systems show no left-leaning bias in legislative voting

* Hanif Sajid
* Benjamin J. Radford
* Yaoyao Dai
* Jason Windett

## Requirements

The analysis in the manuscript was conducted on Ubuntu 24.04.4 LTS with R version 4.3.3 (2024-02-29). You will need R and the following packages:

- wnominate 1.5
- pscl 1.5.9
- dplyr 1.2.1
- tidyr 1.3.2
- tibble 3.3.1
- stringr 1.6.0
- ggplot2 4.0.2
- ggrepel 0.9.8
- patchwork 1.3.2
- viridisLite 0.4.3
- pBrackets 1.0.1
- e1071 1.7.17
- clubSandwich 0.7.0
- knitr 1.51
- parallel (base R)
- tools (base R)

These can be installed with the following command:

```r
install.packages("remotes")

pkgs <- c(
  wnominate    = "1.5",    pscl        = "1.5.9",  dplyr     = "1.2.1",  tidyr        = "1.3.2",
  tibble       = "3.3.1",  stringr     = "1.6.0",  ggplot2   = "4.0.2",  ggrepel      = "0.9.8",
  patchwork    = "1.3.2",  viridisLite = "0.4.3",  pBrackets = "1.0.1",  e1071        = "1.7.17",
  clubSandwich = "0.7.0",  knitr       = "1.51"
)

for (p in names(pkgs)) {
  remotes::install_version(p, version = pkgs[[p]], repos = "https://cloud.r-project.org", upgrade = "never")
}
```

## Data

The necessary data can be obtained from [Harvard Dataverse](https://doi.org/10.7910/DVN/PBK4NI). Download the zip file and unzip its contents into the `data/` folder in the root of this repository.


## Running the Replication

The entire replication can be performed from the repository's root directory with the following command:

```
bash run-all.sh
```

Expect the replication to take several hours, even on a relatively powerful desktop workstation.

## Questions or Concerns

If you notice any errors or encounter difficulties when using this replication archive, please contact the corresponding author or open an issue in this repository.

## Citation

```
Coming soon...
```


