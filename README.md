# RKaggle <a href='https://github.com/benyamindsmith/fastgam'><img src='https://github.com/benyamindsmith/RKaggle/raw/main/utils/png/hex_sticker.png' align="right" height="300" /></a>


An R package for downloading datasets from Kaggle into the R Console as dataframes

## Installation

```r
# install.packages('devtools')

devtools::install_github("benyamindsmith/RKaggle")

```

## Sample Usage

```r
# Download and read the "canadian-prime-ministers" dataset from Kaggle
canadian_prime_ministers <- get_dataset("benjaminsmith/canadian-prime-ministers")
canadian_prime_ministers
```
## Present Scope

`RKaggle` presently only supports the download and loading of csv files.
