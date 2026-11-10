# RKaggle <a href='https://github.com/benyamindsmith/RKaggle'><img src='https://github.com/benyamindsmith/RKaggle/raw/main/utils/png/hex_sticker.png' align="right" height="300" /></a>

[![](https://www.r-pkg.org/badges/version/RKaggle?color=green)](https://cran.r-project.org/package=RKaggle) 
[![downloads](https://cranlogs.r-pkg.org/badges/RKaggle)](https://shinyus.ipub.com/cranview/)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/RKaggle)](https://shinyus.ipub.com/cranview/)

An R package for downloading datasets from Kaggle into the R Console.

## Installation

To install the latest version on CRAN, run: 

```r
install.packages("RKaggle")
```

To install the development version, run: 

```r
# install.packages('devtools')

devtools::install_github("benyamindsmith/RKaggle")

```


## Sample Usage

```r
>library(RKaggle)
># Download and read the "canadian-prime-ministers" dataset from Kaggle
>canadian_prime_ministers <- RKaggle::get_dataset("benjaminsmith/canadian-prime-ministers")
>canadian_prime_ministers
# A tibble: 29 × 5
   No.        Name                `Political Party`     `Term Start`     `Term End`      
   <chr>      <chr>               <chr>                 <chr>            <chr>           
 1 1 (1 of 2) John A. Macdonald   Liberal-Conservative  1 July 1867      5 November 1873 
 2 2          Alexander Mackenzie Liberal               7 November 1873  8 October 1878  
 3 1 (2 of 2) John A. Macdonald   Liberal-Conservative  17 October 1878  6 June 1891     
 4 3          John Abbott         Liberal-Conservative  16 June 1891     24 November 1891
 5 4          John Thompson       Liberal-Conservative  5 December 1892  12 December 1894
 6 5          Mackenzie Bowell    Conservative          21 December 1894 27 April 1896   
 7 6          Charles Tupper      Conservative          1 May 1896       8 July 1896     
 8 7          Wilfrid Laurier     Liberal               11 July 1896     10/6/1911       
 9 8          Robert Borden       Government (Unionist) 10/10/1911       7/10/1920       
10 9 (1 of 2) Arthur Meighen      Conservative          7/10/1920        12/29/1921      
# ℹ 19 more rows
# ℹ Use `print(n = ...)` to see more rows
```
## Present Scope

`RKaggle` presently only supports the download and loading of the following file formats: 

- `.csv`
- `.tsv`
- `.xlsx`
- `.json`
- `.rds`
- `.parquet`
- `.ods`
- `.shp`
- `.geojson`
- `.feather`

## Blogs

1. [RObservations #51: Download Kaggle Datasets into the R Console with {RKaggle}](https://bensstats.wordpress.com/2025/03/30/robservations-51-download-kaggle-datasets-into-the-r-console-with-rkaggle/)


## References

1. Akhtar et al. (2024). Croissant: A Metadata Format for ML-Ready Datasets. Proceedings of the Eighth Workshop on Data Management for End-To-End Machine Learning, 4, 1–6. https://doi.org/10.1145/3650203.3663326

## Similar Works
- [`kaggler`](https://github.com/mkearney/kaggler)
 
## Acknowledgements

The [TidyTuesday project](https://github.com/rfordatascience/tidytuesday) from where I based the code for this package on initially. 
