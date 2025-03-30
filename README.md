# RKaggle <a href='https://github.com/benyamindsmith/RKaggle'><img src='https://github.com/benyamindsmith/RKaggle/raw/main/utils/png/hex_sticker.png' align="right" height="300" /></a>


An R package for downloading datasets from Kaggle into the R Console as dataframes

## Installation

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

## Acknowledgements

The [TidyTuesday project](https://github.com/rfordatascience/tidytuesday) from where I based the code for this package on initially. 
