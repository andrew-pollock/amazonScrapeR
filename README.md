# amazonScrapeR

## Overview

An R package with a basic Amazon review scraping function. Allows the user to scrape all Amazon product reviews from the UK and/or US sites for a specified product ID.

## Installation
The dev version of the package can be installed from GitHub using `devtools`:

``` r
devtools::install_github("andrew-pollock/amazonScrapeR")

```

## Example Usage

``` r
library(amazonScrapeR)

product_reviews <- amazonScrape("0553296124", country = "UK")
head(product_reviews, 2) 

#>  Date        Title                            Review_Text                               Review_Score Country
#> 1 2017-01-09 a well-written Star Wars novel   This was the first novel of the Star War~            4 UK     
#> 2 2016-03-11 This trilogy is a must read for~ Timothy Zahn is a great writer and story~            5 UK  

```
