``` r
remotes::install_github("hrbrmstr/statebins")
#> Skipping install of 'statebins' from a github remote, the SHA1 (cad276fe) has not changed since last install.
#>   Use `force = TRUE` to force installation

library(tidyverse)
library(statebins)
library(viridis)
#> Loading required package: viridisLite


adat <- read_csv(system.file("extdata", "wapostates.csv", package="statebins"))
#> Parsed with column specification:
#> cols(
#>   .default = col_double(),
#>   fipst = col_character(),
#>   stab = col_character(),
#>   state = col_character()
#> )
#> See spec(...) for full column specifications.
election_2012 <- suppressMessages(read_csv(system.file("extdata", "election2012.csv", package="statebins")))


state_bins_df <- tuition_cost %>%
  select(state, state_code, degree_length, in_state_total, out_of_state_total)%>%
  mutate(difference_in_cost =  out_of_state_total - in_state_total) %>%
  group_by(state, state, degree_length) %>%
  summarise(median_difference_in_cost =  median(difference_in_cost, na.rm = T), 
            mean_difference_in_cost = mean(difference_in_cost, na.rm = T))
#> Error in eval(lhs, parent, parent): object 'tuition_cost' not found
  
bins_2_year <- state_bins_df %>%
  na.omit()%>%
  filter(degree_length == "2 Year") 
#> Error in eval(lhs, parent, parent): object 'state_bins_df' not found


statebins(bins_2_year, state_col = "state", value_col = "median_difference_in_cost",round=TRUE)+
  theme_statebins()+
  scale_fill_viridis(option = "viridis", name = "Difference in Cost")+
  labs(title = "Difference of In Stat+e Tuition and Out of State Tuition")+
  theme( legend.key.width = unit(2.5, "cm"),
        plot.title = element_text(hjust = .5), 
        plot.subtitle = element_text(hjust = .5))
#> Error in data.frame(state_data, stringsAsFactors = FALSE): object 'bins_2_year' not found
```

<sup>Created on 2020-03-10 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>

<details>

<summary>Session info</summary>

``` r
devtools::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.6.1 (2019-07-05)
#>  os       Windows 10 x64              
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  ctype    English_United States.1252  
#>  tz       America/Chicago             
#>  date     2020-03-10                  
#> 
#> - Packages -------------------------------------------------------------------
#>  package     * version date       lib source                             
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.1)                     
#>  backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.1)                     
#>  broom         0.5.2   2019-04-07 [1] CRAN (R 3.6.1)                     
#>  callr         3.4.1   2020-01-24 [1] CRAN (R 3.6.2)                     
#>  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.1)                     
#>  cli           2.0.1   2020-01-08 [1] CRAN (R 3.6.2)                     
#>  colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.1)                     
#>  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.1)                     
#>  curl          4.3     2019-12-02 [1] CRAN (R 3.6.1)                     
#>  DBI           1.0.0   2018-05-02 [1] CRAN (R 3.6.1)                     
#>  dbplyr        1.4.2   2019-06-17 [1] CRAN (R 3.6.1)                     
#>  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.1)                     
#>  devtools      2.2.1   2019-09-24 [1] CRAN (R 3.6.1)                     
#>  digest        0.6.23  2019-11-23 [1] CRAN (R 3.6.1)                     
#>  dplyr       * 0.8.3   2019-07-04 [1] CRAN (R 3.6.1)                     
#>  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.1)                     
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.1)                     
#>  fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.2)                     
#>  forcats     * 0.4.0   2019-02-17 [1] CRAN (R 3.6.1)                     
#>  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.1)                     
#>  generics      0.0.2   2018-11-29 [1] CRAN (R 3.6.1)                     
#>  ggplot2     * 3.3.0   2020-03-05 [1] CRAN (R 3.6.1)                     
#>  glue          1.3.1   2019-03-12 [1] CRAN (R 3.6.1)                     
#>  gridExtra     2.3     2017-09-09 [1] CRAN (R 3.6.1)                     
#>  gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.1)                     
#>  haven         2.2.0   2019-11-08 [1] CRAN (R 3.6.1)                     
#>  highr         0.8     2019-03-20 [1] CRAN (R 3.6.1)                     
#>  hms           0.5.2   2019-10-30 [1] CRAN (R 3.6.1)                     
#>  htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.6.1)                     
#>  httr          1.4.1   2019-08-05 [1] CRAN (R 3.6.1)                     
#>  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.6.1)                     
#>  knitr         1.26    2019-11-12 [1] CRAN (R 3.6.1)                     
#>  lattice       0.20-38 2018-11-04 [2] CRAN (R 3.6.1)                     
#>  lifecycle     0.1.0   2019-08-01 [1] CRAN (R 3.6.1)                     
#>  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.6.1)                     
#>  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.1)                     
#>  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.1)                     
#>  modelr        0.1.5   2019-08-08 [1] CRAN (R 3.6.1)                     
#>  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.1)                     
#>  nlme          3.1-140 2019-05-12 [2] CRAN (R 3.6.1)                     
#>  pillar        1.4.3   2019-12-20 [1] CRAN (R 3.6.2)                     
#>  pkgbuild      1.0.6   2019-10-09 [1] CRAN (R 3.6.1)                     
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.1)                     
#>  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.1)                     
#>  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.2)                     
#>  processx      3.4.1   2019-07-18 [1] CRAN (R 3.6.1)                     
#>  ps            1.3.0   2018-12-21 [1] CRAN (R 3.6.1)                     
#>  purrr       * 0.3.3   2019-10-18 [1] CRAN (R 3.6.1)                     
#>  R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.1)                     
#>  Rcpp          1.0.3   2019-11-08 [1] CRAN (R 3.6.1)                     
#>  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.6.1)                     
#>  readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.1)                     
#>  remotes       2.1.0   2019-06-24 [1] CRAN (R 3.6.1)                     
#>  reprex        0.3.0   2019-05-16 [1] CRAN (R 3.6.3)                     
#>  rlang         0.4.4   2020-01-28 [1] CRAN (R 3.6.2)                     
#>  rmarkdown     1.18    2019-11-27 [1] CRAN (R 3.6.1)                     
#>  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.1)                     
#>  rvest         0.3.5   2019-11-08 [1] CRAN (R 3.6.1)                     
#>  scales        1.1.0   2019-11-18 [1] CRAN (R 3.6.2)                     
#>  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.1)                     
#>  statebins   * 2.0.0   2020-03-10 [1] Github (hrbrmstr/statebins@cad276f)
#>  stringi       1.4.3   2019-03-12 [1] CRAN (R 3.6.0)                     
#>  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.1)                     
#>  testthat      2.3.1   2019-12-01 [1] CRAN (R 3.6.1)                     
#>  tibble      * 2.1.3   2019-06-06 [1] CRAN (R 3.6.1)                     
#>  tidyr       * 1.0.0   2019-09-11 [1] CRAN (R 3.6.1)                     
#>  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.6.1)                     
#>  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 3.6.1)                     
#>  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.1)                     
#>  vctrs         0.2.2   2020-01-24 [1] CRAN (R 3.6.2)                     
#>  viridis     * 0.5.1   2018-03-29 [1] CRAN (R 3.6.1)                     
#>  viridisLite * 0.3.0   2018-02-01 [1] CRAN (R 3.6.1)                     
#>  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.1)                     
#>  xfun          0.11    2019-11-12 [1] CRAN (R 3.6.1)                     
#>  xml2          1.2.2   2019-08-09 [1] CRAN (R 3.6.1)                     
#>  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)                     
#> 
#> [1] C:/Users/delabruerejosh/Documents/R/win-library/3.6
#> [2] C:/Program Files/R/R-3.6.1/library
```

</details>
