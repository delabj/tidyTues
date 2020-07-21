Australian Pets
================
Joshua de la Bruere
7/21/2020

## Fetching the data/setup

As always I begin by fetching the data from the tidytuesday repo. \#\#\#
Packages: These are the package I plan on using today:

  - tidyverse (It’s what this is all about)
  - ggtext (A wonderful package by
    \[@ClausWilke\](<https://twitter.com/ClausWilke>) that enables
    better control over text rendering)
  - patchwork (By the amazing
    \[@thomasp85\](<https://twitter.com/thomasp85>) allows for easy
    combining of plots)
  - janitor (makes it easy to clean the names of a data set.)
  - forcats (easy work with factors)
  - delabj (A personal package that includes a few tweaks to ggplot, and
    custom themes on github use
    devtools::install\_github(“delabj/delabj”) to install)

I will start by doing some basic EDA to figure out if there’s any
cleaning that needs to be done or not.

``` r
animal_complaints %T>%
  glimpse() %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()
```

    ## Rows: 42,413
    ## Columns: 5
    ## $ `Animal Type`        <chr> "dog", "dog", "dog", "dog", "dog", "dog", "dog...
    ## $ `Complaint Type`     <chr> "Aggressive Animal", "Noise", "Noise", "Privat...
    ## $ `Date Received`      <chr> "June 2020", "June 2020", "June 2020", "June 2...
    ## $ Suburb               <chr> "Alice River", "Alice River", "Alice River", "...
    ## $ `Electoral Division` <chr> "Division 1", "Division 1", "Division 1", "Div...

    ##  Animal Type           Complaint Type      Date Received           Suburb     
    ##  cat: 4094   Aggressive Animal: 4459   August 2016:  761   Unallocated: 4934  
    ##  dog:38319   Attack           : 3836   May 2017   :  719   Kirwan     : 3713  
    ##              Enclosure        : 6282   June 2017  :  711   Kelso      : 3296  
    ##              Noise            : 9205   July 2017  :  707   Aitkenvale : 2147  
    ##              Private Impound  :12692   July 2016  :  700   Douglas    : 1837  
    ##              Wandering        : 5939   June 2016  :  688   Condon     : 1614  
    ##                                        (Other)    :38127   (Other)    :24872  
    ##    Electoral Division
    ##  Division 4 : 5610   
    ##  Unallocated: 5568   
    ##  Division 8 : 5236   
    ##  Division 2 : 4314   
    ##  Division 7 : 3563   
    ##  Division 9 : 3380   
    ##  (Other)    :14742

``` r
animal_outcomes %T>%
  glimpse() %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()
```

    ## Rows: 664
    ## Columns: 12
    ## $ year        <dbl> 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1...
    ## $ animal_type <chr> "Dogs", "Dogs", "Dogs", "Dogs", "Cats", "Cats", "Cats",...
    ## $ outcome     <chr> "Reclaimed", "Rehomed", "Other", "Euthanized", "Reclaim...
    ## $ ACT         <dbl> 610, 1245, 12, 360, 111, 1442, 0, 1007, 0, 1, 0, 0, 2, ...
    ## $ NSW         <dbl> 3140, 7525, 745, 9221, 201, 3913, 447, 8205, 0, 12, 0, ...
    ## $ NT          <dbl> 205, 526, 955, 9, 22, 269, 0, 847, 1, 3, 0, 0, 0, 120, ...
    ## $ QLD         <dbl> 1392, 5489, 860, 9214, 206, 3901, 386, 10554, 0, 3, 11,...
    ## $ SA          <dbl> 2329, 1105, 380, 1701, 157, 1055, 46, 3415, 2, 10, 1, 0...
    ## $ TAS         <dbl> 516, 480, 168, 599, 31, 752, 124, 1056, 1, 0, 2, 0, 1, ...
    ## $ VIC         <dbl> 7130, 4908, 1001, 5217, 884, 3768, 1501, 6113, 87, 19, ...
    ## $ WA          <dbl> 1, 137, 6, 18, 0, 62, 5, 5, 0, 0, 0, 0, 0, 3, 0, 0, 0, ...
    ## $ Total       <dbl> 15323, 21415, 4127, 26339, 1612, 15162, 2509, 31202, 91...

    ##       year             animal_type               outcome         ACT        
    ##  Min.   :1999   Cats         :114   Euthanized       :120   Min.   :   0.0  
    ##  1st Qu.:2004   Dogs         :114   Other            :120   1st Qu.:   1.0  
    ##  Median :2009   Horses       :114   Transferred      :102   Median :  36.0  
    ##  Mean   :2009   Livestock    :114   Reclaimed        :100   Mean   : 201.9  
    ##  3rd Qu.:2014   Other Animals:114   Rehomed          :100   3rd Qu.: 191.0  
    ##  Max.   :2018   Wildlife     : 94   Currently In Care: 60   Max.   :1628.0  
    ##                                     (Other)          : 62                   
    ##       NSW                NT              QLD                SA        
    ##  Min.   :    0.0   Min.   :   0.0   Min.   :    0.0   Min.   :   0.0  
    ##  1st Qu.:    9.0   1st Qu.:   0.0   1st Qu.:   15.0   1st Qu.:   2.0  
    ##  Median :   78.5   Median :   1.0   Median :  198.5   Median :  34.5  
    ##  Mean   :  970.6   Mean   : 210.8   Mean   : 1289.9   Mean   : 318.3  
    ##  3rd Qu.:  514.8   3rd Qu.:  60.0   3rd Qu.:  854.0   3rd Qu.: 226.8  
    ##  Max.   :13267.0   Max.   :8150.0   Max.   :15690.0   Max.   :4252.0  
    ##                    NA's   :11                         NA's   :2       
    ##       TAS              VIC                 WA              Total      
    ##  Min.   :   0.0   Min.   :    0.00   Min.   :   0.00   Min.   :    0  
    ##  1st Qu.:   3.0   1st Qu.:   13.75   1st Qu.:   0.00   1st Qu.:  162  
    ##  Median :  21.0   Median :  108.50   Median :   2.00   Median : 1015  
    ##  Mean   : 141.3   Mean   :  992.26   Mean   :  88.47   Mean   : 4199  
    ##  3rd Qu.:  96.0   3rd Qu.:  680.75   3rd Qu.:  46.25   3rd Qu.: 2839  
    ##  Max.   :1974.0   Max.   :10567.00   Max.   :6035.00   Max.   :42731  
    ##  NA's   :1                                             NA's   :3

``` r
brisbane_complaints %T>%
  glimpse() %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()
```

    ## Rows: 31,330
    ## Columns: 7
    ## $ nature             <chr> "Animal", "Animal", "Animal", "Animal", "Animal"...
    ## $ animal_type        <chr> "Dog", "Dog", "Dog", "Dog", "Attack", "Attack", ...
    ## $ category           <chr> "Fencing Issues", "Fencing Issues", "Defecating ...
    ## $ suburb             <chr> "SUNNYBANK", "SUNNYBANK HILLS", "SUNNYBANK", "SU...
    ## $ date_range         <chr> "1st-quarter-2016-17.csv", "1st-quarter-2016-17....
    ## $ responsible_office <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ city               <chr> "Brisbane", "Brisbane", "Brisbane", "Brisbane", ...

    ##     nature            animal_type                   category   
    ##  Animal:31330   Attack      : 8539   Fencing Issues     :5490  
    ##                 Cat         : 2070   Wandering          :4250  
    ##                 Cat Trapping: 2675   Attack On An Animal:3039  
    ##                 Dog         :13334   Attack On A Person :2221  
    ##                 Other Animal: 4712   Unregistered       :2025  
    ##                                      (Other)            :8626  
    ##                                      NA's               :5679  
    ##            suburb     
    ##  INALA        : 1053  
    ##  BRACKEN RIDGE:  600  
    ##  WYNNUM       :  575  
    ##  FOREST LAKE  :  542  
    ##  ACACIA RIDGE :  434  
    ##  (Other)      :27818  
    ##  NA's         :  308  
    ##                                                              date_range   
    ##  cars-srsa-open-data-animal-related-complaints-apr-to-jun-2020.csv: 2799  
    ##  cars-srsa-open-data-animal-related-complaints-jan-to-mar-2020.csv: 2146  
    ##  jul-to-sep-2019.csv                                              : 2143  
    ##  jul-to-sep-2018.csv                                              : 2101  
    ##  oct-to-dec-2018.csv                                              : 1968  
    ##  apr-jun-2019.csv                                                 : 1954  
    ##  (Other)                                                          :18219  
    ##  responsible_office       city      
    ##  Mode:logical       Brisbane:31330  
    ##  NA's:31330                         
    ##                                     
    ##                                     
    ##                                     
    ##                                     
    ## 

How many types of compants are there? What about by animal

``` r
animal_complaints %>%
  group_by(`Animal Type`, `Complaint Type`) %>%
  count() %>%
  knitr::kable()
```

| Animal Type | Complaint Type    |    n |
| :---------- | :---------------- | ---: |
| cat         | Enclosure         |  610 |
| cat         | Private Impound   | 3046 |
| cat         | Wandering         |  438 |
| dog         | Aggressive Animal | 4459 |
| dog         | Attack            | 3836 |
| dog         | Enclosure         | 5672 |
| dog         | Noise             | 9205 |
| dog         | Private Impound   | 9646 |
| dog         | Wandering         | 5501 |
