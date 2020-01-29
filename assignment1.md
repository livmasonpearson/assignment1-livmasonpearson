Statistical assignment 1
================
Olivia Mason Pearson, 670025149
29/01/2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code below.

``` r
library(tidyverse)
setwd("/Users/LIV/Desktop/DATA ANALYSIS")
Data <- read_tsv("/Users/LIV/Desktop/DATA ANALYSIS/DATA ANALYSIS III/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
library(dplyr)
Data <- Data %>%
        select(pidp, h_memorig, h_eumem, h_sex_dv, h_age_dv)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- Data %>%
        filter(h_memorig==1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
        mutate(EU = ifelse(
          h_eumem == 1,1,
          ifelse(h_eumem == 2, 0, NA)))

        
Data <- Data %>%
  mutate(sex = ifelse(h_sex_dv == 1, "male",
                            ifelse(h_sex_dv == 2, "female", NA)))


Data <- Data%>%
  mutate(agegr = case_when(
    between(h_age_dv, 16, 25) ~ "16 to 25",
    between(h_age_dv, 26, 40) ~ "26 to 40",
    between(h_age_dv, 41, 55) ~ "41 to 55",
    between(h_age_dv, 56, 70) ~ "56 to 70",
    h_age_dv >= 70 ~ "over 70"
  ))


table(Data$EU, useNA = "always")
```

    ## 
    ##     0     1  <NA> 
    ##  9338 11118  2501

``` r
table(Data$sex, useNA = "always")
```

    ## 
    ## female   male   <NA> 
    ##  12486  10470      1

``` r
table(Data$agegr, useNA = "always")
```

    ## 
    ## 16 to 25 26 to 40 41 to 55 56 to 70  over 70     <NA> 
    ##     2885     4384     6150     5941     3597        0

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
  count(EU) %>%
  mutate(perc = n/sum(n) *100)
```

    ## # A tibble: 3 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  40.7
    ## 2     1 11118  48.4
    ## 3    NA  2501  10.9

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

From this result, I can see that 48.43%, or 48%, of people voted to
Remain, and 40.7, or 41% voted to Leave the EU. This differs to the
actual result of the 2016 referendum with, according to the BBC, 51.9%
voting to Leave and 48.1% voting to Remain. Considering this large
difference in Leave votes, I can assume that those that refused,
inapplicable, proxy or otherwise (NA) voted Leave, as it accounted for
nearly 11% of all respondents.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  group_by(agegr, sex) %>%
  summarise(propEU = mean(EU, na.rm = TRUE) * 100)
```

    ## # A tibble: 11 x 3
    ## # Groups:   agegr [5]
    ##    agegr    sex    propEU
    ##    <chr>    <chr>   <dbl>
    ##  1 16 to 25 female   72.6
    ##  2 16 to 25 male     66.1
    ##  3 16 to 25 <NA>    100  
    ##  4 26 to 40 female   64.0
    ##  5 26 to 40 male     58.8
    ##  6 41 to 55 female   56.9
    ##  7 41 to 55 male     51.7
    ##  8 56 to 70 female   50.5
    ##  9 56 to 70 male     47.6
    ## 10 over 70  female   44.2
    ## 11 over 70  male     37.6

Write a couple of sentences interpreting your results.

These results clearly show a steady decline of Remain votes as age
increases. On average, 69% of voters aged 16-25 voted to remain in the
EU (66% for males, 73% for females) compared with just 41% of over 70s
(44% for females, 38% for males). In addition, I can see that females
consistently voted more for remain than males, with at least a 3%
increase from male to female in remain votes in every age category.
