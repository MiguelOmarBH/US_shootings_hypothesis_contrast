us_shooting_analysis
================
Miguel Omar
2023-07-31

## Introduction

Wellcome! In this markdown we are going to perform a statistical
analysis on United States (US) shootings across different states. It’s R
based, and We’ll learn how to:

- Cleaning and tranform data
- Summarize and graphically represent grouped data
- Check for normality assumptions
- Test for statisticaly significant differences between independent
  samples

### Our data and the starting assumption

The data for this markdown was obtained from the [Gun Violence
Archive](https://www.gunviolencearchive.org/) and contains information
about the gun incidents produced in US by state.

US shooting incidents are a concerning nationwide problem. Since some
political branches defend the individual and constitutional right to
carry guns, some people argue that shooting incidents are more prone to
occur in those states in which the support of those political forces are
majority. Some others defends that this assumption is not true.

Given this initial problem, the purpose of this analyis is to determine
which assumption is true. Shooting incidents data were obtained from
2021 till the present (31-07-2023), after the 2020 Presidential
Election.

## Importing libraries, dataset and quick transformations

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(nortest)
data <- readxl::read_xlsx("shootings_us.xlsx")
```

Now, let’s have a look to the dataset and it’s structure:

``` r
as_tibble(data)
```

    ## # A tibble: 1,750 × 8
    ##    `Incident ID` `Incident Date`     State       `City Or County` Address Killed
    ##            <dbl> <dttm>              <chr>       <chr>            <chr>    <dbl>
    ##  1       2666201 2023-07-28 00:00:00 Florida     Tampa            E Shad…      1
    ##  2       2666189 2023-07-28 00:00:00 Washington  Seattle          9232 R…      0
    ##  3       2666500 2023-07-28 00:00:00 Florida     Fort Lauderdale  NW 19t…      0
    ##  4       2666195 2023-07-28 00:00:00 Illinois    Chicago Heights… 1400 b…      1
    ##  5       2663533 2023-07-25 00:00:00 Pennsylvan… Pittsburgh       Nichol…      0
    ##  6       2662351 2023-07-24 00:00:00 Oklahoma    Tulsa            11107 …      3
    ##  7       2662711 2023-07-24 00:00:00 New York    Bronx            255 E …      1
    ##  8       2661359 2023-07-23 00:00:00 Florida     Tampa            7402 N…      0
    ##  9       2661332 2023-07-23 00:00:00 Alabama     Birmingham       1700 b…      0
    ## 10       2661056 2023-07-23 00:00:00 Texas       Houston          9261 G…      1
    ## # ℹ 1,740 more rows
    ## # ℹ 2 more variables: Injured <dbl>, State_Gov <chr>

``` r
colnames(data)
```

    ## [1] "Incident ID"    "Incident Date"  "State"          "City Or County"
    ## [5] "Address"        "Killed"         "Injured"        "State_Gov"

At a glance we can observe that we have 1750 observations and 8 columns
regarding the incident ID, the date, the state or the political party
that had the most support on the 2020 goverment election. Each incident
ID represents a single shooting observation. We can also see that the
column names have uppercase and blank spaces, so to avoid problems let’s
format them:

``` r
data <- clean_names(data)
```

## Summarizing and preparing data for the analysis

The actual dataset is organized by single incidents, but we want now to
have it organized by state and the number of total incidents ocurred:

``` r
data_summary <- plyr::ddply(data, c("state", "state_gov"), summarise, shootings = length(state))
# We'll create a new dataset to have each state, the goverment support and the total number of shooting incidents
as_tibble(data_summary)
```

    ## # A tibble: 48 × 3
    ##    state                state_gov  shootings
    ##    <chr>                <chr>          <int>
    ##  1 Alabama              Republican        46
    ##  2 Alaska               Republican         1
    ##  3 Arizona              Democrat          26
    ##  4 Arkansas             Republican        14
    ##  5 California           Democrat         124
    ##  6 Colorado             Democrat          32
    ##  7 Connecticut          Democrat           6
    ##  8 Delaware             Democrat           8
    ##  9 District of Columbia Democrat          31
    ## 10 Florida              Republican        85
    ## # ℹ 38 more rows

We have our data better organised to perform our analysis. Now it’s time
to summarize it by creating a table showing the mean, standard
deviation, the min and max grouping by republican and democrat
supportive states. We’ll also create a boxplot to simply visualize the
general informations:

``` r
data_summary %>%
  group_by(state_gov) %>% 
  summarise(Mean = mean(shootings), Sd = sd(shootings), Min = min(shootings), Max = max(shootings))
```

    ## # A tibble: 2 × 5
    ##   state_gov   Mean    Sd   Min   Max
    ##   <chr>      <dbl> <dbl> <int> <int>
    ## 1 Democrat    38.4  41.9     1   171
    ## 2 Republican  34.3  37.3     1   148

``` r
ggplot(data = data_summary, mapping = aes(x= state_gov, y = shootings)) +
  geom_boxplot()
```

![unnamed-chunk-5-1](https://github.com/MiguelOmarBH/US_shootings_hypothesis_contrast/assets/135857016/ff6588ed-9623-4b69-bc9a-9314be58c57e)


The summary table and the boxplot gave us an interesting insight: the
democrat supportive states have a higher shooting rate, but also a
higher variability (SD). Given that, we have to check if that difference
is significant in statistical terms.

IMPORTANT: as we can see in the boxplot both republican and democrat
groups have one outlier. It’s important to have this into account as it
can affect our results. Given out sample size (1750 observations) having
one outlier per group won’t affect our analysis, but it’s important to
keep it in mind anyway.

## Hypothesis testing

The purpose of the analysis is to check if there’s a difference on
shooting incidents (dependent variable) by democrat and republican
supportive states (independent variable). That is: we are doing a
hypothesis testing for two independent samples. Given that, a T-Test is
the statistical tool that best fits for our study. But it’s crucial to
check if our dependent variable follows a normal distribution.

``` r
ad.test(data_summary$shootings)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  data_summary$shootings
    ## A = 2.3256, p-value = 5.561e-06

The Anderson-Darling test brought us two values:

- The A value: representing the discrepancy between our sample
  distribution and the theoretical distribution
- The P-value: the probability that the A value is consistent with the
  null hipothesis

The null hipothesis assumes that our sample follows a normal
distribution. The P-value is very low (\<0.05), indicating that the null
hipothesis is not true. Our sample is not normally distributed.

### Checking for significant differences

Having a normally distributed sample is crucial to perform a T-test. In
this step we can follow two steps:

- Transform our data to make it normally distributed (using a log
  transform e.g.)
- Using a non parametric test

In that case we’ll use a non parametric test. The Wilcoxon-Mann-Whitney
test is the non-parametric alternative to the T-Test, and it uses the
median rather than the mean. To perform it, let’s have our samples
prepared:

``` r
republican_shootings <- data_summary[data_summary$state_gov == "Republican", "shootings"]
democrat_shootings <- data_summary[data_summary$state_gov == "Democrat", "shootings"]

# This code creates two vectors: one for the values of republican supportive states and other one for democrats
```

Now, we can perform the Wilcoxon-Mann-Whitney test:

``` r
test <- wilcox.test(democrat_shootings, republican_shootings, exact = FALSE)
print(test)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  democrat_shootings and republican_shootings
    ## W = 305, p-value = 0.7253
    ## alternative hypothesis: true location shift is not equal to 0

The test brings us two values:

- The W: the sum of the ranks assigned to the data of a specific group
- The P-value: as the normality test, the probability to support the
  null hypothesis

The null hypothesis assumes that the differences between our groups are
not significant. Our P-value is high (\>0.05), supporting the null
hypothesis.

## Conclusion

Given our analysis we observed that the democrat supportive states had a
higher shooting rate, but testing for statistical significance we can’t
assume that these differences are due to our independent variable. In
other words: being a democrat or republican supportive state doesn’t
explain the shooting incidence rate.

In this markdown we’ve showed how to organize data, summarize and
visualize relevant statistics and how to conduct a hypothesis contrast
checking for normality assumpions before that. Hope this is useful for
everyone reading. Thank you!
