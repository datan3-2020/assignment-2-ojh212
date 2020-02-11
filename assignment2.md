Data analysis assignment 2
================
Olivia Hudson
05/02/2020

In this assignment you will work with relational data, i.e. data coming
from different data tables that you can combine using keys. Please read
ch.13 from R for Data Science before completing this assignment –
<https://r4ds.had.co.nz/relational-data.html>.

## Read data

We will work with three different tables: household roster from wave 8
(*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and
household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("C:/Users/Liv/OneDrive - University of Exeter/Second Year/Term 2/POL2094 Data Analysis iii/Github/datan3/Data/6614tab_10EB1BA6123C0D95E60D28E156AEA8F7_V1/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

## Filter household roster data (10 points)

The **egoalt8** data table contains data on the kin and other
relationships between people in the same household. In each row in this
table you will have a pair of individuals in the same household: ego
(identified by *pidp*) and alter (identified by *apidp*).
*h\_relationship\_dv* shows the type of relationship between ego and
alter. You can check the codes in the Understanding Society codebooks
here –
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and
wives or cohabiting partners (codes 1 and 2). For convenience, we also
want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household
identifier), *h\_relationship\_dv*, *h\_esex* (ego’s sex), and *h\_asex*
(alter’s sex). (ego’s sex is coded in Egoalt8 as h\_sex now)

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1|h_relationship_dv == 2) %>%
        select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex)
```

Each couple now appears in the data twice: 1) with one partner as ego
and the other as alter, 2) the other way round. Now we will only focus
on heterosexual couples, and keep one observation per couple with women
as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%
        # filter out same-sex couples
        filter(h_sex + h_asex == 3) %>%
        # keep only one observation per couple with women as egos
        filter(h_sex == 2)
```

## Recode data on ethnicity (10 points)

In this assignment we will explore ethnic endogamy, i.e. marriages and
partnerships within the same ethnic group. First, let us a create a
version of the table with stable individual characteristics with two
variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

Let’s code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable
with the following values: “White” (codes 1 to 4) and “non-White” (all
other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = case_when(
                between(racel_dv, 1,4) ~ "White",
                racel_dv > 4 ~ "non-White"))
```

## Join data (30 points)

Now we want to join data from the household roster (*Hetero8*) and the
data table with ethnicity (*Stable2*). First let us merge in the data on
ego’s ethnicity. We want to keep all the observations we have in
*Hetero8*, but we don’t want to add any other individuals from
*Stable2*.

``` r
JoinedEthn <- Hetero8 %>%
        left_join(Stable2)
```

Let us rename the variables for ethnicity to clearly indicate that they
refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter’s ethnicity. Note that in this
case the key variables have different names in two data tables; please
refer to the documentation for your join function (or the relevant
section from R for Data Science) to check the solution for this problem.

``` r
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c("apidp" = "pidp"))
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

Let us start by looking at the joint distribution of race (White
vs. non-White) of both partners.

``` r
TableRace <- JoinedEthn %>%
        # filter out observations with missing data
        filter(!is.na(egoRace), !is.na(alterRace)) %>%
        count(egoRace, alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have
a White partner, 2) for a White woman to have a non-White partner, 3)
for a non-White woman to have a White partner, 4) for a non-White woman
to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the
code will not be reproducible: if the data change the code will need to
be changed, too. Your task is to write reproducible code producing a
table with the required four probabilities.

``` r
TableRace %>% 
        # group by ego's race  to calculate sums
        group_by(egoRace) %>%
        # create a new variable with the total number of women by race
        mutate(sum_women_race = sum(n))  %>%  
        # create a new variable with the required probabilities 
        mutate(prob_women_race = n/sum_women_race)
```

    ## # A tibble: 4 x 5
    ## # Groups:   egoRace [2]
    ##   egoRace   alterRace     n sum_women_race prob_women_race
    ##   <chr>     <chr>     <int>          <int>           <dbl>
    ## 1 non-White non-White  1790           2116          0.846 
    ## 2 non-White White       326           2116          0.154 
    ## 3 White     non-White   266           9960          0.0267
    ## 4 White     White      9694           9960          0.973

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

1)  Join the individual-level file with the household-level data from
    wave 8 (Hh8) (specifically, we want the variable for the number of
    children in the household - h\_nkids\_dv).
2)  Select only couples that are ethnically endogamous (i.e. partners
    come from the same ethnic group) for the following groups: White
    British (1), Indian(9), and Pakistani(10).
3)  Produce a table showing the mean and median number of children in
    these households by ethnic group (make sure the table has meaningful
    labels for ethnic groups, not just numerical codes).
4)  Write a short interpretation of your results. What could affect your
    findings?

<!-- end list -->

``` r
Hh8_and_Indiv <- JoinedEthn  %>%
        left_join(Hh8, by = NULL) %>%
        select(egoRacel_dv, alterRacel_dv, h_nkids_dv) %>%
        filter(egoRacel_dv == 1 & alterRacel_dv == 1| egoRacel_dv == 9 & alterRacel_dv == 9 | egoRacel_dv == 10 & alterRacel_dv == 10) %>% 
        mutate(Couples_Ethnic_Group = case_when(
                egoRacel_dv == 1 ~ "White British",
                egoRacel_dv == 9 ~ "Indian",
                egoRacel_dv == 10 ~ "Pakistani")) %>%
        group_by(Couples_Ethnic_Group) %>%
        summarise(Mean_Number_of_Children = mean(h_nkids_dv, na.rm = TRUE), Median_Number_of_Children = median(h_nkids_dv, na.rm = TRUE))
Hh8_and_Indiv       
```

    ## # A tibble: 3 x 3
    ##   Couples_Ethnic_Group Mean_Number_of_Children Median_Number_of_Children
    ##   <chr>                                  <dbl>                     <dbl>
    ## 1 Indian                                 0.955                         1
    ## 2 Pakistani                              1.81                          2
    ## 3 White British                          0.565                         0

The results from the table above show that:

  - For ethnically endogamous Indian couples, there are a mean number of
    **0.9553753** and a median number of **1** children in the same
    household.

  - For ethnically endogamous Pakistani couples, there are a mean number
    of **1.8108747** and a median of **2** children in the same
    household.

  - For ethnically endogamous White British couples, there are a mean
    number of **0.5651314** and a medain of **0** children in the same
    household.

These results suggest that households with Indian couples have an
average of **1** child living in the same household. Pakistani couples
have an average of **2** children living in the same household and White
British couples have an average of **0** children living in the same
household.

These results suggest that Pakistani couples have on average more
children in their households compared to Indian couples and White
British couples.

However, certain factors could affect these findings:

1.  This is only a sample of the population, there are more white than
    non-white participants in the survey and so the survey results may
    not be representative of the average amount of children per
    household by ethnic group for the whole population.

2.  **Non-response bias** can affect the findings.Some couples may
    refuse to answer how many children are in their household. For
    example, if they have more children living in one household than UK
    Property law allows, they may conceal their answer on the survey to
    protect themselves. This could result in biased inferences if those
    who respond the the question are systematically different to those
    who dont respond to the question; e.g. if White British couples
    refused to respond more compared to Indian couples.

3.  **Misreporting** could also affect the findings. Respondants may lie
    about how many children they have in their household. This can be a
    source of bias.

4.  This data is from 2016-17, since then the amount of children that
    people are having/adopting or living with is likely to have changed
    over time, these findings therefore might not be reflective of the
    current situation and cannot be generalised as such.
