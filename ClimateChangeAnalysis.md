Climate Change Data Exploration Tidyverse
================

Obtaining the Data
==================

We have to specify the column types, as by default readr will believe 'Per Capita' to be an integer when infact it needs to be a double.

``` r
climate<-read_csv("https://pkgstore.datahub.io/core/co2-fossil-by-nation/fossil-fuel-co2-emissions-by-nation_csv/data/0f04181960a0a896ebaf6d8afb0b71a6/fossil-fuel-co2-emissions-by-nation_csv.csv",
                  col_types = cols(
                    Year = col_integer(),
                    Country = col_character(),
                    Total = col_integer(),
                    `Solid Fuel` = col_integer(),
                    `Liquid Fuel` = col_integer(),
                    `Gas Fuel` = col_integer(),
                    Cement = col_integer(),
                    `Gas Flaring` = col_integer(),
                    `Per Capita` = col_double(),
                    `Bunker fuels (Not in Total)` = col_integer())
)
```

Inspecting the top pollutors, per year and all time.
====================================================

Some basic aggregation validates the received wisdom that the US has historically been the biggest contributor to global warming, but that China is now a bigger issue.

``` r
climate %>% top_n(20, Total) %>% arrange(desc(Total))
```

    ## # A tibble: 20 x 10
    ##     Year Country        Total `Solid Fuel` `Liquid Fuel` `Gas Fuel` Cement
    ##    <int> <chr>          <int>        <int>         <int>      <int>  <int>
    ##  1  2014 CHINA (MAINL… 2.81e6      2026492        344725      96504 338912
    ##  2  2013 CHINA (MAINL… 2.80e6      2045156        336960      87371 327896
    ##  3  2012 CHINA (MAINL… 2.73e6      2037338        321155      75764 300560
    ##  4  2011 CHINA (MAINL… 2.65e6      1995029        306730      67137 285464
    ##  5  2010 CHINA (MAINL… 2.39e6      1792793        298191      54472 247792
    ##  6  2009 CHINA (MAINL… 2.18e6      1645333        268108      44870 223584
    ##  7  2008 CHINA (MAINL… 2.06e6      1563721        264770      40850 190400
    ##  8  2007 CHINA (MAINL… 1.92e6      1443751        253098      35348 185119
    ##  9  2006 CHINA (MAINL… 1.78e6      1338803        245233      28317 168201
    ## 10  2005 CHINA (MAINL… 1.61e6      1207530        232034      23187 145364
    ## 11  2005 UNITED STATE… 1.58e6       579104        667143     317152  13723
    ## 12  2007 UNITED STATE… 1.58e6       575692        648278     339433  13172
    ## 13  2004 UNITED STATE… 1.57e6       573935        662168     318710  13466
    ## 14  2006 UNITED STATE… 1.55e6       571099        653101     313999  13561
    ## 15  2000 UNITED STATE… 1.55e6       564757        632129     342282  12173
    ## 16  2003 UNITED STATE… 1.55e6       560926        648067     324514  12829
    ## 17  2002 UNITED STATE… 1.54e6       561866        628411     334252  12412
    ## 18  2008 UNITED STATE… 1.53e6       566810        611445     338359  11915
    ## 19  2001 UNITED STATE… 1.53e6       555810        634801     321651  12301
    ## 20  1999 UNITED STATE… 1.50e6       541784        618335     327459  11938
    ## # ... with 3 more variables: `Gas Flaring` <int>, `Per Capita` <dbl>,
    ## #   `Bunker fuels (Not in Total)` <int>

``` r
top10alltime <- climate %>% 
         group_by(Country) %>%
         summarise(total = sum(Total)) %>%
         arrange(desc(total)) %>% 
         top_n(10)
```

    ## Selecting by total

``` r
top10alltime
```

    ## # A tibble: 10 x 2
    ##    Country                       total
    ##    <chr>                         <int>
    ##  1 UNITED STATES OF AMERICA  102510260
    ##  2 CHINA (MAINLAND)           47649834
    ##  3 USSR                       30790355
    ##  4 UNITED KINGDOM             20500813
    ##  5 JAPAN                      14585037
    ##  6 GERMANY                    12764185
    ##  7 INDIA                      11385351
    ##  8 RUSSIAN FEDERATION         10466421
    ##  9 FRANCE (INCLUDING MONACO)   9697149
    ## 10 CANADA                      8038299

``` r
climate %>% 
  filter(Country %in% top10alltime$Country) %>%
  filter(Year>2000) %>%
  ggplot(aes(x=Year, y =Total)) +
    geom_line() +
    theme(legend.position="none") +
    facet_grid(. ~ Country) +
    ggtitle("Total co2 Emissions of Largest 10 Polluters" , subtitle = "2000-2014")
```

![](ClimateChangeAnalysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

Trends in co2 emmissions origin - big picture
=============================================

``` r
climate %>% 
  gather(key=fueltype, Emissions, `Solid Fuel`:`Gas Flaring`) %>% 
  ggplot(aes(x=Year, y=Emissions, color = fueltype)) + geom_bar(stat="identity")
```

![](ClimateChangeAnalysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

Visualising recent changes - total
==================================

``` r
climate %>% 
    filter(Year>2000) %>%
    gather(key=fueltype, Emissions, `Solid Fuel`:`Gas Flaring`) %>% 
    group_by(fueltype, Year) %>%
    summarise(emissions = sum(Emissions)) %>%
    ggplot(aes(x=Year, y=emissions, fill = fueltype)) + geom_bar(stat="identity") + facet_grid(fueltype ~ ., scales = "free_y")
```

![](ClimateChangeAnalysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

Specific Changes by Emission Source of the largest pollutors.
=============================================================

We let the y axis scale per emission source, worth keeping an eye on the overall scale.

``` r
climate %>% 
    filter(Year>2000) %>%
    filter(Country %in% top10alltime$Country) %>%
    gather(key=fueltype, Emissions, `Solid Fuel`:`Gas Flaring`) %>% 
    ggplot(aes(x=Year, y=Emissions, fill = fueltype)) +
        geom_bar(stat="identity") + 
        facet_grid(fueltype ~ Country, scales = "free_y", labeller = label_wrap_gen(10)) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90))
```

![](ClimateChangeAnalysis_files/figure-markdown_github/unnamed-chunk-6-1.png)
