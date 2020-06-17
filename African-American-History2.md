African-American History
================
Machiel Visser
16/06/2020

### Get the data

``` r
library(tidytuesdayR)
library(tidyverse)
theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load('2020-06-16')
```

    ## 
    ##  Downloading file 1 of 4: `census.csv`
    ##  Downloading file 2 of 4: `slave_routes.csv`
    ##  Downloading file 3 of 4: `african_names.csv`
    ##  Downloading file 4 of 4: `blackpast.csv`

``` r
blackpast <- tuesdata$blackpast
census <- tuesdata$census
slave_routes <- tuesdata$slave_routes
african_names <- tuesdata$african_names
```

### Census data

``` r
census %>%
  filter(is.na(division)) %>% 
  ggplot(aes(x = year, y = total, color = region)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1800, 1860, 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total population size in the USA",
       x = "Year",
       y = "Population size",
       colour = "Region")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
census %>% 
  filter(is.na(division),
         region != "USA Total") %>% 
  ggplot(aes(x = year, y = total, fill = region)) +
  geom_area(alpha = 0.8) +
  scale_x_continuous(breaks = seq(1800, 1860, 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total population size in the USA",
       x = "Year",
       y = "Population size",
       fill = "Region")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
census %>% 
  pivot_longer(cols = total:black_slaves, names_to = "population", values_to = "value") %>% 
  filter(population %in% c("white", "black")) %>% 
  ggplot(aes(x = year, y = value, fill = population)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1790, 1870, 10), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total population size in the USA by ethnic group",
       x = "Year",
       y = "Population size", 
       fill = "Population")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
census %>% 
  pivot_longer(cols = total:black_slaves, names_to = "population", values_to = "value") %>% 
  filter(population %in% c("black_free", "black_slaves", "white")) %>% 
  ggplot(aes(x = year, y = value, fill = population)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1790, 1870, 10), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total population size in the USA by ethnic group", 
       x = "Year",
       y = "Population size", 
       fill = "Population")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
census %>% 
  pivot_longer(cols = total:black_slaves, names_to = "population", values_to = "value") %>% 
  filter(population %in% c("black_free", "black_slaves")) %>% 
  ggplot(aes(x = year, y = value, fill = population)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1790, 1870, 10), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Black population in the USA",
       x = "Year",
       y = "Population size", 
       fill = "Population")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
census %>% 
  filter(region == "USA Total") %>% 
  ggplot(aes(x = year, y = black_slaves)) +
  geom_col(alpha = 0.9) +
  scale_x_continuous(breaks = seq(1790, 1870, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total number of black slaves in the USA", 
       x = "Year",
       y = "Population size") +
  theme(legend.position = "none")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
census %>% 
  filter(region != "USA Total") %>% 
  ggplot(aes(x = year, y = black_slaves, fill = region)) +
  geom_col(alpha = 0.9) +
  scale_x_continuous(breaks = seq(1790, 1870, 20)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Number of black slaves in each region in the USA",
       x = "Year",
       y = "Population size") +
  theme(legend.position = "none")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

``` r
census %>% 
  filter(!is.na(division)) %>% 
  ggplot(aes(x = year, y = black_slaves, fill = division)) +
  geom_col(alpha = 0.9) +
  scale_x_continuous(breaks = seq(1790, 1870, 20)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ division, ncol = 3, scales = "free_y") +
  labs(title = "Number of black slaves in each division in the USA",
       x = "Year",
       y = "Population size") +
  theme(legend.position = "none")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->

### Slave routes

``` r
slave_routes %>% 
  ggplot(aes(x = year_arrival)) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  labs(title = "Number of voyages per year",
       x = "Year of arrival",
       y = "Number of voyages")
```

![](African-American-History_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
slave_routes %>% 
  filter(!is.na(n_slaves_arrived),
         !is.na(ship_name)) %>% 
  group_by(ship_name) %>% 
  mutate(total_slaves_arrived = sum(n_slaves_arrived)) %>% 
  ungroup() %>% 
  select(ship_name, total_slaves_arrived) %>% 
  distinct() %>% 
  arrange(desc(total_slaves_arrived)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = reorder(ship_name, total_slaves_arrived), y = total_slaves_arrived)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total number of slaves that arrived per ship",
       x = "", 
       y = "Total number of slaves")
```

    ## Selecting by total_slaves_arrived

![](African-American-History_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
slave_routes %>% 
  filter(!is.na(port_origin)) %>% 
  count(port_origin, sort = TRUE)
```

    ## # A tibble: 243 x 2
    ##    port_origin                            n
    ##    <chr>                              <int>
    ##  1 Liverpool                           4973
    ##  2 Bahia, port unspecified             4478
    ##  3 London                              3126
    ##  4 Rio de Janeiro                      2545
    ##  5 Bristol                             2083
    ##  6 Nantes                              1731
    ##  7 Pernambuco, port unspecified        1310
    ##  8 Havana                              1146
    ##  9 Southeast Brazil, port unspecified  1126
    ## 10 Lisbon                              1098
    ## # ... with 233 more rows

``` r
slave_routes %>% 
  filter(!is.na(port_arrival)) %>% 
  count(port_arrival, sort = TRUE)
```

    ## # A tibble: 273 x 2
    ##    port_arrival                     n
    ##    <chr>                        <int>
    ##  1 Bahia, port unspecified       4223
    ##  2 Rio de Janeiro                2887
    ##  3 Barbados, port unspecified    2038
    ##  4 Jamaica, port unspecified     1715
    ##  5 Kingston                      1622
    ##  6 Pernambuco, port unspecified  1335
    ##  7 Havana                        1321
    ##  8 Cap Francais                  1127
    ##  9 Charleston                     811
    ## 10 Suriname                       761
    ## # ... with 263 more rows
