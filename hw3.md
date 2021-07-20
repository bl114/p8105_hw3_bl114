Homework 3
================
Ben Lebwohl

## Problem 1

Load the instacart dataset

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
data("instacart")

knitr::opts_chunk$set(
  fig.width = 8,
  fig.asp = 0.6,
  out.width = "90%")
```

*Write a short description of the dataset, noting the size and structure
of the data, describing some key variables, and giving illustrative
examples of observations.*

``` r
instacart
```

    ## # A tibble: 1,384,617 x 15
    ##    order_id product_id add_to_cart_order reordered user_id eval_set order_number
    ##       <int>      <int>             <int>     <int>   <int> <chr>           <int>
    ##  1        1      49302                 1         1  112108 train               4
    ##  2        1      11109                 2         1  112108 train               4
    ##  3        1      10246                 3         0  112108 train               4
    ##  4        1      49683                 4         0  112108 train               4
    ##  5        1      43633                 5         1  112108 train               4
    ##  6        1      13176                 6         0  112108 train               4
    ##  7        1      47209                 7         0  112108 train               4
    ##  8        1      22035                 8         1  112108 train               4
    ##  9       36      39612                 1         0   79431 train              23
    ## 10       36      19660                 2         1   79431 train              23
    ## # … with 1,384,607 more rows, and 8 more variables: order_dow <int>,
    ## #   order_hour_of_day <int>, days_since_prior_order <int>, product_name <chr>,
    ## #   aisle_id <int>, department_id <int>, aisle <chr>, department <chr>

``` r
view(instacart)
```

The data set is a tibble, with &gt;1.3 million observations and 14
columns. Each row is an order of a product. These come in batches of
orders (order\_id) and are tied to a user (user\_id). Each product is
associated with an aisle (aisle\_id) and department (department\_id)

Let’s see some of the more common departments and aisles that are
ordered:

``` r
instacart %>% 
  group_by(department, aisle) %>% 
  summarize(
    n_obs = n()
  ) %>% 
  arrange(desc(n_obs))
```

    ## `summarise()` has grouped output by 'department'. You can override using the `.groups` argument.

    ## # A tibble: 134 x 3
    ## # Groups:   department [21]
    ##    department aisle                          n_obs
    ##    <chr>      <chr>                          <int>
    ##  1 produce    fresh vegetables              150609
    ##  2 produce    fresh fruits                  150473
    ##  3 produce    packaged vegetables fruits     78493
    ##  4 dairy eggs yogurt                         55240
    ##  5 dairy eggs packaged cheese                41699
    ##  6 beverages  water seltzer sparkling water  36617
    ##  7 dairy eggs milk                           32644
    ##  8 snacks     chips pretzels                 31269
    ##  9 dairy eggs soy lactosefree                26240
    ## 10 bakery     bread                          23635
    ## # … with 124 more rows

*How many aisles are there, and which aisles are the most items ordered
from?*

``` r
instacart %>% 
  group_by(aisle) %>% 
  summarize(
    n_obs = n()
  ) %>% 
  arrange(desc(n_obs))
```

    ## # A tibble: 134 x 2
    ##    aisle                          n_obs
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

There are 134 aisles, and the aisles that most items are ordered from
are fresh vegetables and fresh fruits.

*Make a plot that shows the number of items ordered in each aisle,
limiting this to aisles with more than 10000 items ordered. Arrange
aisles sensibly, and organize your plot so others can read it.*

``` r
instacart %>% 
  group_by(aisle) %>% 
  summarize(
    n_obs = n()
  ) %>% 
  filter(n_obs >= 10000) %>% 
     mutate(aisle = fct_reorder(aisle, n_obs)) %>% 
  ggplot(aes(x = aisle, y = n_obs, label = aisle)) +
geom_point() +
scale_y_log10(breaks = c(20000, 40000, 60000, 80000, 100000, 150000), labels = c("20000", "40000", "60000", "80000", "100000", "150000")) +
  geom_text(aes(label=aisle, hjust = 0, vjust = 1), size = 3, check_overlap = TRUE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(
    y = "Number of orders" 
  )
```

<img src="hw3_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

*Make a table showing the three most popular items in each of the aisles
“baking ingredients”, “dog food care”, and “packaged vegetables fruits”.
Include the number of times each item is ordered in your table.*

``` r
instacart %>% 
  group_by(aisle, product_name) %>% 
  summarize(
    n_obs = n()
  ) %>% 
  mutate(
    order_rank = min_rank(-n_obs)
  ) %>% 
  filter(aisle == "baking ingredients" | aisle == "dog food care" | aisle == "packaged vegetables fruits") %>% 
  filter(order_rank <= 3) %>% 
  arrange(aisle, order_rank)
```

    ## `summarise()` has grouped output by 'aisle'. You can override using the `.groups` argument.

    ## # A tibble: 9 x 4
    ## # Groups:   aisle [3]
    ##   aisle                  product_name                           n_obs order_rank
    ##   <chr>                  <chr>                                  <int>      <int>
    ## 1 baking ingredients     Light Brown Sugar                        499          1
    ## 2 baking ingredients     Pure Baking Soda                         387          2
    ## 3 baking ingredients     Cane Sugar                               336          3
    ## 4 dog food care          Snack Sticks Chicken & Rice Recipe Do…    30          1
    ## 5 dog food care          Organix Chicken & Brown Rice Recipe       28          2
    ## 6 dog food care          Small Dog Biscuits                        26          3
    ## 7 packaged vegetables f… Organic Baby Spinach                    9784          1
    ## 8 packaged vegetables f… Organic Raspberries                     5546          2
    ## 9 packaged vegetables f… Organic Blueberries                     4966          3

*Make a table showing the mean hour of the day at which Pink Lady Apples
and Coffee Ice Cream are ordered on each day of the week; format this
table for human readers (i.e. produce a 2 x 7 table).*

``` r
instacart %>% 
  filter(product_name == "Pink Lady Apples" | product_name == "Coffee Ice Cream") %>% 
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour = mean(order_hour_of_day)) %>% 
  pivot_wider(names_from = product_name, values_from = mean_hour)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the `.groups` argument.

    ## # A tibble: 7 x 3
    ##   order_dow `Coffee Ice Cream` `Pink Lady Apples`
    ##       <int>              <dbl>              <dbl>
    ## 1         0               13.8               13.4
    ## 2         1               14.3               11.4
    ## 3         2               15.4               11.7
    ## 4         3               15.3               14.2
    ## 5         4               15.2               11.6
    ## 6         5               12.3               12.8
    ## 7         6               13.8               11.9

## Problem 2

*Load, tidy, and otherwise wrangle the data. Your final dataset should
include all originally observed variables and values; have useful
variable names; include a weekday vs weekend variable; and encode data
with reasonable variable classes. Describe the resulting dataset
(e.g. what variables exist, how many observations, etc).*

``` r
accel_df =
read_csv("./data/accel_data.csv") %>% 
janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440, 
    names_to = "minute", 
    names_prefix = "activity_", 
    values_to = "activity") %>% 
  mutate(
    type_of_day = ifelse(day == "Saturday" | day == "Sunday" , "weekend", "weekday")
  )
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
accel_df %>% 
  group_by(day_id) %>% 
  summarize(n_obs = n())
```

    ## # A tibble: 35 x 2
    ##    day_id n_obs
    ##     <dbl> <int>
    ##  1      1  1440
    ##  2      2  1440
    ##  3      3  1440
    ##  4      4  1440
    ##  5      5  1440
    ##  6      6  1440
    ##  7      7  1440
    ##  8      8  1440
    ##  9      9  1440
    ## 10     10  1440
    ## # … with 25 more rows

The data set contains are 35 days, each with activity data on each of
the 1440 minutes of the day.

*Traditional analyses of accelerometer data focus on the total activity
over the day. Using your tidied dataset, aggregate accross minutes to
create a total activity variable for each day, and create a table
showing these totals. Are any trends apparent?*

``` r
accel_df %>% 
  group_by(day_id, type_of_day) %>% 
  summarize (total_activity = sum(activity)) %>% 
  ggplot(aes(x = day_id, y = total_activity, color = type_of_day)) +
  geom_point()
```

    ## `summarise()` has grouped output by 'day_id'. You can override using the `.groups` argument.

<img src="hw3_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

There is wide variability in activity! Weekends tend to have lower total
activity.

*Accelerometer data allows the inspection activity over the course of
the day. Make a single-panel plot that shows the 24-hour activity time
courses for each day and use color to indicate day of the week. Describe
in words any patterns or conclusions you can make based on this graph.*

``` r
accel_df %>% 
  ggplot(aes(x = minute, y = activity, color = day)) +
  geom_point() 
```

<img src="hw3_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

## Problem 3
