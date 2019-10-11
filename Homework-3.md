Homework 3
================

``` r
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
data("instacart")
ins_aisles = instacart %>% 
  count(aisle, name = "aisle_order_count") %>% 
  arrange(desc(aisle_order_count))


library(ggplot2)
ins_aisles %>% 
filter(aisle_order_count > 10000) %>% 
ggplot(aes(x = aisle_order_count, y = aisle)) + 
  geom_point()
```

![](Homework-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#create a table showing the most popular items in each of three categories
instacart2 = instacart %>% 
  select(aisle_id, aisle, product_name) %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
count(aisle, product_name) %>% 
group_by(aisle) %>% 
top_n(3) %>% 
  knitr::kable()
```

    ## Selecting by n

``` r
instacart2
```

| aisle                      | product\_name                                 |    n |
| :------------------------- | :-------------------------------------------- | ---: |
| baking ingredients         | Cane Sugar                                    |  336 |
| baking ingredients         | Light Brown Sugar                             |  499 |
| baking ingredients         | Pure Baking Soda                              |  387 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |
| dog food care              | Small Dog Biscuits                            |   26 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |

# Description of the dataframe

  - The size of the dataset instacart is 1384617 rows and 15 columns.
  - Some of the key variables are order\_id and product\_name
  - The most number of orders are placed from the fresh vegetables aisle
    and fresh vegetables aisle.
  - The 3 most popular items ordered from these aisles are:
  - baking ingredients: Light brown sugar (499), pure baking soda (387),
    and cane sugar (336)
  - dog food care: Snack Sticks Chicken & Rice Recipe Dog Treats (30),
    Organix Chicken & Brown Rice Recipe (28), Small Dog Biscuits (26)
  - packaged food vegetables: Organic Baby Spinach (9784), Organic
    Raspberries (5546), Organic Blueberries
(4966)

<!-- end list -->

``` r
#A table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week
icecream_apples = instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  select(order_dow, order_hour_of_day, product_name) %>%
  arrange(desc(order_dow)) %>% 
  group_by(order_dow) %>% 
  summarise(average_order_hour_of_day = mean(order_hour_of_day)) %>% 
  mutate(order_dow = recode(order_dow, `1` = 'Mon', `2` = 'Tues', `3` = 'Wed', `4` = 'Thur', `5` = 'Fri', `6` = 'Sat', `0` = 'Sun')) %>% 
  pivot_wider(names_from = order_dow, values_from = average_order_hour_of_day)
  knitr::kable(icecream_apples)
```

|  Sun |      Mon |     Tues |      Wed |     Thur |      Fri |   Sat |
| ---: | -------: | -------: | -------: | -------: | -------: | ----: |
| 13.6 | 12.17391 | 12.83824 | 14.68519 | 13.17308 | 12.64286 | 13.25 |
