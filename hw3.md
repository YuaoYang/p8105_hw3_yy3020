hw3\_yy3020
================
YuaoYang
2019/10/13

``` r
library(patchwork)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------ tidyverse 1.2.1 --

    ## √ ggplot2 3.2.1     √ purrr   0.3.2
    ## √ tibble  2.1.3     √ dplyr   0.8.3
    ## √ tidyr   1.0.0     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.4.0

    ## -- Conflicts --------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

knitr::opts_chunk$set(
  fig.width = 12,
  fig.asp = 1,
  out.width = "100%"
)

theme_set(theme_bw() + theme(legend.position = "right"))
```

# Problem 1

``` r
data("instacart")
instacart
```

    ## # A tibble: 1,384,617 x 15
    ##    order_id product_id add_to_cart_ord~ reordered user_id eval_set
    ##       <int>      <int>            <int>     <int>   <int> <chr>   
    ##  1        1      49302                1         1  112108 train   
    ##  2        1      11109                2         1  112108 train   
    ##  3        1      10246                3         0  112108 train   
    ##  4        1      49683                4         0  112108 train   
    ##  5        1      43633                5         1  112108 train   
    ##  6        1      13176                6         0  112108 train   
    ##  7        1      47209                7         0  112108 train   
    ##  8        1      22035                8         1  112108 train   
    ##  9       36      39612                1         0   79431 train   
    ## 10       36      19660                2         1   79431 train   
    ## # ... with 1,384,607 more rows, and 9 more variables: order_number <int>,
    ## #   order_dow <int>, order_hour_of_day <int>,
    ## #   days_since_prior_order <int>, product_name <chr>, aisle_id <int>,
    ## #   department_id <int>, aisle <chr>, department <chr>

There are {1384617} observations and the structure of dataset is
(1384617, 15)in this dataset. And the key variables are(order\_id,
product\_id, add\_to\_cart\_order, reordered, user\_id, eval\_set,
order\_number, order\_dow, order\_hour\_of\_day,
days\_since\_prior\_order, product\_name, aisle\_id, department\_id,
aisle, department). The fisrt row means the identifier is {1}, the
product identifier is {49302}, order in which this product was added to
cart is {1}, and this prodcut has been ordered by this user in the past
by user id {112108}, evaluation set this order belongs in {train}, the
order sequence number for this user is {4}, the day of the week on this
order was placed on {4} day, the hour of the day on which the order is
placed on {10}, days since the last order is {9}, the name of this
product is {Bulgarian Yogurt}, the aisle identifier is {120} and name is
{yogurt}which belongs to department {dairy eggs} and the id of this
department is {16}.

``` r
aisle_new = instacart%>%
  group_by(aisle) %>%
  count() %>%
  arrange(n) 
most = pull(aisle_new[134, 1])
```

There are {134} aisles, and the most odered item is {fresh vegetables}.

``` r
 instacart%>%
  group_by(aisle) %>%
  count() %>%
filter(n >10000) %>%
 ggplot(aes(x = n, y =aisle)) +
  geom_point() +
 labs(
   title = "the number of items ordered which is more than 10000",
    x = "Numbers Of Items Odered",
    y = "Aisles"
   ) 
```

<img src="hw3_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />
From the plot, we can find that the fresh vegetablea and fresh fruits
are most odered items.

``` r
instacart %>%
  filter(aisle %in% c('baking ingredients', 'dog food care', 'packaged vegetables fruits')) %>%
select(aisle, product_name) %>%
   group_by(aisle,product_name) %>%  
  count() %>% 
   group_by(aisle)%>%
  filter(min_rank(desc(n))<=3)%>%
  knitr::kable()
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

\#From the table, I find the most ordered items are cane sugar, light
brown sugar and pure baking soda in aisle baking ingredients.The most
ordered items are organix chicken & brown rice recipe, small dog
biscuits and snack sticks chicken \&rice recioe dog treats in aisle dog
food care. The most ordered items are organic baby spinach, organic
blueberries, and organic raspberries in aisle packaged vagetables
fruits.

``` r
 instacart %>%
  filter( product_name  %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
 summarise( means = mean(order_hour_of_day)) %>%
  pivot_wider(id_cols = product_name ,
              names_from = order_dow,
              values_from = means) %>%
    knitr::kable()
```

| product\_name    |        0 |        1 |        2 |        3 |        4 |        5 |        6 |
| :--------------- | -------: | -------: | -------: | -------: | -------: | -------: | -------: |
| Coffee Ice Cream | 13.77419 | 14.31579 | 15.38095 | 15.31818 | 15.21739 | 12.26316 | 13.83333 |
| Pink Lady Apples | 13.44118 | 11.36000 | 11.70213 | 14.25000 | 11.55172 | 12.78431 | 11.93750 |

\#From the table, I find that in Coffee Ice Cream, the mean hour is
concerning on afternoon from 13 to 15, and the mean hour is concerning
on noon from 11 to 14.
