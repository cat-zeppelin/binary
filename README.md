# binary

## Overview

The package contains a set of functions for discretization and vizualization that may help in developing credit scorecrds in particular and binary classification in general.

## Installation

The package can be installed via `devtools`

``` r
devtools::install_github("cat-zeppelin/binary")
```

## Usage

```r
library(tidyverse)
library(binary)
library(nodots)
library(patchwork)

df_data <- read_csv(url("https://raw.githubusercontent.com/cat-zeppelin/datasets/main/credit-scoring.csv"))

s <- df_data$score
y <- as_logical(df_data$target)
p <- s2p(s)

roc <- ROC(p, y)
ks <- kolmogorov_smirnov(s, y)

plot_density(s) + plot_density(s, y) + plot_roc(roc) + plot_ks(ks)

# Save the plot if need
# ggsave("plot.png", width = 9, height = 8, units = "in")
```

![image](https://user-images.githubusercontent.com/7435943/228504807-d952fc75-814c-4a78-867d-6b3dd0675015.png)
