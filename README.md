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

cm <- confusion_matrix(s, y, desc = FALSE)
roc <- ROC(p, y)
ks <- kolmogorov_smirnov(s, y)
pr <- precision_recall(p, y)

p <- (plot_density(s) | plot_density(s, y) | plot_hit(s, y) | plot_precision_recall(pr)) /
    (plot_roc(roc) | plot_ks(ks) | plot_f1(cm) | plot_mcc(cm))
p

# Save the plot if need
# ggsave("plot.png", p, width = 18, height = 8, units = "in")
```
![plot](https://user-images.githubusercontent.com/7435943/228507251-80c2d40f-b59d-43c1-b908-baf42f2a1068.png)

