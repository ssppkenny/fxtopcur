# This is a currency conversion package

### Purpose
It is sometimes interesting to know the historical conversion rates.
It is possible using this packаге.

### Installation
* install devtools (install.packages("devtools"))
* devtools::install_github("ssppkenny/fxtopcur")

### Usage

```r
library(fxtopcur)
library(ggplot2)
df <- fetch_cur_data(cur_from="CHF", cur_to="RUB", years=5)
df %>% ggplot(aes(x=day, y=value)) + geom_line()
```


![Plotted with ggplot](https://github.com/ssppkenny/fxtopcur/blob/master/Rplot.png)


