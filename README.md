
[Package Repository:
Final\_Project\_Group6](https://github.com/AU-R-Programming/Final_Project_Group6)

The Package name is “FinalProjectGroup6”.

Before running our linear regression model, it is necessary to
initialize the two input variables:  
`x`: matrix with number of rows representing the number of predictors
(explanatory) variables  
`y`: vector representing the response variable  

We are using the `bank.csv` dataset as an example.

``` r
library(FinalProjectGroup6)
df <- read.csv2("bank.csv", header = TRUE)
df$job <- as.numeric(as.factor(df$job))
df$marital <- as.numeric(as.factor(df$marital))
df$education <- as.numeric(as.factor(df$education))
df$default <- as.numeric(as.factor(df$default))
df$housing <- as.numeric(as.factor(df$housing))
df$loan <- as.numeric(as.factor(df$loan))
df$contact <- as.numeric(as.factor(df$contact))
df$month <- as.numeric(as.factor(df$month))
df$poutcome <- as.numeric(as.factor(df$poutcome))
df$y <- as.numeric(as.factor(df$y))

# defining the input variables
x <- df[, -which(names(df) == "y")]
x <- x[c("age", "balance", "duration")] #selected 3 predictors
y <- df$y 
```

The next step is to run the model by calling the `our_lm` function. We
further, compare our results to that of the built-in `lm` function. We
can see that the results are very similar.

``` r
model <- our_lm(y, x, alpha = 0.05)
```

    ## Warning in sqrt(var.beta): NaNs produced
    
    ## Warning in sqrt(var.beta): NaNs produced

``` r
model2 <- lm(y ~ x$age + x$balance + df$duration)
model$beta
```

    ## [1] 9.266616e-01 1.342707e-03 2.178674e-06 4.933943e-04

``` r
model2$coefficients
```

    ##  (Intercept)        x$age    x$balance  df$duration 
    ## 9.267811e-01 1.338111e-03 2.185320e-06 4.934805e-04

To obtain the desired outputs by subsetting the model to the desired
output. For example, we can obtain the p-value and F-statistic of the
model by running the following command:

``` r
#p-value
model$p_value
```

    ## [1] 2.207598e-174

``` r
# F-statistic
model$f_stat
```

    ## [1] 294.0769

Plots for the residuals can also be generated.

``` r
# residuals vs fitted-values
res_plot(model$res, model$preds)

# qq-plot
qq_plot(model$res)

# histogram
histogram(model$res)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)![](README_files/figure-gfm/unnamed-chunk-4-2.png)![](README_files/figure-gfm/unnamed-chunk-4-3.png)
