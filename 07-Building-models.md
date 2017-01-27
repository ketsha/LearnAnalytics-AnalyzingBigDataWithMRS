Building models
================
Seth Mottaghinejad
2017-01-27

Building models
===============

Trying to modeling a given behavior can be a very involved task, as the data itself and business requirements have a say into our choice for a model. Some models have higher predictive power but are less easy to interpret, and others are the other way around. Moreover, the process of building a model can also involves several stages such as choosing among many models then iterating so we can tune the model we've decided upon.

Our next exercise will consist of using several analytics functions offered by `RevoScaleR` to build models for predicting the amount customers tip for a trip. We will use the pick-up and drop-off neighborhoods and the time and day of the trip as the variables most likely to influence tip.

Learning objectives
-------------------

At the end of this chapter, we will have a better understanding of how - to build models with `RevoScaleR` - to understand trade-offs between various models - use visualizations to guide the process of choosing among certain models - to **score** (run predictions on) a dataset with a model we just built

This chapter is not necessarily a thorough guide on running and choosing models. Instead it offers examples with code implementation as a starting point toward that end goal.

A linear model predicting tip percent
-------------------------------------

Let's begin by building a linear model involving two interactive terms: one between `pickup_nb` and `dropoff_nb` and another one between `pickup_dow` and `pickup_hour`. The idea here is that we think trip percent is not just influenced by which neighborhood the passengers was pickup up from, or which neighborhood they were dropped off to, but which neighborhood they were picked up from AND dropped off to. Similarly, we intuit that the day of the week and the hour of the day together influence tipping. For example, just because people tip high on Sundays between 9 and 12, doesn't mean that they tend to tip high any day of the week between 9 and 12 PM, or any time of the day on a Sunday. This intuition is encoded in the model formula argument that we pass to the `rxLinMod` function: `tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour` where we use `:` to separate interactive terms and `+` to separate additive terms.

``` r
form_1 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_1 <- rxLinMod(form_1, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.035 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.042 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.043 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.037 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.038 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.042 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.044 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.042 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.039 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.039 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.038 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.042 seconds 
    ## Computation time: 0.730 seconds.

Examining the model coefficients individually is a daunting task because of how many there are. Moreover, when working with big datasets, a lot of coefficients come out as statistically significant by virtue of large sample size, without necessarily being practically significant. Instead for now we just look at how our predictions are looking. We start by extracting each variable's factor levels into a `list` which we can pass to `expand.grid` to create a dataset with all the possible combinations of the factor levels. We then use `rxPredict` to predict `tip_percent` using the above model.

``` r
rxs <- rxSummary(~pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.022 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.031 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.030 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.032 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.031 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.029 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.032 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.035 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.033 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.032 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.030 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.036 seconds 
    ## Computation time: 0.404 seconds.

``` r
ll <- lapply(rxs$categorical, function(x) x[, 1])
names(ll) <- c("pickup_nb", "dropoff_nb", "pickup_hour", "pickup_dow")
pred_df_1 <- expand.grid(ll)
pred_df_1 <- rxPredict(rxlm_1, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
```

    ## Rows Read: 38416, Total Rows Processed: 38416, Total Chunk Time: 0.108 seconds

``` r
names(pred_df_1)[1:2] <- paste(c("tip_pred", "tip_stderr"), 1, sep = "_")
head(pred_df_1, 10)
```

    ##    tip_pred_1 tip_stderr_1          pickup_nb dropoff_nb pickup_dow
    ## 1    7.585604    0.7724980          Chinatown  Chinatown        Sun
    ## 2   11.652358    0.4108979            Tribeca  Chinatown        Sun
    ## 3   14.799271    0.7357894       Little Italy  Chinatown        Sun
    ## 4   10.838549    0.2804085 Financial District  Chinatown        Sun
    ## 5   12.078835    0.3243162    Lower East Side  Chinatown        Sun
    ## 6   12.681298    0.4418141               Soho  Chinatown        Sun
    ## 7   11.396901    0.5630987       Battery Park  Chinatown        Sun
    ## 8   13.041078    0.3134503  Greenwich Village  Chinatown        Sun
    ## 9   13.974115    0.2971224       East Village  Chinatown        Sun
    ## 10  12.652115    0.5050404       West Village  Chinatown        Sun
    ##    pickup_hour
    ## 1      1AM-5AM
    ## 2      1AM-5AM
    ## 3      1AM-5AM
    ## 4      1AM-5AM
    ## 5      1AM-5AM
    ## 6      1AM-5AM
    ## 7      1AM-5AM
    ## 8      1AM-5AM
    ## 9      1AM-5AM
    ## 10     1AM-5AM

Going over predictions
----------------------

We can now visualize the model's predictions by plotting the average predictions for all combinations of the interactive terms.

``` r
library(ggplot2)
ggplot(pred_df_1, aes(x = pickup_nb, y = dropoff_nb)) + geom_tile(aes(fill = tip_pred_1), 
    colour = "white") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    scale_fill_gradient(low = "white", high = "red") + coord_fixed(ratio = 0.9)
```

![](images/unnamed-chunk-3-1.png)

``` r
ggplot(pred_df_1, aes(x = pickup_dow, y = pickup_hour)) + geom_tile(aes(fill = tip_pred_1), 
    colour = "white") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    scale_fill_gradient(low = "white", high = "red") + coord_fixed(ratio = 0.9)
```

![](images/unnamed-chunk-4-1.png)

Choosing between models
-----------------------

A question we might ask ourselves is how important is the interaction between `pickup_dow` and `pickup_hour` to the predictions? How much worse would the predictions be if we only kept the interaction between `pickup_nb` and `dropoff_nb` and dropped the second interactive term? To answer this, we can build a simpler model with `rxLinMod` in which we only include `pickup_nb:dropoff_nb`. We then predict with the simpler model and use `cbind` to append the new predictions next to the data with the old predictions we made with the more complex model.

``` r
form_2 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb)
rxlm_2 <- rxLinMod(form_2, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.028 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.030 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.027 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.026 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.027 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.024 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.027 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.024 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.026 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.028 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.024 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.025 seconds 
    ## Computation time: 0.445 seconds.

``` r
pred_df_2 <- rxPredict(rxlm_2, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
```

    ## Rows Read: 38416, Total Rows Processed: 38416, Total Chunk Time: 0.091 seconds

``` r
names(pred_df_2)[1:2] <- paste(c("tip_pred", "tip_stderr"), 2, sep = "_")
library(dplyr)
pred_df <- pred_df_2 %>% select(starts_with("tip_")) %>% cbind(pred_df_1) %>% arrange(pickup_nb, 
    dropoff_nb, pickup_dow, pickup_hour) %>% select(pickup_dow, pickup_hour, pickup_nb, 
    dropoff_nb, starts_with("tip_pred_"))
head(pred_df)
```

    ##   pickup_dow pickup_hour pickup_nb dropoff_nb tip_pred_2 tip_pred_1
    ## 1        Sun     1AM-5AM Chinatown  Chinatown   7.581197   7.585604
    ## 2        Sun     5AM-9AM Chinatown  Chinatown   7.581197   6.479020
    ## 3        Sun    9AM-12PM Chinatown  Chinatown   7.581197   6.719867
    ## 4        Sun    12PM-4PM Chinatown  Chinatown   7.581197   6.474056
    ## 5        Sun     4PM-6PM Chinatown  Chinatown   7.581197   6.738151
    ## 6        Sun    6PM-10PM Chinatown  Chinatown   7.581197   7.488318

We can see from the results above that the predictions with the simpler model are identical across all the days of the week and all the hours for the same pick-up and drop-off combination. Whereas the predictions by the more complex model are unique for every combination of all four variables. In other words, adding `pickup_dow:pickup_hour` to the model adds extra variation to the predictions, and what we'd like to know is if this variation contains important signals or if it more or less behaves like noise. To get to the answer, we compare the distribution of the two predictions when we break them up by `pickup_dow` and `pickup_hour`.

``` r
ggplot(data = pred_df) + geom_density(aes(x = tip_pred_1, col = "complex")) + geom_density(aes(x = tip_pred_2, 
    col = "simple")) + facet_grid(pickup_hour ~ pickup_dow)
```

![](images/unnamed-chunk-6-1.png)

The simpler model shows the same distribution all throughout, because these two variables have no effect on its predictions, but the more complex model shows a slightly different distribution for each combination of `pickup_dow` and `pickup_hour`, usually in the form of a slight shift in the distribution. That shift represents the effect of `pickup_dow` and `pickup_hour` at each given combination of the two variables. Because the shift is directional (not haphazard), it's safe to say that it captures some kind of important signal (although its practical significance is still up for debate). We can simplify the above plot if we apply some business logic to it.

Let's us `cut` to bin the tip predictions. To choose what the cut-offs should be, we can use the `rxQuantile` function to guide us.

``` r
dfq <- data.frame(probs = seq(0, 1, by = 0.05))
dfq$tip_percent <- rxQuantile("tip_percent", data = mht_xdf, probs = dfq$probs)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.032 secondsRows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.046 secondsRows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.050 secondsRows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.045 secondsRows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.048 secondsRows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.068 secondsRows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.039 secondsRows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.043 secondsRows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.166 secondsRows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.045 secondsRows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.043 secondsRows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.046 seconds 
    ## Computation time: 0.689 seconds.

``` r
ggplot(aes(x = tip_percent, y = probs), data = dfq) + geom_line()
```

![](images/unnamed-chunk-7-1.png)

Based on the above results, we can bin `tip_percent` by whether they are less than 8%, between 8% and 12%, between 12% and 15%, between 15% and 18%, or 18% or higher. We can then plot a bar plot showing the same information as above, but slightly easier to interpret.

``` r
pred_df %>% mutate_each(funs(cut(., c(-Inf, 8, 12, 15, 18, Inf))), tip_pred_1, tip_pred_2) %>% 
    ggplot() + geom_bar(aes(x = tip_pred_1, fill = "complex"), alpha = 0.5) + geom_bar(aes(x = tip_pred_2, 
    fill = "simple"), alpha = 0.5) + facet_grid(pickup_hour ~ pickup_dow) + xlab("tip percent prediction") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](images/unnamed-chunk-8-1.png)

Based on the above plot, we can see that compared to the simple model, the complex model tends to predict more high-tipping passengers and fewer average-tipping ones during certain day and time combinations (such as Monday through Thursday during the rush hours).

### Exercises

In this section, we will try to improve our predictions. To do so, we can think of selecting "better" algorithms, but "better" is usually subjective as we discussed since every algorithm has its pros and cons and choosing between two algorithm can be a balancing act. However, one thing that any model can benefit from is better features. Better features can mean features that have been pre-processed to suit a particular algorithm, or it can refer to using more inputs in the model.

1.  Let's continue with linear models. Let's build a linear model very similar to the one represented by `form_1`, except that we throw `payment_type` into the mix. Let's call the new formula `form_3`.

``` r
form_3 <- ## formula described above goes here
rxlm_3 <- ## build a linear model based on the above formula
```

We now create a dataset called `pred_df` which will contain all the combinations of the features contained in `form_3`.

``` r
rxs <- rxSummary(~payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, 
    mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[, 1])
names(ll) <- c("payment_type", "pickup_nb", "dropoff_nb", "pickup_hour", "pickup_dow")
pred_df <- expand.grid(ll)
```

1.  Use `rxPredict` to put the predictions from the first model we built, `rxlm_1` and the current model, `rxlm_3` into this dataset. In other words, score `pred_df` with `rxlm_1` and `rxlm_3`. Dump results in a `data.frame` called `pred_all`. Name the predictions `p1` and `p3` respectively. Using the same binning process as before, replace the numeric predictions with the categorical predictions (e.g. `p1` is replaced by `cut(p1, c(-Inf, 8, 12, 15, 18, Inf))`).

2.  Feed `pred_all` to the following code snippet to create a plot comparing the two predictions for different days and times of the day. What is your conclusion based on the plot?

``` r
ggplot(data = pred_all) + geom_bar(aes(x = p1, fill = "model 1", group = payment_type, 
    alpha = 0.5)) + geom_bar(aes(x = p3, fill = "model 3", group = payment_type, 
    alpha = 0.5)) + facet_grid(pickup_hour ~ pickup_dow) + xlab("tip percent prediction") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

### Solutions

As it turns out, `payment_type` is a very important feature for what we're trying to predict. For whatever reason, `tip_amount` does not show up in the data when a customer pays in cash. In other words, as far as the data is concerned, cash-paying customers do not tip. Let's set aside the reason, although it is something that is worth investigating. Knowing this, what we expect to happen is that predictions made by `rxlm_3` will be heavily influenced by `payment_type`.

1.  We begin by building the model and storing it in the model object `rxlm_3`.

``` r
form_3 <- as.formula(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_3 <- rxLinMod(form_3, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.041 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.049 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.046 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.046 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.049 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.044 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.050 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.050 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.055 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.083 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.073 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.065 seconds 
    ## Computation time: 0.955 seconds.

1.  There are different ways of combining the predictions. One approach is to first let's make the predictions and store them in separate datasets and use `cbind` to combine them (as long as the order of the rows doesn't change). We can then use the `mutate_at` function in `dplyr` to apply the binning transformation to the two predictions (we can use `mutate` too but `mutate_at` has a more concise notation).

``` r
rxs <- rxSummary(~payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, 
    mht_xdf)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.036 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.046 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.047 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.048 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.047 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.040 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.037 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.037 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.037 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.040 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.040 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.042 seconds 
    ## Computation time: 0.536 seconds.

``` r
ll <- lapply(rxs$categorical, function(x) x[, 1])
names(ll) <- c("payment_type", "pickup_nb", "dropoff_nb", "pickup_hour", "pickup_dow")
pred_df <- expand.grid(ll)
pred_df_1 <- rxPredict(rxlm_1, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)
```

    ## Rows Read: 76832, Total Rows Processed: 76832, Total Chunk Time: 0.198 seconds

``` r
pred_df_3 <- rxPredict(rxlm_3, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)
```

    ## Rows Read: 76832, Total Rows Processed: 76832, Total Chunk Time: 0.201 seconds

``` r
pred_all <- pred_df %>% cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>% 
    cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>% mutate_at(vars(p1, 
    p3), funs(cut(., c(-Inf, 8, 12, 15, 18, Inf))))
```

1.  Once we have the data ready, we feed it to `ggplot` to create bar plots comparing predictions made by each models broken up by whether the customer paid with a card or using cash.

``` r
ggplot(data = pred_all) + geom_bar(aes(x = p1, fill = "model 1", alpha = 0.5)) + 
    geom_bar(aes(x = p3, fill = "model 3", alpha = 0.5)) + facet_grid(~payment_type) + 
    xlab("tip percent prediction") + theme(axis.text.x = element_text(angle = 90, 
    hjust = 1))
```

![](images/unnamed-chunk-14-1.png)

The model does in fact predict as we expected. It is also possible that are predictions are good, but need to be calibrated. We can recalibrate the predictions by adding a single line of code just before we bin the predictions. To recalibrate the predictions, we use the `rescale` function in the `scales` library. In this case, we are rescaling predictions so that both models predict a number between 0 and 20% tip.

``` r
library(scales)
pred_all <- pred_df %>% cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>% 
    cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>% mutate_at(vars(p1, 
    p3), funs(rescale(., to = c(0, 20)))) %>% mutate_at(vars(p1, p3), funs(cut(., 
    c(-Inf, 8, 12, 15, 18, Inf))))
ggplot(data = pred_all) + geom_bar(aes(x = p1, fill = "model 1"), alpha = 0.5) + 
    geom_bar(aes(x = p3, fill = "model 3"), alpha = 0.5) + facet_grid(~payment_type) + 
    xlab("tip percent prediction") + theme(axis.text.x = element_text(angle = 90, 
    hjust = 1))
```

![](images/unnamed-chunk-15-1.png)

Using other algorithms
----------------------

So far we've only looked at two models from the same `rxLinMod` algorithm. When comparing the two, we looked at the way their predictions capture the effects of the variables used to build each model. To do the comparison, we built a dataset with all combinations of the variables used to build the models with, and then scored that dataset with the two models using `rxPredict`. By doing so we can see how the predictions are distributed, but we still don't know if the predictions are good. The true test of a model's performance is in its ability to predict **out of sample**, which is why we split the data in two and set aside a portion of it for model testing.

To divide the data into training and testing portions, we first used `rxDataStep` to create a new `factor` column called `split` where each row is `"train"` or `"test"` such that a given proportion of the data (here 75 percent) is used to train a model and the rest is used to test the model's predictive power. We then used the `rxSplit` function to divide the data into the two portions. The `rx_split_xdf` function we create here combines the two steps into one and sets some arguments to defaults.

``` r
dir.create(file.path(data_dir, "output"), showWarnings = FALSE)
rx_split_xdf <- function(xdf = mht_xdf, split_perc = 0.75, output_path = "output", 
    ...) {
    # first create a column to split by
    outFile <- tempfile(fileext = "xdf")
    rxDataStep(inData = xdf, outFile = xdf, transforms = list(split = factor(ifelse(rbinom(.rxNumRows, 
        size = 1, prob = splitperc), "train", "test"))), transformObjects = list(splitperc = split_perc), 
        overwrite = TRUE, ...)
    # then split the data in two based on the column we just created
    splitDS <- rxSplit(inData = xdf, outFilesBase = file.path(output_path, "train"), 
        splitByFactor = "split", overwrite = TRUE)
    return(splitDS)
}
# we can now split to data in two
mht_split <- rx_split_xdf(xdf = mht_xdf, varsToKeep = c("payment_type", "fare_amount", 
    "tip_amount", "tip_percent", "pickup_hour", "pickup_dow", "pickup_nb", "dropoff_nb"))
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.809 secondsRows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.741 secondsRows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.687 secondsRows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.725 secondsRows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.709 secondsRows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.615 secondsRows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.607 secondsRows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.600 secondsRows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.607 secondsRows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.619 secondsRows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.603 secondsRows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.611 seconds 
    ## Rows Read: 103130, Total Rows Processed: 103130, Total Chunk Time: 0.320 seconds 
    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 1.065 seconds 
    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 5.588 secondsRows Read: 102423, Total Rows Processed: 102423, Total Chunk Time: 0.275 seconds 
    ## Rows Read: 309592, Total Rows Processed: 309592, Total Chunk Time: 0.842 seconds 
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 4.656 secondsRows Read: 102353, Total Rows Processed: 102353, Total Chunk Time: 0.285 seconds 
    ## Rows Read: 307967, Total Rows Processed: 307967, Total Chunk Time: 0.930 seconds 
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 4.902 secondsRows Read: 102143, Total Rows Processed: 102143, Total Chunk Time: 0.269 seconds 
    ## Rows Read: 308297, Total Rows Processed: 308297, Total Chunk Time: 0.831 seconds 
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 4.376 secondsRows Read: 103896, Total Rows Processed: 103896, Total Chunk Time: 0.278 seconds 
    ## Rows Read: 310367, Total Rows Processed: 310367, Total Chunk Time: 0.846 seconds 
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 4.424 secondsRows Read: 103551, Total Rows Processed: 103551, Total Chunk Time: 0.274 seconds 
    ## Rows Read: 310425, Total Rows Processed: 310425, Total Chunk Time: 0.848 seconds 
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 4.316 secondsRows Read: 103252, Total Rows Processed: 103252, Total Chunk Time: 0.304 seconds 
    ## Rows Read: 310704, Total Rows Processed: 310704, Total Chunk Time: 0.865 seconds 
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 4.898 secondsRows Read: 103611, Total Rows Processed: 103611, Total Chunk Time: 0.278 seconds 
    ## Rows Read: 311068, Total Rows Processed: 311068, Total Chunk Time: 0.918 seconds 
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 4.588 secondsRows Read: 104485, Total Rows Processed: 104485, Total Chunk Time: 0.277 seconds 
    ## Rows Read: 313823, Total Rows Processed: 313823, Total Chunk Time: 0.975 seconds 
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 4.690 secondsRows Read: 105176, Total Rows Processed: 105176, Total Chunk Time: 0.283 seconds 
    ## Rows Read: 313472, Total Rows Processed: 313472, Total Chunk Time: 0.845 seconds 
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 4.605 secondsRows Read: 104372, Total Rows Processed: 104372, Total Chunk Time: 0.278 seconds 
    ## Rows Read: 311178, Total Rows Processed: 311178, Total Chunk Time: 0.911 seconds 
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 4.980 secondsRows Read: 103984, Total Rows Processed: 103984, Total Chunk Time: 0.449 seconds 
    ## Rows Read: 311491, Total Rows Processed: 311491, Total Chunk Time: 1.172 seconds 
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 5.366 seconds

``` r
names(mht_split) <- c("train", "test")
```

We now run three different algorithms on the data:

-   `rxLinMod`, the linear model from earlier with the terms `tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour`
-   `rxDTree`, the decision tree algorithm with the terms `tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + pickup_hour` (decision trees don't need interactive factors because interactions are built into the algorithm itself)
-   `rxDForest`, the random forest algorithm with the same terms as decision trees Since this is not a modeling course, we will not discuss how the algorithms are implemented. Instead we run the algorithms and use them to predict tip percent on the test data so we can see which one works better.

``` r
system.time(linmod <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, 
    data = mht_split$train, reportProgress = 0))
```

    ##    user  system elapsed 
    ##    0.02    0.01    0.49

``` r
system.time(dtree <- rxDTree(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + 
    pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0))
```

    ##    user  system elapsed 
    ##    0.03    0.00   87.11

``` r
system.time(dforest <- rxDForest(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + 
    pickup_hour, data = mht_split$train, nTree = 30, importance = TRUE, useSparseCube = TRUE, 
    reportProgress = 0))
```

    ##    user  system elapsed 
    ##    0.03    0.02  247.45

Since running the above algorithms can take a while, it may be worth saving the models that each return.

``` r
trained.models <- list(linmod = linmod, dtree = dtree, dforest = dforest)
save(trained.models, file = "trained_models.Rdata")
```

Comparing predictions
---------------------

Before applying the algorithm to the test data, let's apply it to the small dataset with all the combinations of categorical variables and visualize the predictions. This might help us develop some intuition about each algorithm.

``` r
pred_df <- expand.grid(ll[2:5])
pred_df_1 <- rxPredict(trained.models$linmod, data = pred_df, predVarNames = "pred_linmod")
```

    ## Rows Read: 38416, Total Rows Processed: 38416, Total Chunk Time: 0.007 seconds

``` r
pred_df_2 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
```

    ## Rows Read: 38416, Total Rows Processed: 38416, Total Chunk Time: 0.040 seconds

``` r
pred_df_3 <- rxPredict(trained.models$dforest, data = pred_df, predVarNames = "pred_dforest")
```

    ## Rows Read: 38416, Total Rows Processed: 38416, Total Chunk Time: 2.666 seconds

``` r
pred_df <- do.call(cbind, list(pred_df, pred_df_1, pred_df_2, pred_df_3))
observed_df <- rxSummary(tip_percent ~ pickup_nb:dropoff_nb:pickup_dow:pickup_hour, 
    mht_xdf)
```

    ## Rows Read: 412183, Total Rows Processed: 412183, Total Chunk Time: 0.031 seconds
    ## Rows Read: 412015, Total Rows Processed: 824198, Total Chunk Time: 0.059 seconds
    ## Rows Read: 410320, Total Rows Processed: 1234518, Total Chunk Time: 0.070 seconds
    ## Rows Read: 410440, Total Rows Processed: 1644958, Total Chunk Time: 0.072 seconds
    ## Rows Read: 414263, Total Rows Processed: 2059221, Total Chunk Time: 0.061 seconds
    ## Rows Read: 413976, Total Rows Processed: 2473197, Total Chunk Time: 0.066 seconds
    ## Rows Read: 413956, Total Rows Processed: 2887153, Total Chunk Time: 0.069 seconds
    ## Rows Read: 414679, Total Rows Processed: 3301832, Total Chunk Time: 0.074 seconds
    ## Rows Read: 418308, Total Rows Processed: 3720140, Total Chunk Time: 0.067 seconds
    ## Rows Read: 418648, Total Rows Processed: 4138788, Total Chunk Time: 0.066 seconds
    ## Rows Read: 415550, Total Rows Processed: 4554338, Total Chunk Time: 0.063 seconds
    ## Rows Read: 415475, Total Rows Processed: 4969813, Total Chunk Time: 0.062 seconds 
    ## Computation time: 0.890 seconds.

``` r
observed_df <- observed_df$categorical[[1]][, c(2:6)]
pred_df <- inner_join(pred_df, observed_df, by = names(pred_df)[1:4])
ggplot(data = pred_df) + geom_density(aes(x = Means, col = "observed average"), size = 1) + 
    geom_density(aes(x = pred_linmod, col = "linmod"), size = 1) + geom_density(aes(x = pred_dtree, 
    col = "dtree"), size = 1) + geom_density(aes(x = pred_dforest, col = "dforest"), 
    size = 1) + xlim(-1, 30) + xlab("tip percent")
```

![](images/unnamed-chunk-19-1.png)

Both the linear model and the random forest give us smooth predictions. We can see that the random forest predictions are the most concentrated. The predictions for the decision tree follow a jagged distribution, probably as a result of overfitting, but we don't know that until we check performance against the test set.

Comparing predictive performance
--------------------------------

We now apply the model to the test data so we can compare the predictive power of each model. If we are correct about the decision tree over-fitting, then we should see it preform poorly on the test data compared to the other two models. If we believe the random forest captures some inherent signals in the data that the linear model misses, we should see it perform better than the linear model on the test data.

The first metric we look at is the average of the squared residuals, which gives us an idea of how close the predictions are to the observed values. Since we're predicting tip percent, which usually falls in a narrow range of 0 to about 20 percent, we should expect on average the residuals for a good model to be no more than 2 or 3 percentage points.

``` r
rxPredict(trained.models$linmod, data = mht_split$test, outData = mht_split$test, 
    predVarNames = "tip_percent_pred_linmod", overwrite = TRUE)
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.028 seconds
    ## Rows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 0.115 seconds
    ## Rows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 0.117 seconds
    ## Rows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 0.150 seconds
    ## Rows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 0.172 seconds
    ## Rows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 0.176 seconds
    ## Rows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 0.202 seconds
    ## Rows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 0.207 seconds
    ## Rows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 0.168 seconds
    ## Rows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 0.144 seconds
    ## Rows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 0.131 seconds
    ## Rows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 0.111 seconds

``` r
rxPredict(trained.models$dtree, data = mht_split$test, outData = mht_split$test, 
    predVarNames = "tip_percent_pred_dtree", overwrite = TRUE)
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.351 seconds
    ## Rows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 0.836 seconds
    ## Rows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 0.899 seconds
    ## Rows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 0.833 seconds
    ## Rows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 0.877 seconds
    ## Rows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 0.848 seconds
    ## Rows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 0.805 seconds
    ## Rows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 0.946 seconds
    ## Rows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 0.861 seconds
    ## Rows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 0.870 seconds
    ## Rows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 0.905 seconds
    ## Rows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 0.871 seconds

``` r
rxPredict(trained.models$dforest, data = mht_split$test, outData = mht_split$test, 
    predVarNames = "tip_percent_pred_dforest", overwrite = TRUE)
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.351 seconds
    ## Rows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 44.908 seconds
    ## Rows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 34.017 seconds
    ## Rows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 33.498 seconds
    ## Rows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 31.823 seconds
    ## Rows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 32.358 seconds
    ## Rows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 30.875 seconds
    ## Rows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 32.455 seconds
    ## Rows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 31.428 seconds
    ## Rows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 33.858 seconds
    ## Rows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 30.872 seconds
    ## Rows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 30.889 seconds

``` r
rxSummary(~SSE_linmod + SSE_dtree + SSE_dforest, data = mht_split$test, transforms = list(SSE_linmod = (tip_percent - 
    tip_percent_pred_linmod)^2, SSE_dtree = (tip_percent - tip_percent_pred_dtree)^2, 
    SSE_dforest = (tip_percent - tip_percent_pred_dforest)^2))
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.089 secondsRows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 0.094 secondsRows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 0.093 secondsRows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 0.094 secondsRows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 0.120 secondsRows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 0.101 secondsRows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 0.094 secondsRows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 0.093 secondsRows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 0.097 secondsRows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 0.100 secondsRows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 0.095 secondsRows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 0.098 seconds 
    ## Computation time: 1.223 seconds.

    ## Call:
    ## rxSummary(formula = ~SSE_linmod + SSE_dtree + SSE_dforest, data = mht_split$test, 
    ##     transforms = list(SSE_linmod = (tip_percent - tip_percent_pred_linmod)^2, 
    ##         SSE_dtree = (tip_percent - tip_percent_pred_dtree)^2, 
    ##         SSE_dforest = (tip_percent - tip_percent_pred_dforest)^2))
    ## 
    ## Summary Statistics Results for: ~SSE_linmod + SSE_dtree +
    ##     SSE_dforest
    ## Data: mht_split$test (RxXdfData Data Source)
    ## File name:
    ##     C:\Users\sethmott\OneDrive\Documents\courses\LearnAnalytics-AnalyzingBigDataWithMRS\output\train.split.train.xdf
    ## Number of valid observations: 3727437 
    ##  
    ##  Name        Mean     StdDev   Min          Max      ValidObs MissingObs
    ##  SSE_linmod  139.2002 176.1228 7.099748e-30 7552.816 3724524  2913      
    ##  SSE_dtree   139.1963 176.5068 1.928081e-06 7459.282 3724524  2913      
    ##  SSE_dforest 138.8575 175.1327 6.685260e-10 7316.892 3724524  2913

Another metric worth looking at is a correlation matrix. This can help us determine to what extent the predictions from the different models are close to each other, and to what extent each is close to the actual or observed tip percent.

``` r
rxc <- rxCor(~tip_percent + tip_percent_pred_linmod + tip_percent_pred_dtree + tip_percent_pred_dforest, 
    data = mht_split$test)
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.043 seconds
    ## Rows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 0.043 seconds
    ## Rows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 0.045 seconds
    ## Rows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 0.045 seconds
    ## Rows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 0.042 seconds
    ## Rows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 0.047 seconds
    ## Rows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 0.044 seconds
    ## Rows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 0.045 seconds
    ## Rows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 0.044 seconds
    ## Rows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 0.044 seconds
    ## Rows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 0.043 seconds
    ## Rows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 0.043 seconds 
    ## Computation time: 0.537 seconds.

``` r
print(rxc)
```

    ##                          tip_percent tip_percent_pred_linmod
    ## tip_percent                1.0000000               0.1357460
    ## tip_percent_pred_linmod    0.1357460               1.0000000
    ## tip_percent_pred_dtree     0.1357740               0.8410099
    ## tip_percent_pred_dforest   0.1472373               0.9110847
    ##                          tip_percent_pred_dtree tip_percent_pred_dforest
    ## tip_percent                           0.1357740                0.1472373
    ## tip_percent_pred_linmod               0.8410099                0.9110847
    ## tip_percent_pred_dtree                1.0000000                0.9261610
    ## tip_percent_pred_dforest              0.9261610                1.0000000

### Exercises

The models we built in the last section were disappointing, and the last exercise showed us that it's because those models ignored `payment_type`, which is strongly correlated with `tip_percent` since customers who pay cash for their taxi fare do not have their tips registered by the data.

1.  Now that we used `RevoScaleR` to split the data into training and testing sets, let's go back and rebuild the three models from last section but include `payment_type` this time, in addition to all the features we used last time.

``` r
# we re-build the linear model without payment_type for comparison
linmod_1 <- rxLinMod(## formula goes here
                     data = mht_split$train, reportProgress = 0)
# we build a linear model with payment_type
linmod_2 <- rxLinMod(## formula goes here
                     data = mht_split$train, reportProgress = 0)
# we build a decision tree with payment_type
dtree <- rxDTree(## formula goes here
                 data = mht_split$train, pruneCp = "auto", reportProgress = 0)

trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)
```

1.  Here is a code to build a prediction dataset called `pred_df`. Fill in the code for predicting `payment_type` using the three models from above and attach the predictions to `pred_df` as separate columns called `pred_linmod_1`, `pred_linmod_2` and `pred_dtree`.

``` r
pred_df <- expand.grid(ll)
## predict payment_type on pred_df by each of three models pred_df_1 stores
## predictions made by linmod_1 into a column called pred_linmod_1 pred_df_2
## stores predictions made by linmod_2 into a column called pred_linmod_2
## pred_df_3 stores predictions made by dtree into a column called pred_dtree
pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
head(pred_df)
```

1.  Run your dataset against the following code snippet to produce a plot similar to the plot we examined earlier. What is different about the plot this time?

``` r
ggplot(data = pred_df) + geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) + 
    geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) + geom_density(aes(x = pred_dtree, 
    col = "dtree"), size = 1) + xlim(-10, 40) + xlab("tip percent") + facet_grid(payment_type ~ 
    ., scales = "free")
```

1.  Extract only the necessary columns from the test data and run predictions on it. In the last section, we put the predictions directly in the test data's XDF file, but because this is an IO intensive operation, we can run it faster by using a `data.frame` instead (as long as the test data is not big enough to make us run out of memory). Use `rxPredict` to run score `test_df` with the predictions from each of the three models.

``` r
test_df <- rxDataStep(mht_split$test, 
  varsToKeep = c('tip_percent', 'payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow'), 
  maxRowsByCols = 10^9)
test_df_1 <- ## predict for the first model, call the predictions `tip_pred_linmod`
test_df_2 <- ## predict for the second model, call the predictions `tip_pred_dtree`
test_df_3 <- ## predict for the third model, call the predictions `tip_pred_dforest`
```

We can now attach models' predictions to the `test_df` and run a `rxSummary` (or any R function we like, since `test_df` is a `data.frame`) to look at the performance of the models.

``` r
test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))
rxSummary(~SSE_linmod + SSE_dtree + SSE_dforest, data = test_df, transforms = list(SSE_linmod = (tip_percent - 
    tip_pred_linmod)^2, SSE_dtree = (tip_percent - tip_pred_dtree)^2, SSE_dforest = (tip_percent - 
    tip_pred_dforest)^2))
```

1.  Run the above summary and report your findings.

### Solutions

1.  We begin by building the three models on the training data and saving the results in an `Rdata` file.

``` r
linmod_1 <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, 
    data = mht_split$train, reportProgress = 0)
linmod_2 <- rxLinMod(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour, 
    data = mht_split$train, reportProgress = 0)
dtree <- rxDTree(tip_percent ~ payment_type + pickup_nb + dropoff_nb + pickup_dow + 
    pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0)
trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)
```

1.  We can now build a prediction dataset with all the combinations of the input variables. If any of the input variables was numeric, we would have to discretize it so the dataset does not blow up in size. In our case, we don't have numeric inputs. Finally, with `do.call` we can recursively join the predictions to the original data using the `cbind` function.

``` r
pred_df <- expand.grid(ll)
pred_df_1 <- rxPredict(trained.models$linmod_1, data = pred_df, predVarNames = "pred_linmod_1")
```

    ## Rows Read: 76832, Total Rows Processed: 76832, Total Chunk Time: 0.007 seconds

``` r
pred_df_2 <- rxPredict(trained.models$linmod_2, data = pred_df, predVarNames = "pred_linmod_2")
```

    ## Rows Read: 76832, Total Rows Processed: 76832, Total Chunk Time: 0.009 seconds

``` r
pred_df_3 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
```

    ## Rows Read: 76832, Total Rows Processed: 76832, Total Chunk Time: 0.051 seconds

``` r
pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
head(pred_df)
```

    ##   payment_type    pickup_nb dropoff_nb pickup_hour pickup_dow
    ## 1         card    Chinatown  Chinatown     1AM-5AM        Sun
    ## 2         cash    Chinatown  Chinatown     1AM-5AM        Sun
    ## 3         card      Tribeca  Chinatown     1AM-5AM        Sun
    ## 4         cash      Tribeca  Chinatown     1AM-5AM        Sun
    ## 5         card Little Italy  Chinatown     1AM-5AM        Sun
    ## 6         cash Little Italy  Chinatown     1AM-5AM        Sun
    ##   pred_linmod_1 pred_linmod_2   pred_dtree
    ## 1      7.218613   20.88122443 2.147128e+01
    ## 2      7.218613   -0.49123761 2.409766e-04
    ## 3     11.424330   21.29615123 2.179141e+01
    ## 4     11.424330   -0.07631081 2.409766e-04
    ## 5     12.920763   21.91703579 2.179141e+01
    ## 6     12.920763    0.54457375 2.409766e-04

1.  We now feed the above data to `ggplot` to look at the distribution of the predictions made by each model. It should come as no surprise that with the inclusion of `payment_type` the predictions have a **bimodal distribution** one for trips paid in cash and one for trips paid using a card. For trips paid in cash, the actual distribution is not as important, but for trips paid using a card we can see that the random forest model makes predictions that are less spread out than the other two models.

``` r
ggplot(data = pred_df) + geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) + 
    geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) + geom_density(aes(x = pred_dtree, 
    col = "dtree"), size = 1) + xlim(-10, 40) + xlab("tip percent") + facet_grid(payment_type ~ 
    ., scales = "free")
```

![](images/unnamed-chunk-29-1.png)

1.  We now run the predictions on the test data to evaluate each model's performance. To do so, we use `rxPredict` without the `outData` argument and make an assignment on the left side so results would go into a `data.frame`.

``` r
test_df <- rxDataStep(mht_split$test, varsToKeep = c("tip_percent", "payment_type", 
    "pickup_nb", "dropoff_nb", "pickup_hour", "pickup_dow"), maxRowsByCols = 10^9)
```

    ## Rows Read: 309053, Total Rows Processed: 309053, Total Chunk Time: 0.120 seconds
    ## Rows Read: 309592, Total Rows Processed: 618645, Total Chunk Time: 0.113 seconds
    ## Rows Read: 307967, Total Rows Processed: 926612, Total Chunk Time: 0.111 seconds
    ## Rows Read: 308297, Total Rows Processed: 1234909, Total Chunk Time: 0.113 seconds
    ## Rows Read: 310367, Total Rows Processed: 1545276, Total Chunk Time: 0.172 seconds
    ## Rows Read: 310425, Total Rows Processed: 1855701, Total Chunk Time: 0.134 seconds
    ## Rows Read: 310704, Total Rows Processed: 2166405, Total Chunk Time: 0.126 seconds
    ## Rows Read: 311068, Total Rows Processed: 2477473, Total Chunk Time: 0.114 seconds
    ## Rows Read: 313823, Total Rows Processed: 2791296, Total Chunk Time: 0.116 seconds
    ## Rows Read: 313472, Total Rows Processed: 3104768, Total Chunk Time: 0.154 seconds
    ## Rows Read: 311178, Total Rows Processed: 3415946, Total Chunk Time: 0.125 seconds
    ## Rows Read: 311491, Total Rows Processed: 3727437, Total Chunk Time: 0.125 seconds

``` r
test_df_1 <- rxPredict(trained.models$linmod_1, data = test_df, predVarNames = "tip_pred_linmod_1")
```

    ## Rows Read: 3727437, Total Rows Processed: 3727437, Total Chunk Time: 0.292 seconds

``` r
test_df_2 <- rxPredict(trained.models$linmod_2, data = test_df, predVarNames = "tip_pred_linmod_2")
```

    ## Rows Read: 3727437, Total Rows Processed: 3727437, Total Chunk Time: 0.399 seconds

``` r
test_df_3 <- rxPredict(trained.models$dtree, data = test_df, predVarNames = "tip_pred_dtree")
```

    ## Rows Read: 3727437, Total Rows Processed: 3727437, Total Chunk Time: 4.545 seconds

``` r
test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))
head(test_df)
```

    ##   tip_percent payment_type        pickup_nb       dropoff_nb pickup_hour
    ## 1          29         card      Murray Hill         Gramercy    6PM-10PM
    ## 2           0         cash  Upper West Side Garment District     5AM-9AM
    ## 3          35         card  Upper West Side    Carnegie Hill    6PM-10PM
    ## 4          22         card     West Village  Lower East Side    10PM-1AM
    ## 5           0         cash         Gramercy     West Village    12PM-4PM
    ## 6          34         card Garment District          Midtown     1AM-5AM
    ##   pickup_dow tip_pred_linmod_1 tip_pred_linmod_2 tip_pred_dtree
    ## 1        Tue          17.75716        23.3761078   2.319917e+01
    ## 2        Wed          13.50980        -1.2928009   2.409766e-04
    ## 3        Sun          14.07954        21.2211192   2.201043e+01
    ## 4        Fri          14.23836        19.4645582   1.872195e+01
    ## 5        Tue          15.12583        -0.6026712   2.409766e-04
    ## 6        Fri          11.42987        22.3534984   2.233461e+01

Since we have the test data in a `data.frame` we can also plot the distribution of the predictions on the test data to compare it with the last plot. As we can see, the random forest and linear model both probably waste some computation effort making predictions for trips paid in cash.

``` r
ggplot(data = test_df) + geom_density(aes(x = tip_pred_linmod_1, col = "linmod w/o payment type")) + 
    geom_density(aes(x = tip_pred_linmod_2, col = "linmod w/ payment type")) + geom_density(aes(x = tip_pred_dtree, 
    col = "dtree with payment type")) + xlab("tip percent")  # + facet_grid(pickup_hour ~ pickup_dow)
```

![](images/unnamed-chunk-31-1.png)

1.  Recall from the last section that the predictions made by the three models had an average SSE of about 80. With the inclusion of `payment_type` we should see a considerable drop in this number.

``` r
rxSummary(~SSE_linmod_1 + SSE_linmod_2 + SSE_dtree, data = test_df, transforms = list(SSE_linmod_1 = (tip_percent - 
    tip_pred_linmod_1)^2, SSE_linmod_2 = (tip_percent - tip_pred_linmod_2)^2, SSE_dtree = (tip_percent - 
    tip_pred_dtree)^2))
```

    ## Rows Read: 3727437, Total Rows Processed: 3727437, Total Chunk Time: 1.124 seconds 
    ## Computation time: 1.149 seconds.

    ## Call:
    ## rxSummary(formula = ~SSE_linmod_1 + SSE_linmod_2 + SSE_dtree, 
    ##     data = test_df, transforms = list(SSE_linmod_1 = (tip_percent - 
    ##         tip_pred_linmod_1)^2, SSE_linmod_2 = (tip_percent - tip_pred_linmod_2)^2, 
    ##         SSE_dtree = (tip_percent - tip_pred_dtree)^2))
    ## 
    ## Summary Statistics Results for: ~SSE_linmod_1 + SSE_linmod_2 +
    ##     SSE_dtree
    ## Data: test_df
    ## Number of valid observations: 3727437 
    ##  
    ##  Name         Mean      StdDev   Min          Max      ValidObs MissingObs
    ##  SSE_linmod_1 139.20016 176.1228 7.099748e-30 7552.816 3724524   2913     
    ##  SSE_linmod_2  40.16044 135.6578 3.356909e-12 6510.418 3709825  17612     
    ##  SSE_dtree     41.91945 139.0436 5.806974e-08 6629.801 3724524   2913

``` r
rxc <- rxCor(~tip_percent + tip_pred_linmod_1 + tip_pred_linmod_2 + tip_pred_dtree, 
    data = test_df)
```

    ## Rows Read: 3727437, Total Rows Processed: 3727437, Total Chunk Time: 0.068 seconds 
    ## Computation time: 0.070 seconds.

``` r
print(rxc)
```

    ##                   tip_percent tip_pred_linmod_1 tip_pred_linmod_2
    ## tip_percent         1.0000000         0.1353449         0.8463832
    ## tip_pred_linmod_1   0.1353449         1.0000000         0.1621350
    ## tip_pred_linmod_2   0.8463832         0.1621350         1.0000000
    ## tip_pred_dtree      0.8458855         0.1553302         0.9946482
    ##                   tip_pred_dtree
    ## tip_percent            0.8458855
    ## tip_pred_linmod_1      0.1553302
    ## tip_pred_linmod_2      0.9946482
    ## tip_pred_dtree         1.0000000

The average SSE has now dropped to a little over 20, which confirms how the inclusion of the right features (`payment_type` in this case) can have a significant impact on our model's predictive power.

Final thoughts
--------------

An analytics or machine learning workflow is not a linear process. As we saw in some of the examples we provided, discovering certain things about our data or getting disappointing results from our models can force us to go back to square one and think about how we can improve our data. This can mean getting more data, getting better data, or very probably both. Getting more data doesn't always help, since we saw that even not-so-large samples can do a good job of representing the overall population. However, getting more data can be especially helpful if we need to model very unusual behaviors (sometimes called rare events) using a lot of features. Getting better data can refer to two things: 1. Using new features in the hope that they will reduce noise in our models. A *new* feature here can be a new source of data, or a feature that was derived directly from the existing feature set. As we saw in the last exercise, this approach can considerably reduce noise. However, obtaining or engineering new features is not always easy, as it may require a lot of domain knowledge about the business and data sources. 2. Doing a better job of cleaning and transforming our existing feature set before we use them to build a model. In this case, what we mean by *cleaning and transforming* depends somewhat on the model we want to build. Therefore, using this approach requires a certain amount of machine learning expertise.

With its parallel data processing and analytics capabilities, `RevoScaleR` can help a data scientist to reduce the time spent on iterating and improving models, and once ready, models can be deployed to a production environment with relative ease. This allows the data scientist to keep their focus on the analytics and modeling and spend less time being side-tracked into the hurdles faced in distributed computing environments.
