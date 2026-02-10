# fatherStay

## Interface Wishlist

The task of nowcasting can be separated into the steps detailed below.

For user-facing functions, we take the following principles:

- Consistent interface. 
	- Functions will take arguments in the same order, and arguments will gave consistent names.
	- Function outputs will have the same structure regardless of the model / imputation strategy.
- Composable functions.
	- The output of one function can be piped into another.
	- Functions that take a "data" argument will accept either the original data (in which case the formula must be specified) or will accept the output of a previous function (which includes the formula, and thus it does not need specified again).
- Sensible defaults, with messages if defaults are used somewhere that the user would likely want to change something.
	- The package will guess the correct input when it is unambiguous (e.g., if only one column is a date-time type, it will be used as the dates).
- Detailed documentation.
	- All parameters will be defined. All possible input values will be stated and described in detail.
	- Inspection of the "usage" field in the help file should be all you need in 90% of cases.
- Extensible.
	- The user can write their own modelling or imputation methods that will work within our interface.


### Single Function Approach

It would be absolutely fantastic if the user could simply:

```r
m1 <- nowcast(
	formula = y ~ x,
	date_col = "collection_date",
	data = my_df,
	model = "lm",
	impute = "linear",
	enbpi = list(reps = 200, batch_size = 50),
	level = 0.95
)
m2 <- new_model(m1, model = "ar", params = list(p = 1))

m3 <- new_formula(m1, formula = y ~ lag(x, 1))
m4 <- new_model(m3, model = "ar", params = list(p = 1))

m5 <- ensemble(list(m1, m2, m3, m4), strategy = "average")

# Comparison metrics only work if formula base variables are the same
compare(m1, m2, m3, m4, m5)
autoplot(m1, m2, m3, m4, m5) # Plot predictions, including imputations
```

# The process below will probably be deprecated

This process was developed under the assumption that the user would want multiple different methods for imputation and a suite of models. Instead, I'd rather have the user see a familiar lm-like interfact and a couple helper functions for comparisons/model tweaking. I want to minimize the number of functions that users need to know about.

### 1. Load and clean the data

Since we do not have access to the true data, this is left to the discretion of the user.

We expect the data to have the following properties:

- A single data frame (or tibble), which includes columns for 
    - DAD (the response), 
    - CNISP, PTSOS, and RVDSS (the covariates), 
    - date (as a datetime, e.g. from `lubridate::ymd()`)
    - (Note that the column names could be anything; a formula will communicate the names to the functions).
- The covariates may have missing values, which will be imputed.
- The last date for which at least one of the covariates are available is the date to be nowcasted.
- The response is NA for the period to be nowcasted.
	- All NA values in the response up until the last observed covariate value will be nowcasted. The user does not need to specify a date range.

We will implement a check function to ensure these are correctly specified:

```r
check_data(data, formula = NULL, date_col = NULL)
```

The result will be information about the missingness of the data and ensure there's a datetime column in a valid format.

The formula will tell the function which variable is the response so we can provide better information.



### 2. Perform imputation and display the results

There are several possible imputation strategies that we will implement, so this is split as a separate function.

```r
imputed_data <- data |>
	impute(formula, date_col = NULL, method = "full_linear")
```

Alternatively the composability allows for the following:

```r
imputed_data <- check_data(data, formula = y ~ x, date_col = "date") |>
	impute(method = "full_linear")
```

There will be an `autoplot()` method for imputed data, which shows the raw data and the predictions in a `ggplot2` object..

```r
autoplot(imputed_data) +
	theme_bw()
```

### 3. Fit the model(s)

The fitting function will take in the data and output a model object.

```r
fitted <- fit_model(data, formula, date_col, model = "lm", cv_strategy = "basic")

fitted <- check_data(data, formula, date_col) |>
	impute_data(method = "full_linear") |>
	fit_model(model = "lm", cv_strategy = "basic")
```

Ideally, the `method` argument will also accept a user-defined function that has compatible inputs/outputs with our approach.

The autoplot method will show relevant diagnostics depending on the model.

```r
autoplot(fitted)
```

There will also be a way to compare multiple fitted models for training set performance.

### 4. Nowcast, with prediction intervals

```r
nowcasted <- nowcast(data, formula, date_col, model, measure, cv_strategy)


nowcasted <- check_data(data, formula, date_col) |>
	impute_data(method = "full_linear") |>
	fit_model(model = "lm", measure = "rmse", cv_strategy = "basic") |>
	nowcast()

autoplot(nowcasted)
```

## Dream Features

Imagine if this was the entire workflow:

```r
model_one <- nowcast(
	formula = DAD ~ RVDSS + CNISP, data = mydata, method = "lm"
)
model_two <- nowcast(
	formula = DAD = CNISP, data = mydata, method = "lm"
)

models <- compare_models(model_one, model_two)
summary(models)
autoplot(models)

# Pie-in-the-sky hopes and dreams
multimodel <- nowcast(
	formula = list(
		"test_pos" = DAD ~ RVDSS,
		"transfers" = DAD ~ CNISP + PTSOS,
		"saturated" = DAD ~ RVDSS + CNISP + PTSOS,
		# AR and KF models will ignore formulae with lags in them
		"lag_transfers" = DAD ~ lag(CNISP, 2) + lag(PTSOS, 3),
		"poly_pos" = DAD ~ poly(RVDSS, 3)
	),
	data = mydata,
	date_col = "date_col", # If NULL, guess?
	method = list(
		"lm", "arx", "armax", "gam", "randomForest", "kalmanFilter"
	),
	params = list(
		"arx" = list("order" = c(1, 2)),
		"gam" = list("smooths" = list(c("CNISP", "PTSOS"), c("RVDSS"))),
		"randomForest" = list("n_trees" = c(100, 500))
	),
	eval = c("absolute", "relative")
)

autoplot(multimodel) +
	labs(
		title = "Comparison of model fits",
		subtitle = "All combinations of imputation method and model (with params)."
	)

best_nowcast <- multimodel |>
	extract_best(measure = "rmse") |>
	nowcast()

nowcast_ensemble <- nowcast_ensemble(multimodel, weights = NULL)
```

What a world that would be!



