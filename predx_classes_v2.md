## Continuous
Numerical predictions with continuous real numbers. For all continuous predictions, minima and maxima may be defined as `lower` (default: `-Inf`) and `upper` (default: `Inf`).

### __Point__
A numeric point prediction.

CSV column name: _point_

Validity:
- Numeric
- Not NA
- `lower <= point <= upper`

### __Bin__
Predictions specified as a set of probabilities corresponding to a discrete set of bins across a range of possible numeric outcomes defined by `lower` and `upper`. The specific bins may be specified by a bin __interval__ (generates equally sized bins) or a vector of specific __bins__ defined by the lower bounds of each bin. Either version assumes the lower bound is inclusive and upper bound not inclusive, except for the final bin ending at `upper`. For example, for observable values in `x`, the bins include the probability that observation `x` is greater than or equal to the bin-specific lower bound and less than the bin-specific upper bound: `bin_lwr <= x < bin_upr`, except for at the `upper` bound, where `bin_lwr <= x <= upper`.

__Interval__-defined binned predictions

Bins are defined by bounds and intervals. For example, `Continous(prob = probs, type='Bin', lower = 0, upper = 100, interval = 1)` requires 101 probabilities (`probs`) that cover the bins `0 <= probs[1] < 1`, `1 <= probs[2] < 2`, ... `98 <= probs[1] < 99`, `99 <= probs[101] <= 100`. 

Interval-defined binned predictions are represented internally as a list of:
- `lower`: the lower bound of the range of possible predictions
- `upper`: the upper bound of the range of possible predictions
- `interval`: the span of each bin
- `prob`: the ordered probabilities assigned to each bin

Validity:
- All inputs are numeric
- No NAs
- `lower != -Inf` and `upper != Inf`
- A probability is specified for each bin defined by `lower`, `upper`, and `interval`
- The sum of `prob` is 1.0

__Bin__-defined binned predictions

Bins are defined explicitly by their lower bounds. For example, `Continous(prob = probs, type='Bin', lwr = lwr_bounds)` defines the bins by their lower bounds (`lwr`) and accepts an equal number of probabilities (`probs`), which are associated in order with those bins.

Bin-defined binned predictions are represented internally as a data.frame with two columns:
- `lwr`: inclusive numeric lower bounds for sequential bins (equal intervals)
- `prob`: probabilities assigned to each bin 

Validity:
- All inputs are numeric
- No NAs
- `lower != -Inf`
- `lwr[1] == lower`
- `max(lwr) < upper`
- A probability is specified for each bin (`length(prob) == length(lwr)`)
- The sum of `prob` is 1.0

### __Sample__
Numeric samples for continous outcomes between `lower` and `upper`.

CSV column name: _sample_

Validity:
- Numeric
- No NAs
- `lower <= sample <= upper`

### __Parametric__
Predictions characterized by parametric distributions defined according to base R. Distribution truncation has not been configure, so `upper` and `lower` should not be specified and default to those for the respective distribution.

Parametric predictions are represented internally as a data.frame with 2 columns:
- `parameter_name` with the parameter name (from the set describe below)
- `parameter_value` the corresponding numeric parameter

The following distributions are parameters are currently supported:
__Normal__: `mean`, `sd` (Support: real numbers)
__Log-normal__: `meanlog`, `sdlog` (Support: positive real numbers)
__Gamma__: `shape`, `rate` (or `shape`, `scale`) (Support: positive real numbers)
__Beta__: `shape1`, `shape2` (Support: real numbers in [0, 1])

Validity:
- The supplied parameters names (`parameter_names`) must exactly match those of the specified parametric distribution
- `parameter_values` must be numeric
- `parameter_values` cannot be NA
- `parameter_values` must be appropriate for the specified parametric distribution (e.g. `0 < shape`) 
- `lower` and `upper` must not be user specified or must be equivalent to those of the specified parametric distribution






## Discrete
Quantitative discrete numeric forecasts.

#### __Point__
A numeric point prediction.

CSV column name: _point_

Validity:
- Numeric
- Not NA

## Categorical
### __Point__
A prediction of the most likely categorical outcome.

CSV column name: _point_

Validity:
- String
- Not NA

## Binary
### __Point__
A prediction of the most likely categorical outcome.

CSV column name: _point_

Validity:
- Numeric
- Not NA
- 0 <= value <= 1


## Dates & Times
All dates are formated in ISO standard format: `YYYY-MM-DD`. Forecasts may be specific to a time period, such as a week, month, or year. Those should be consistently defined in the context of the forecast as they are not defined explicitly in the `predx` object.

Times are formatted in ISO standard 24 hour format: 
`YYYY-MM-DDTHH:MM:SS+HH:MM`, where the final HH:MM is the adjustment compared to Coordinated Universal Time (UTC). If the final `:MM` is `:00`, it may be dropped.
Examples:
- 2020-12-18T13:20:37+00:00 is 13:20:37 (1:20 PM with 37 seconds) on 12 December 2020 in UTC (Greenwich Mean Time)
- 2025-01-03T02:00:00-04:00 is 2:00 (2:00 AM) on 3 January 2025 in UTC-04 (Eastern Standard Time). 

### __Point__
A prediction of the most likely date.

CSV column name: _point_

Validity:
- ISO date or time (described above)
- Not NA


## Time
All dates are formated in 

### __Point__
A prediction of the most likely categorical outcome.

CSV column name: _point_

Validity:
- Numeric
- Not NA
- 0 <= value <= 1


## Point predictions

	
### __PointCat__
A character string point prediction, e.g. associated with SampleCat or BinCat.

CSV column name: _point_

Validity:
- Not NA

## Continuous distributions
### __Normal__: mean, sd
### __Log-normal__: mean, sd
### __Gamma__: shape, rate
### __Beta__: a, b


## Discrete distributions
### __Binary__: prob
A numeric probability.

CSV column name: _prob_

Validity:
- Not NA
- 0 <= _prob_ <= 1

### __Binomial__: p, n
### __Poisson__: mean
### __Negative-Binomial__: r, p
### __Negative-Binomial2__: mean, dispersion


## Empirical distributions
### __BinLwr__
Binned distribution defined by inclusive lower bounds for each bin.

A data.frame object with two columns:
- _lwr_: inclusive numeric lower bounds for sequential bins (equal intervals)
- _prob_: probabilities assigned to each bin 

CSV column names: _lwr_, _prob_

Validity:
- No NAs in _lwr_ or _prob_
- Probabilities are positive
- Probabilities sum to 1.0
- Bins are in ascending order
- Bin sizes are uniform
	
### __BinCat__
Binned distribution with a category for each bin. 

A data.frame object with two columns:
- __cat__: character strings representing each possible outcome category
- __prob__: probabilities assigned to each bin

CSV column names: _cat_, _prob_

Validity:
- No NAs in _lwr_ or _prob_
- Probabilities are positive
- Probabilities sum to 1.0
	
### __Sample__
Numeric samples.

CSV column name: _sample_

Validity:
- No NAs
	
### __SampleCat__ 
Character string samples.

CSV column name: _sample_

Validity:
- No NAs

