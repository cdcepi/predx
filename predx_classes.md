# Point forecasts
- __Point__: point


# Continuous distributions
- __Normal__: mean, sd
- __Log-normal__: mean, sd
- __Gamma__: shape, rate
- __Beta__: a, b


# Discrete distributions
- __Binary__: prob
- __Binomial__: p, n
- __Poisson__: mean
- __Negative-Binomial__: r, p
- __Negative-Binomial2__: mean, dispersion


# Empirical distributions
- __BinLwr__: Binned distribution defined by inclusive lower bounds for each bin. A data.frame object with two columns:
  - __lwr__: inclusive lower bounds for sequential bins (numeric, equal intervals)
  - __prob__: probability assigned to each bin 
  - Validity:
	- No NAs in _lwr_ or _prob_
	- Probabilities are positive
	- Probabilities sum to 1.0
	- Bins are in ascending order
	- Bin sizes are uniform
- __BinCat__: Binned distribution with a category for each bin. A data.frame object with two columns:
  - __cat__: TBD
  - __prob__: probability assigned to each bin 
  - Validity:
    - TBD
- __Sample__: Numeric samples from a distribution 
- __SampleCat__: String samples from categories

