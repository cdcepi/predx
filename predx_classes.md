## Point predictions
### __Point__
	A numeric point prediction.
	
	Validity:
  	- Not NA
	

## Continuous distributions
### __Normal__: mean, sd
### __Log-normal__: mean, sd
### __Gamma__: shape, rate
### __Beta__: a, b


## Discrete distributions
### __Binary__: prob
### __Binomial__: p, n
### __Poisson__: mean
### __Negative-Binomial__: r, p
### __Negative-Binomial2__: mean, dispersion


## Empirical distributions
### __BinLwr__
	Binned distribution defined by inclusive lower bounds for each bin.

	A data.frame object with two columns:
	- __lwr__: inclusive numeric lower bounds for sequential bins (equal intervals)
  	- __prob__: probabilities assigned to each bin 

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

	Validity:
	- No NAs in _lwr_ or _prob_
	- Probabilities are positive
	- Probabilities sum to 1.0
	
### __Sample__
	Numeric samples.
  
	Validity:
	- No NAs
	
### __SampleCat__ 
	Character string samples from categories.

	Two vectors:
  	- __cat__: vector of character strings representing each possible outcome category
  	- __sample__: vector of character string samples

	Validity:
	- No NAs in _cat_ or _sample_
	- No duplications in _cat_
	- No _sample_ that is not included in _cat_ (but each _cat_ does not necessarily need to be include in _sample_)
