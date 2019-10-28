## Point predictions
### __Point__
A numeric point prediction.

CSV column name: _point_

Validity:
- Not NA
	
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

