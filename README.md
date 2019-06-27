# Text classification

## Documentation:
https://bsmulski.shinyapps.io/documentation/

## Files:
- scripts/exploration.R - first look at the data
- scripts/model.R - data preparation, model building and prediction
- scripts/documentation/documentation.rmd - the main file on which the documentation is based
 
## Results:
- results/prediction.rda - data frame with predicted observtions
- results/model - trained h2o model

To load the model please type:
```r
library(h2o)
h2o.init()
model <- h2o.loadModel("results/model")
```
