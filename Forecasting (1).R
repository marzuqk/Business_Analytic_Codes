###############
# Forecasting #
###############

# Prepared by Jay Simon
# Last updated on 8/2/2018

# This script is intended for use in ITEC 620: Business Insights through Analytics at American University

# This script provides a demonstration of several time series forecasting techniques in R
# There is also a section at the end showing how to compare and evaluate different forecasting models
# The script uses the USAccDeaths data set. This data set contains monthly accidental deaths in the US from 1973-1978

# This script relies on the HoltWinters function that comes with R
# The forecast & xts packages have additional time series functions, but they are not needed in this course

# Before running any forecasting methods, we need to store our data explicitly as a time series using the ts function

# The following command stores the accidental death data as a time series contained by the variable x
# The start parameter tells R the starting date of the time series
# The freq parameter tells R how many periods there are (in this case, we have monthly data)
x <- ts(USAccDeaths,start=1973,freq=12)

# Note: if the data set has multiple columns, you must specify the name of the column as well
# For instance, if the USAccDeaths data set had separate columns for each state, and we wanted to analyze
# the Maryland column, the command would be:
x <- ts(USAccDeaths$`Maryland`,start=1973,freq=12)
# (Of course, this exact command will produce an error, because the data set does not have a column called Maryland.)

# If there is no start date, omit the "start" parameter, and the points will be numbered starting at 1.
# If the data are not seasonal (i.e. do not have a fixed # of repeating periods), omit the "freq" parameter.

# The following command shows the time series
plot(x)

# The following command fits a single exponential smoothing model to the data, and stores it in a variable called x.SESmodel
# It automatically optimizes the parameters (in this case, alpha is the only parameter used)
x.SESmodel <- HoltWinters(x, beta=FALSE, gamma=FALSE)

# The following command shows the model's output (including parameter values)
x.SESmodel

# The following command shows the time series along with the model
plot(x.SESmodel)

# The following command uses the model to predict the next period
predict(x.SESmodel,1)


# The following command fits a double exponential smoothing model to the data, and stores it in a variable called x.DESmodel
x.DESmodel <- HoltWinters(x, gamma=FALSE)

# The same commands we used for the single exponential smoothing model will still work (replace x.SESmodel with x.DESmodel):
x.DESmodel
plot(x.DESmodel)
predict(x.DESmodel,1)


# The following command fits a Holt-Winters smoothing model to the data, and stores it in a variable called x.HWmodel
x.HWmodel <- HoltWinters(x)

# It uses additive seasonal factors by default; the following command will use multiplicative seasonal factors instead
x.HWmodel.mult <- HoltWinters(x,seasonal="multiplicative")

# The same commands we used for the earlier models will still work, with a couple of modifications
x.HWmodel
plot(x.HWmodel)

# We may want to forecast an entire set of periods, instead of only the next period
predict(x.HWmodel,12)



# It is possible to compare the models informally simply by looking at the plots we've generated
# The remainder of this script demonstrates a more rigorous method

# We will build the models using only a "training" set of data that excludes
# some recent data points (called a "test" set)
# We will then compare their mean squared errors of the predicted values for the test set vs. the actual values in those periods

# First, create the training and test sets.  Here, we're taking the first 4 years (which leaves out the last 2 years of data)
# These three variables state the number of data points in the training and test sets, and the length of each cycle
# Omit the cycle variable if no models with seasonality will be used
train.periods <- 48
test.periods <- 24
cycle <- 12
x.training <- ts(USAccDeaths[1:train.periods],start=1973,freq=cycle)

# Then, build each of the three models using the training set
x.training.SESmodel <- HoltWinters(x.training, beta=FALSE, gamma=FALSE)
x.training.DESmodel <- HoltWinters(x.training, gamma=FALSE)
x.training.HWmodel <- HoltWinters(x.training)

# Because exponential smoothing uses the new data points when making subsequent predictions, we cannot simply compare
# the predictions from these training models to the actual recent data.  Instead, we create models for the full time series
# and force them to take the values of alpha/beta/gamma from the training models.
x.SESmodel <- HoltWinters(x, alpha=x.training.SESmodel$alpha, beta=FALSE, gamma=FALSE)
x.DESmodel <- HoltWinters(x, alpha=x.training.DESmodel$alpha, beta=x.training.DESmodel$beta, gamma=FALSE)
x.HWmodel <- HoltWinters(x, alpha=x.training.HWmodel$alpha, beta=x.training.HWmodel$beta, gamma=x.training.HWmodel$gamma)

# Now we can compute MSE on the most recent two years of data with each model

# First, specify the starting & ending indices of the time series to use
data.start <- train.periods + 1
data.end <- train.periods + test.periods

# Then, for each model, specify the starting & ending indices of the fitted values to use, and compute MSE

fit.start <- train.periods # The fitted points for SES start in period 2, so we want to begin 1 row before the test set starting row number
fit.end <- train.periods + test.periods - 1
SES.MSE <- sum((x.SESmodel$fitted[fit.start:fit.end] - x[data.start:data.end])^2) / (test.periods)

fit.start <- train.periods - 1 # The fitted points for DES start in period 3, so we want to begin 2 rows before the test set starting row number
fit.end <- train.periods + test.periods - 2
DES.MSE <- sum((x.DESmodel$fitted[fit.start:fit.end] - x[data.start:data.end])^2) / (test.periods)

fit.start <- train.periods - cycle + 1 # The fitted points in HW skip the first cycle, so we want to begin 1 cycle's worth of rows before the test set starting row number
fit.end <- train.periods + test.periods - cycle
HW.MSE <- sum((x.HWmodel$fitted[fit.start:fit.end] - x[data.start:data.end])^2) / (test.periods)

cat(paste("SES MSE =",SES.MSE,"\nDES MSE =",DES.MSE,"\nHW MSE =",HW.MSE))

# We can use the same "predict" commands as above to generate forecasts for new periods, and
# the same "plot" commands to view the models along with the actual data
