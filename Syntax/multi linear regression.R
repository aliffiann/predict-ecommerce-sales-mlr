# load data
df = read.csv(file.choose(), header=TRUE, sep= ",")
dim(df)

# descriptive statistics
summary(df)

# check missing value
sapply(df, function(x) sum(is.na(x)))

# check outlier
par(mfrow = c(2, 3))
boxplot(df$Avg_Session_Length, main = 'Avg_Session_Length')
boxplot(df$Time_on_App, main = 'Time_on_App')
boxplot(df$Time_on_Website, main = 'Time_on_Website')
boxplot(df$Length_of_Membership, main = 'Length_of_Membership')
boxplot(df$Yearly_Amount_Spent, main = 'Yearly_Amount_Spent')

# remove outlier
outliers <- function(x) {
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

df_new = remove_outliers(df, c('Avg_Session_Length',
                               'Time_on_App',
                               'Time_on_Website',
                               'Length_of_Membership',
                               'Yearly_Amount_Spent'))
dim(df_new)

# train & test split
library(caTools)
set.seed(42)
split = sample.split(df_new$Yearly_Amount_Spent, SplitRatio = 0.7)
train = subset(df_new, split == TRUE)
test = subset(df_new,split == FALSE)


# fitting model
model = lm(Yearly_Amount_Spent
           ~Avg_Session_Length
           +Time_on_App
           +Time_on_Website
           +Length_of_Membership, data = train)
summary(model)

# fitting new model
model_new = lm(Yearly_Amount_Spent
               ~Avg_Session_Length
               +Time_on_App
               +Length_of_Membership, data = train)
summary(model_new)

# normality test
# histogram & plot densitas
par(mfrow = c(1, 1))
hist(model_new$residuals, probability= T, main="Histogram of Residuals")
lines(density(model_new$residuals),col="red")

# normality test kolmogorof-smirnov
library(nortest)
lillie.test(model_new$residuals)  

# autocorrelation test dulbin-waltson
library(lmtest)
dwtest(model_new)

# homoscedasticity test breusch-pagan test
library(lmtest)
bptest(model_new, studentize=FALSE, data=train)

# multicollinearity
library(car)
vif(model_new)

# prediction
prediction = predict(model_new, newdata = test)
result = data.frame(test$Yearly_Amount_Spent, prediction)

# MAPE
library(MLmetrics)
MAPE(y_pred = prediction, y_true = test$Yearly_Amount_Spent)
