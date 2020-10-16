# Covariance function
# Input:
#   X: Observations
#   XStar: Observations
#   sigmaF: Standard deviation of f
#   l: Smoothness factor

KerlenSquaredExp <- function(X, XStar, sigmaF, l) {
  X.len <- length(X)
  XStar.len <- length(XStar)
  kValues <- matrix(NA, X.len, XStar.len)
  
  for (i in 1:XStar.len) {
    kValues[, i] <- sigmaF ^ 2 * exp(-0.5 * ((X - XStar[i]) / l) ^ 2)
  }
  return(kValues)
}



# Function posteriorGP

## Inputs
### X: Vector of training inputs.
### y: Vector of training targets/outputs.
### XStar: Vector of inputs where the posterior distribution is evaluated, i.e. X_*.
### sigmaNoise: Noise standard deviation sigma_n.
### k: Covariance function or kernel.

posteriorGP <- function(X, y, XStar, sigmaNoise, k, ...) {
  kValues <- KerlenSquaredExp(X, X, ...)
  kStarValues <- KerlenSquaredExp(X, XStar, ...)
  I <- diag(length(diag(kStarValues)))
  L <- t(chol(kValues + (sigmaNoise ^ 2 * I)))
  alpha <- solve(a = t(L), b = solve(a = L, b = y))
  fStar <- t(kStarValues) %*% alpha
  v <- solve(a = L, b = kStarValues)
  vfStart <-
    diag(KerlenSquaredExp(XStar, XStar, ...) - (t(v) %*% v))
  logMarginalLikelihood <-
    (-1 / 2) * t(y) %*% alpha - sum(log(L)) - (length(X) / 2) * log(2 * pi)
  
  return(
    list(
      mean = fStar,
      variance = vfStart,
      logMarginalLikelihood = logMarginalLikelihood
    )
  )
}

# task 2.1.2

sigmaF <- 1
l = 0.3
sigmaNoise <- 0.1

xGrid <- seq(-1, 1, length.out = 200)

Obs <- data.frame(x = c(0.4), y = c(0.719))

fPosterior <-
  posteriorGP(
    X = Obs$x ,
    y = Obs$y ,
    XStar = xGrid,
    sigmaNoise = sigmaNoise,
    sigmaF = sigmaF,
    l = l
  )

CI <-
  data.frame(
    upper = fPosterior$mean + 1.96 * sqrt(fPosterior$variance),
    lower = fPosterior$mean - 1.96 * sqrt(fPosterior$variance)
  )

plot(
  x = xGrid,
  y = fPosterior$mean,
  type = 'l',
  col = 'red',
  ylab = 'Posterior mean',
  xlab = 'xGrid',
  ylim = c(min(CI$lower), max(CI$upper)),
  main = paste("sigmaF =", sigmaF, "| l =", l)
)



points(xGrid, CI$upper , col = "blue")
points(xGrid, CI$lower , col = "blue")
points(Obs, col = "green", pch = 16)

legend(
  'topleft',
  legend = c('Mean of f', 'CI' , 'Observation'),
  col = c('red', 'blue', 'green'),
  lty = c(1, NA, NA),
  pch = c(NA, 1, 16)
)


# task 2.1.3

sigmaF <- 1
l = 0.3
sigmaNoise <- 0.1

xGrid <- seq(-1, 1, length.out = 200)

Obs <- data.frame(x = c(0.4,-0.6), y = c(0.719,-0.044))

fPosterior <-
  posteriorGP(
    X = Obs$x ,
    y = Obs$y ,
    XStar = xGrid,
    sigmaNoise = sigmaNoise,
    sigmaF = sigmaF,
    l = l
  )

CI <-
  data.frame(
    upper = fPosterior$mean + 1.96 * sqrt(fPosterior$variance),
    lower = fPosterior$mean - 1.96 * sqrt(fPosterior$variance)
  )

plot(
  x = xGrid,
  y = fPosterior$mean,
  type = 'l',
  col = 'red',
  ylab = 'Posterior mean',
  xlab = 'xGrid',
  ylim = c(min(CI$lower), max(CI$upper)),
  main = paste("sigmaF =", sigmaF, "| l =", l)
)



points(xGrid, CI$upper , col = "blue")
points(xGrid, CI$lower , col = "blue")
points(Obs, col = "green", pch = 16)

legend(
  'topleft',
  legend = c('Mean of f', 'CI' , 'Observation'),
  col = c('red', 'blue', 'green'),
  lty = c(1, NA, NA),
  pch = c(NA, 1, 16)
)

# task 2.1.4

sigmaF <- 1
l = 0.3
sigmaNoise <- 0.1

xGrid <- seq(-1, 1, length.out = 50)

Obs <- data.frame(
  x = c(-1.0, -0.6, -0.2, 0.4, 0.8),
  y = c(0.768, -0.044, 0.940, 0.719, -0.664)
)

fPosterior <-
  posteriorGP(
    X = Obs$x ,
    y = Obs$y ,
    XStar = xGrid,
    sigmaNoise = sigmaNoise,
    sigmaF = sigmaF,
    l = l
  )

CI <-
  data.frame(
    upper = fPosterior$mean + 1.96 * sqrt(fPosterior$variance),
    lower = fPosterior$mean - 1.96 * sqrt(fPosterior$variance)
  )

plot(
  x = xGrid,
  y = fPosterior$mean,
  type = 'l',
  col = 'red',
  ylab = 'Posterior mean',
  xlab = 'xGrid',
  ylim = c(min(CI$lower) - 0.2, max(CI$upper) + 0.2),
  main = paste("sigmaF =", sigmaF, "| l =", l)
)



points(xGrid, CI$upper , col = "blue")
points(xGrid, CI$lower , col = "blue")
points(Obs, col = "green", pch = 16)

legend(
  'topleft',
  legend = c('Mean of f', 'CI' , 'Observation'),
  col = c('red', 'blue', 'green'),
  lty = c(1, NA, NA),
  pch = c(NA, 1, 16)
)

# task 2.1.5


sigmaF <- 1
l = 1
sigmaNoise <- 0.1

xGrid <- seq(-1, 1, length.out = 50)

Obs <- data.frame(
  x = c(-1.0, -0.6, -0.2, 0.4, 0.8),
  y = c(0.768, -0.044, 0.940, 0.719, -0.664)
)

fPosterior <-
  posteriorGP(
    X = Obs$x ,
    y = Obs$y ,
    XStar = xGrid,
    sigmaNoise = sigmaNoise,
    sigmaF = sigmaF,
    l = l
  )

CI <-
  data.frame(
    upper = fPosterior$mean + 1.96 * sqrt(fPosterior$variance),
    lower = fPosterior$mean - 1.96 * sqrt(fPosterior$variance)
  )

plot(
  x = xGrid,
  y = fPosterior$mean,
  type = 'l',
  col = 'red',
  ylab = 'Posterior mean',
  xlab = 'xGrid',
  ylim = c(min(CI$lower) - 0.2, max(CI$upper) + 0.2),
  main = paste("sigmaF =", sigmaF, "| l =", l)
)



points(xGrid, CI$upper , col = "blue")
points(xGrid, CI$lower , col = "blue")
points(Obs, col = "green", pch = 16)

legend(
  'bottomleft',
  legend = c('Mean of f', 'CI' , 'Observation'),
  col = c('red', 'blue', 'green'),
  lty = c(1, NA, NA),
  pch = c(NA, 1, 16)
)

# Part 2.2
library(kernlab)

preset_KerlenSquaredExp <- function(sigmaF, l) {
  KerlenSquaredExp <- function(X, XStar = NULL) {
    X.len <- length(X)
    XStar.len <- length(XStar)
    kValues <- matrix(NA, X.len, XStar.len)
    
    for (i in 1:XStar.len) {
      kValues[, i] <- sigmaF ^ 2 * exp(-0.5 * ((X - XStar[i]) / l) ^ 2)
    }
    return(kValues)
  }
  
  class(KerlenSquaredExp) <- 'kernel'
  return(KerlenSquaredExp)
}

## getting data and doing setup
data <-
  read.csv(
    "https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv",
    header = TRUE,
    sep = ";"
  )
time <- seq(1, nrow(data), 1)
day <-
  c(seq(1, 365),
    seq(1, 365),
    seq(1, 365),
    seq(1, 365),
    seq(1, 365),
    seq(1, 365))
data$time <- time
data$day <- day

sampleIndexes <- seq(1, nrow(data), 5)

dataSample <- data[sampleIndexes,]

# 2.2.1

ownKernel <- preset_KerlenSquaredExp(sigmaF = 1, l = 0.3)

X <- c(1, 3, 4)
xStar <- c(2, 3, 4)

covMatrix <- kernelMatrix(x = X,
                          y = xStar,
                          kernel = ownKernel)

print(covMatrix)

# 2.2.2

simpleModel <- lm(dataSample$temp ~ poly(dataSample$time, 2))
sigmaNoise <- sd(simpleModel$residuals)

sigmaF <- 20
l <- 0.2

ownKernel <- preset_KerlenSquaredExp(sigmaF = sigmaF, l = l)

# Fit GP to observed data
GP.fit <- gausspr(
  x = dataSample$time,
  y = dataSample$temp,
  kernel = preset_KerlenSquaredExp,
  kpar = list(sigmaF = sigmaF, l = l),
  var = sigmaNoise ^ 2
)

# Predict using fitted GP
posteriorMean <- predict(GP.fit, dataSample$time)

plot(
  dataSample$time,
  dataSample$temp,
  main = paste("sigmaF =", sigmaF, "| l =", l) ,
  ylab = "Mean of f"
)
lines(dataSample$time, posteriorMean, col = "red")


# 2.2.3

x <- dataSample$time
xs <- dataSample$time # XStar.
n <- length(x)
Kss <- kernelMatrix(kernel = ownKernel, x = xs, y = xs)
Kxx <- kernelMatrix(kernel = ownKernel, x = x, y = x)
Kxs <- kernelMatrix(kernel = ownKernel, x = x, y = xs)
Covf = Kss - t(Kxs) %*% solve(Kxx + sigmaNoise ^ 2 * diag(n), Kxs) # Covariance matrix of fStar.

CI <- data.frame(upper = posteriorMean + 1.96 * sqrt(diag(Covf)),
                 lower = posteriorMean - 1.96 * sqrt(diag(Covf)))

plot(
  dataSample$time,
  dataSample$temp,
  main = paste("sigmaF =", sigmaF, "| l =", l),
  ylim = c(min(CI$lower), max(CI$upper)),
  ylab = "Mean of f"
)
lines(dataSample$time,
      posteriorMean,
      col = "red",
      lwd = 2)

# Probability intervals for fStar.
lines(xs, CI$lower, col = "blue")
lines(xs, CI$upper, col = "blue")

# 2.2.4

simpleModel <- lm(dataSample$temp ~ poly(dataSample$day, 2))
sigmaNoise <- sd(simpleModel$residuals)

sigmaF <- 20
l <- 0.2

ownKernel <- preset_KerlenSquaredExp(sigmaF = sigmaF, l = l)

# Fit GP to observed data
GP.fit <- gausspr(
  x = dataSample$day,
  y = dataSample$temp,
  kernel = preset_KerlenSquaredExp,
  kpar = list(sigmaF = sigmaF, l = l),
  var = sigmaNoise ^ 2
)

# Predict using fitted GP
posteriorMean.day <- predict(GP.fit, dataSample$day)

plot(
  dataSample$time,
  dataSample$temp,
  main = "Comparison between time and day as x variable",
  lwd = 0.2,
  ylab = "Mean of f"
)
lines(dataSample$time, posteriorMean, col = "red")
lines(dataSample$time, posteriorMean.day, col = "blue")
legend(
  'bottomright',
  legend = c('Mean of f(time)', 'Mean of f(day)'),
  col = c('red', 'blue'),
  lty = c(1, 1),
  pch = c(NA, NA)
)


# 2.2.5

generalizationPeriodicKernel <- function(sigmaF, l_1, l_2, d) {
  kernelFunc <- function (X, XStar = NULL) {
    absDist <- abs(X - XStar)
    return ((sigmaF ^ 2) * exp(-2 * (sin(pi * absDist / d) ^ 2) / (l_1 ^
                                                                     2)) * exp(-(1 / 2) * (absDist / l_2) ^ 2))
  }
  
  class(kernelFunc) <- 'kernel'
  return (kernelFunc)
}

simpleModel <- lm(dataSample$temp ~ poly(dataSample$time, 2))
sigmaNoise <- sd(simpleModel$residuals)

sigmaF <- 20
l_1 <- 1
l_2 <- 10
d <- 365 / sd(dataSample$time)

GP.fit <- gausspr(
  x = dataSample$time,
  y = dataSample$temp,
  kernel = generalizationPeriodicKernel,
  kpar = list(
    sigmaF = sigmaF,
    l_1 = l_1,
    l_2 = l_2,
    d = d
  ),
  var = sigmaNoise ^ 2
)

posteriorMean.general <- predict(GP.fit, dataSample$time)

plot(
  dataSample$time,
  dataSample$temp,
  main = "Comparison between GP model predictions",
  lwd = 0.2,
  ylab = "Mean of f"
)
lines(dataSample$time, posteriorMean, col = "red")
lines(dataSample$time, posteriorMean.day, col = "blue")
lines(dataSample$time, posteriorMean.general, col = "green")
legend(
  'bottomright',
  legend = c(
    'Mean of f(time)',
    'Mean of f(day)',
    'Mean of f(time) generalizationPeriodicKernel'
  ),
  col = c('red', 'blue', 'green'),
  lty = c(1, 1, 1),
  pch = c(NA, NA, NA)
)


# Part 2.3

## Downloading data and setup
library(kernlab)
library(AtmRay)
data <-
  read.csv(
    "https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv",
    header = FALSE,
    sep = ","
  )
names(data) <-
  c("varWave", "skewWave", "kurtWave", "entropyWave", "fraud")
data[, 5] <- as.factor(data[, 5])

## Traing and test data
set.seed(111)
SelectTraining <-
  sample(1:dim(data)[1], size = 1000, replace = FALSE)
train <- data[SelectTraining, ]
test <- data[-SelectTraining, ]

# 2.3.1

GP.fit <- gausspr(fraud ~ varWave + skewWave, data = train)

xGrid1 <- seq(
  from = min(train$varWave),
  to = max(train$varWave),
  length = 100
)
xGrid2 <- seq(
  from = min(train$skewWave),
  to = max(train$skewWave),
  length = 100
)

gridPoints <- meshgrid(xGrid1, xGrid2)
gridPoints <- cbind(c(gridPoints$x), c(gridPoints$y))

gridPoints <- data.frame(gridPoints)
names(gridPoints) <- c("varWave", "skewWave")
probPreds <- predict(GP.fit, gridPoints, type = "probabilities")



contour(
  x = xGrid1,
  y = xGrid2,
  z = matrix(probPreds[, 2], 100, byrow = TRUE),
  20,
  xlab = "varWave",
  ylab = "skewWave",
  main = 'Probability of fraud'
)

casesOfFraud <- which(train$fraud==1)

points(x = train$varWave[casesOfFraud], y = train$skewWave[casesOfFraud], col = 'blue')
points(x = train$varWave[-casesOfFraud], y = train$skewWave[-casesOfFraud], col = 'red')

train.pred <- predict(GP.fit, train, type = "probabilities")
classifiedFraud <- ifelse(train.pred[,2]>0.5,1,0)

confmtx <- table(predictions=classifiedFraud,true=train$fraud)
acc <- sum(diag(confmtx))/sum(confmtx)
print(confmtx)
print(acc)

# 2.3.2

test.pred <- predict(GP.fit, test, type = "probabilities")
classifiedFraud <- ifelse(test[,2]>0.5,1,0)

confmtx <- table(predictions=classifiedFraud,true=test$fraud)
acc <- sum(diag(confmtx))/sum(confmtx)
print(confmtx)
print(acc)

# 2.3.3

GP.fit.all <- gausspr(fraud ~ ., data = train)

test.pred <- predict(GP.fit.all, test, type = "probabilities")
classifiedFraud <- ifelse(test.pred[,2]>0.5,1,0)

confmtx <- table(predictions=classifiedFraud,true=test$fraud)
acc <- sum(diag(confmtx))/sum(confmtx)
print(confmtx)
print(acc)
