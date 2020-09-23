setwd("~/Projects/TDDE15-Advanced-machine-learning/lab2")

library(HMM)
library(entropy)

# TASK 1

states <- seq(1, 10)
symbols <- seq(1, 10)

## Setting start prob vector
startProbs <- rep(1 / length(states), length(states))

## Building transition prob matrix
transProbs <- diag(0.5, length(states))
for (i in 1:nrow(transProbs)) {
  row = i
  col = ifelse(i == ncol(transProbs), 1, i + 1)
  transProbs[row, col] = 0.5
}
transProbs

## Building emission prob matrix
emission.interval <- seq(from = -2, to = 2, by = 1)
emissionProbs <-
  diag(1 / length(emission.interval), length(symbols))

for (r in 1:nrow(emissionProbs)) {
  row = r
  
  for (c in emission.interval) {
    col = r + c
    col = col %% length(symbols)
    if (col == 0)
      col = ncol(emissionProbs)
    emissionProbs[row, col] = (1 / length(emission.interval))
  }
}
emissionProbs

## Creating HMM model

hmm.model <-
  initHMM(
    States = states,
    Symbols = symbols,
    startProbs = startProbs,
    transProbs = transProbs,
    emissionProbs = emissionProbs
  )

hmm.model


# TASK 2
## Simulating steps from model created in task 1
set.seed(123123)

simulation.result <- simHMM(hmm.model, 100)

simulation.result


# TASK 3

observations <- simulation.result$observation

most.probable.path <- viterbi(hmm.model, observations)


func.filter <- function(alphas) {
  outdata <- c()
  for (c in 1:ncol(alphas)) {
    outdata <- cbind(outdata, alphas[,c ] / sum(alphas[,c ]))
  }
  return(outdata)
}

func.smooth <- function(alphas, betas) {
  outdata <- c()
  for (c in 1:ncol(alphas)) {
    outdata <-
      cbind(outdata, (alphas[,c ] * betas[,c ]) / sum((alphas[,c ] * betas[,c ])))
  }
  return(outdata)
}

log.forward.probabilities <- forward(hmm.model, observations)

forward.probabilities <- exp(log.forward.probabilities)

log.backward.probabilities <- backward(hmm.model, observations)

backward.probabilities <- exp(log.backward.probabilities)

filtered.prob <- func.filter(forward.probabilities)

smooth.prob <-
  func.smooth(forward.probabilities, backward.probabilities)

# TASK 4

## Creating predictions for each dist. in TASK 3

prediction.filter <- apply(t(filtered.prob), MARGIN = 1, which.max)
prediction.smooth <- apply(t(smooth.prob), MARGIN = 1, which.max)

comparision <-
  cbind(
    filter = prediction.filter,
    smooth = prediction.smooth,
    path = most.probable.path,
    true = simulation.result$states
  )

func.calc.accuracy <- function(prediction, true) {
  confmtx <- table(true = true, prediction = prediction)
  return(sum(diag(confmtx)) / sum(confmtx))
}

trueState = simulation.result$states

accuracy.filter <- func.calc.accuracy(prediction.filter, trueState)

accuracy.smooth <- func.calc.accuracy(prediction.smooth , trueState)

accuracy.path <- func.calc.accuracy(most.probable.path, trueState)

accuracy.filter
accuracy.smooth
accuracy.path


# TASK 5

## Different seed
set.seed(2127)

simulation.result <- simHMM(hmm.model, 100)

simulation.result

observations <- simulation.result$observation

most.probable.path <- viterbi(hmm.model, observations)


log.forward.probabilities <- forward(hmm.model, observations)

forward.probabilities <- exp(log.forward.probabilities)

log.backward.probabilities <- backward(hmm.model, observations)

backward.probabilities <- exp(log.backward.probabilities)

filtered.prob <- func.filter(forward.probabilities)

smooth.prob <-
  func.smooth(forward.probabilities, backward.probabilities)


prediction.filter <- apply(t(filtered.prob), MARGIN = 1, which.max)
prediction.smooth <- apply(t(smooth.prob), MARGIN = 1, which.max)

trueState = simulation.result$states

accuracy.filter.2 <-
  func.calc.accuracy(prediction.filter, trueState)

accuracy.smooth.2 <-
  func.calc.accuracy(prediction.smooth , trueState)

accuracy.path.2 <- func.calc.accuracy(most.probable.path, trueState)

## Comparing rounds of accuracy

print("First round")
accuracy.filter
accuracy.smooth
accuracy.path
print("Second round")
accuracy.filter.2
accuracy.smooth.2
accuracy.path.2

# TASK 6

set.seed(2127)

simulation.result.more <- simHMM(hmm.model, 300)

simulation.result.more

observations.more <- simulation.result.more$observation

most.probable.path.more <- viterbi(hmm.model, observations.more)


log.forward.probabilities.more <-
  forward(hmm.model, observations.more)

forward.probabilities.more <- exp(log.forward.probabilities.more)

log.backward.probabilities.more <-
  backward(hmm.model, observations.more)

backward.probabilities.more <- exp(log.backward.probabilities.more)

filtered.prob.more <- func.filter(forward.probabilities.more)

smooth.prob.more <-
  func.smooth(forward.probabilities.more, backward.probabilities.more)


prediction.filter.more <-
  apply(t(filtered.prob.more), MARGIN = 1, which.max)
prediction.smooth.more <-
  apply(t(smooth.prob.more), MARGIN = 1, which.max)

trueState.more = simulation.result.more$states

accuracy.filter.more <-
  func.calc.accuracy(prediction.filter.more, trueState.more)

accuracy.smooth.more <-
  func.calc.accuracy(prediction.smooth.more , trueState.more)

accuracy.path.more <-
  func.calc.accuracy(most.probable.path.more, trueState.more)

## Comparing entropy
entropy.empirical(filtered.prob)
entropy.empirical(smooth.prob)
entropy.empirical(filtered.prob.more)
entropy.empirical(smooth.prob.more)
## Answer: No, more observations does not always lead to better estimates of where the robot is.

# TASK 7

## "Taking" step 101
step.101 <-  filtered.prob[,100]  %*% transProbs

print("Probabilities of the hidden states for the time step 101")
t(step.101)
