setwd("~/Projects/TDDE15-Advanced-machine-learning/lab2")

library(HMM)

set.seed(123123)

# TASK 1

states <- LETTERS[1:10]
symbols <- letters[1:10]

states <- seq(1, 10)
symbols <- seq(1, 10)

## Setting start prob vector
startProbs <- rep(1 / length(states), length(states))

## Building transition prob matrix
transProbs <- diag(0.5, length(states))
for (i in 1:nrow(transProbs)) {
  row = i
  col = ifelse(i == ncol(transProbs), 1, i + 1)
  print(paste(row, ",", col))
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
    print(paste("col:", col, "r", r, "c", c))
    col = col %% length(symbols)
    if (col == 0)
      col = ncol(emissionProbs)
    print(paste(row, ",", col))
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

simulation.result <- simHMM(hmm.model, 100)

simulation.result


# TASK 3

observations <- simulation.result$observation

most.probable.path <- viterbi(hmm.model, observations)


func.filter <- function(alphas) {
  outdata <- c()
  for (r in 1:nrow(alphas)) {
    outdata <- rbind(outdata, alphas[r, ] / sum(alphas[r, ]))
  }
  return(outdata)
}

func.smooth <- function(alphas, betas) {
  outdata <- c()
  for (r in 1:nrow(alphas)) {
    outdata <-
      rbind(outdata, (alphas[r, ] * betas[r, ]) / sum((alphas[r, ] * betas[r, ])))
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
  cbind(filter = prediction.filter,
        smooth = prediction.smooth,
        path = most.probable.path,
        true=simulation.result$states)

func.calc.accuracy <- function(prediction, true){
  confmtx <- table(true=true,prediction=prediction)
  return(sum(diag(confmtx))/sum(confmtx))
}

trueState = simulation.result$states

accuracy.filter <- func.calc.accuracy(prediction.filter,trueState)

accuracy.smooth <- func.calc.accuracy(prediction.smooth ,trueState)

accuracy.path <- func.calc.accuracy(most.probable.path,trueState)

accuracy.filter
accuracy.smooth
accuracy.path


