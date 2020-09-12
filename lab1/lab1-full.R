library(bnlearn)
library(Rgraphviz)
library(gRain)

source("./helper-functions.R")

### TASK 1 ###
# Show that multiple runs of the hill-climbing algorithm
# can return non-equivalent Bayesian network (BN) structures.
# Explain why this happens.


data("asia")

# Testing different random restarts
set.seed(567)
bn.hc.5 <- hc(x = asia, restart = 5)

set.seed(567)
bn.hc.10 <- hc(x = asia, restart = 10)

set.seed(567)
bn.hc.1 <- hc(x = asia, restart = 1)

set.seed(567)
bn.hc.100 <- hc(x = asia, restart = 100)

plot(bn.hc.10, main="Random restarts: 10")
plot(bn.hc.5, main="Random restarts: 5")
plot(bn.hc.1, main="Random restarts: 1")
plot(bn.hc.100, main="Random restarts: 100")

print(all.equal(bn.hc.10,bn.hc.5))
print(all.equal(bn.hc.10,bn.hc.1))
print(all.equal(bn.hc.10,bn.hc.100))
print(all.equal(bn.hc.5,bn.hc.1))
print(all.equal(bn.hc.5,bn.hc.100))
print(all.equal(bn.hc.1,bn.hc.100))

# Testing with different initial structures
nodes <- colnames(asia)
init <- empty.graph(nodes)
print(init)

arc.set.1 = matrix(c("A", "S", "X", "D", "E", "X", "X", "T"),
                   ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("from", "to")))
print(arc.set.1)

arc.set.2 = matrix(c("B", "D", "X", "D"),
                   ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("from", "to")))
print(arc.set.2)

arc.set.3 = matrix(c("S", "A"),
                   ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("from", "to")))
print(arc.set.3)


set.seed(234)
bn.hc.arc.0 <- hc(x = asia)

set.seed(234)
arcs(init) = arc.set.1
print(init)
bn.hc.arc.1 <- hc(x = asia, start=init)

set.seed(234)
arcs(init) = arc.set.2
print(init)
bn.hc.arc.2 <- hc(x = asia, start=init)

set.seed(234)
arcs(init) = arc.set.3
print(init)
bn.hc.arc.3 <- hc(x = asia, start=init)

plot(bn.hc.arc.0, main="arc.set 0")
plot(bn.hc.arc.1, main="arc.set 1")
plot(bn.hc.arc.2, main="arc.set 2")
plot(bn.hc.arc.3, main="arc.set 3")

## Finding: If the initial structure just has one or two directed arces the hc process seem to
## fallback to the same final graph

# Testing different scoring methodes
score.methodes = c("bic","loglik","aic","mbde")

set.seed(173)
bn.hc.s.1 <- hc(x = asia, score = score.methodes[1])

set.seed(173)
bn.hc.s.2 <- hc(x = asia, score = score.methodes[2])

set.seed(173)
bn.hc.s.3 <- hc(x = asia, score = score.methodes[3])

set.seed(173)
bn.hc.s.4 <- hc(x = asia, score = score.methodes[4])

plot(bn.hc.s.1, main= paste("Scoring method:",score.methodes[1]))
plot(bn.hc.s.2, main= paste("Scoring method:",score.methodes[2]))
plot(bn.hc.s.3, main= paste("Scoring method:",score.methodes[3]))
plot(bn.hc.s.4, main= paste("Scoring method:",score.methodes[4]))

## Finding: This method resulted in the larges differences between each final graph.

# Conclusion: The hill-climbing algorithm starts at a random point in the data and from there walks

### TASK 2 ###
# Learn a BN from 80 % of the Asia dataset.
# Use the BN learned to classify the remaining 20 % of
# the Asia dataset in two classes: S = yes and S = no.
# Use exact or approximate inference with the help of the bnlearn and gRain packages,
# you are not allowed to use functions such as predict.
# Report the confusion matrix; true/false positives/negatives.

set.seed(2020)

# Splitting data 80/20 train/test
n=nrow(asia)
id=sample(1:n,floor(n*0.8))
train=asia[id,]
test=asia[-id,]

# Learning the structure of the BN.
bn.structure <- hc(x = train, restart = 15, score = "bic")
plot(bn.structure, main="Learned structure")
# Learning the parameters.
bn.fitted <- bn.fit(x = bn.structure, data = train, method = "bayes")

# Setting the true structure of the BN
bn.true.structure = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
plot(bn.true.structure, main="True structure")
# Learning the parameters using the true structure.
bn.true.fitted <- bn.fit(x = bn.true.structure, data = train, method = "bayes")

# Converting to grain obj. to use in compile.grain.
bn.fitted.grain <- as.grain(bn.fitted)
bn.true.fitted.grain <- as.grain(bn.true.fitted)


# Now creating junction trees using compile.grain.
# The trees will be queried to make predictions for S.
junction.tree.own <- compile(bn.fitted.grain)
junction.tree.true <- compile(bn.true.fitted.grain)

# Making predictions using helper-function (separate file)
prediction.own <- predictfunction(junction.tree.own,test,"S")
prediction.true <- predictfunction(junction.tree.true,test,"S")

# Comparing predictions with true values
conf.own <- make.confusion.matrix(test[,"S"],prediction.own)
conf.true <- make.confusion.matrix(test[,"S"],prediction.true)

# Output
conf.own
conf.true

all.equal(conf.own,conf.true) # -> The two are essentially the same

### TASK 3 ###
# In the previous exercise, S was classified using observations for all the rest of the variables.
# S should be classified by using only observations for the so-called Markov blanket of S,
# i.e. its parents plus its children plus the parents of its children minus S itself.
# Report again the confusion matrix.

# Learning the structure of the BN.
bn.structure <- hc(x = train, restart = 15, score = "bic")
plot(bn.structure)
# Learning the parameters.
bn.fitted <- bn.fit(x = bn.structure, data = train, method = "bayes")

# Setting the true structure of the BN
bn.true.structure = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
plot(bn.true.structure)
# Learning the parameters using the true structure.
bn.true.fitted <- bn.fit(x = bn.true.structure, data = train, method = "bayes")

# Converting to grain obj. to use in compile.grain.
bn.fitted.grain <- as.grain(bn.fitted)
bn.true.fitted.grain <- as.grain(bn.true.fitted)


# Now creating junction trees using compile.grain.
# The trees will be queried to make predictions for S.
junction.tree.own <- compile(bn.fitted.grain)
junction.tree.true <- compile(bn.true.fitted.grain)

# Making predictions using helper-function (separate file)
prediction.own <- predictfunction(junction.tree.own,test,"S", bn.for.markowblanket = bn.structure)
prediction.true <- predictfunction(junction.tree.true,test,"S", bn.for.markowblanket = bn.true.structure)

# Comparing predictions with true values
conf.own <- make.confusion.matrix(test[,"S"],prediction.own)
conf.true <- make.confusion.matrix(test[,"S"],prediction.true)

# Output
conf.own
conf.true

all.equal(conf.own,conf.true) # -> The two are essentially the same

### TASK 4 ###
# Repeat the exercise (2) using a naive Bayes classifier,
# i.e. the predictive variables are independent given
# the class variable. Model the naive Bayes classifier as a BN.
# You have to create the BN by hand.

# Learning the structure of the BN.
bn.structure <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]")
plot(bn.structure)
# Learning the parameters.
bn.fitted <- bn.fit(x = bn.structure, data = train, method = "mle")

# Converting to grain obj. to use in compile.grain.
bn.fitted.grain <- as.grain(bn.fitted)

# Now creating junction trees using compile.grain.
# The trees will be queried to make predictions for S.
junction.tree.own <- compile(bn.fitted.grain)

# Making predictions using helper-function (separate file)
prediction.own <- predictfunction(junction.tree.own,test,"S")

# Comparing predictions with true values
conf.own <- make.confusion.matrix(test[,"S"],prediction.own)

# Output
conf.own
prop.table(conf.own)


