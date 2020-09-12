library(bnlearn)
library(Rgraphviz)
library(gRain)

source("./helper-functions.R")

### TASK 3 ###
# In the previous exercise, S was classified using observations for all the rest of the variables.
# S should be classified by using only observations for the so-called Markov blanket of S,
# i.e. its parents plus its children plus the parents of its children minus S itself.
# Report again the confusion matrix.

set.seed(2020)

data("asia")

# Splitting data 80/20 train/test
n=nrow(asia)
id=sample(1:n,floor(n*0.8))
train=asia[id,]
test=asia[-id,]

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



