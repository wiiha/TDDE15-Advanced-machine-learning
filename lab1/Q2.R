library(bnlearn)
library(Rgraphviz)
library(gRain)

source("./helper-functions.R")

### TASK 2 ###
# Learn a BN from 80 % of the Asia dataset.
# Use the BN learned to classify the remaining 20 % of
# the Asia dataset in two classes: S = yes and S = no.
# Use exact or approximate inference with the help of the bnlearn and gRain packages,
# you are not allowed to use functions such as predict.
# Report the confusion matrix; true/false positives/negatives.

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
prediction.own <- predictfunction(junction.tree.own,test,"S")
prediction.true <- predictfunction(junction.tree.true,test,"S")

# Comparing predictions with true values
conf.own <- make.confusion.matrix(test[,"S"],prediction.own)
conf.true <- make.confusion.matrix(test[,"S"],prediction.true)

# Output
conf.own
conf.true

all.equal(conf.own,conf.true) # -> The two are essentially the same



