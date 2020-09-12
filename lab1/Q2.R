library(bnlearn)
library(Rgraphviz)
library(gRain)

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

# Creating own function for prediction
predictfunction <- function(junction.tree, data, target){
  prediction <- c()
  
  # Setting observation variables
  observ.nodes <- names(data)[-which(names(data) == target)]
  # print(observ.nodes) # DEBUG
  
  for (r in 1:nrow(data)) {
    observ.nodes.state <- c()
    # observ.nodes.state <- data[r,!(names(data) %in% target)] # This method did not work. Don't know why, might be as.factor
    
    for (c in observ.nodes) {
      observ.nodes.state[c] = ifelse(data[r, c] == "yes", "yes", "no")
    }
    
    # print(observ.nodes.state)
    evidence = setEvidence(junction.tree, observ.nodes, observ.nodes.state)
    probability = querygrain(evidence, target)$S["yes"]
    #print(probability)
    prediction[r] = ifelse (probability >= 0.5, "yes", "no")
    
  }
  return(prediction)
}

make.confusion.matrix <- function(true.values,predicted.values){
  return(table(true=true.values,prediction=predicted.values))
}

# Making predictions using function
prediction.own <- predictfunction(junction.tree.own,test,"S")
prediction.true <- predictfunction(junction.tree.true,test,"S")

conf.own <- make.confusion.matrix(test[,"S"],prediction.own)
conf.true <- make.confusion.matrix(test[,"S"],prediction.true)

conf.own
conf.true

all.equal(conf.own,conf.true) # -> The two are essentially the same



