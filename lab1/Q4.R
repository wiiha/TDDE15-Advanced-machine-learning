library(bnlearn)
library(Rgraphviz)
library(gRain)

source("./helper-functions.R")

### TASK 4 ###
# Repeat the exercise (2) using a naive Bayes classifier,
# i.e. the predictive variables are independent given
# the class variable. Model the naive Bayes classifier as a BN.
# You have to create the BN by hand,

set.seed(2020)

data("asia")

# Splitting data 80/20 train/test
n=nrow(asia)
id=sample(1:n,floor(n*0.8))
train=asia[id,]
test=asia[-id,]

# Learning the structure of the BN.
bn.structure <- model2network("[S][A|S][T|S][L|S][B|S][E|S][X|S][D|S]")
plot(bn.structure)
# Learning the parameters.
bn.fitted <- bn.fit(x = bn.structure, data = train, method = "mle")

# Setting the true structure of the BN
bn.true.structure = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

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


