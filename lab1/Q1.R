library(bnlearn)

### TASK 1 ###
# Show that multiple runs of the hill-climbing algorithm
# can return non-equivalent Bayesian network (BN) structures.
# Explain why this happens.


# Helper function
info <- function(bn, score.method = ""){
  plot(bn,main=score.method)
  print(bn)
}

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
plot(cpdag(bn.hc.10), main="Random restarts: 10")
plot(bn.hc.5, main="Random restarts: 5")
plot(cpdag(bn.hc.5), main="Random restarts: 5")
plot(bn.hc.1, main="Random restarts: 1")
plot(cpdag(bn.hc.1), main="Random restarts: 1")
plot(bn.hc.100, main="Random restarts: 100")
plot(cpdag(bn.hc.100), main="Random restarts: 100")

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

