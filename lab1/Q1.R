library(bnlearn)
data("asia")

info <- function(bn, score.method){
  plot(bn,main=score.method)
  print(bn)
}

score.methodes = c("bic","loglik","aic","mbde")

for (s in score.methodes) {
  bn = hc(asia, score=s)
  info(bn, s)
}
