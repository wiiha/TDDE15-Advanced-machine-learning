library(bnlearn)

e = empty.graph(LETTERS[1:6], num = 1)

class(e)
e

arc.set = matrix(c("A", "B", "B", "F", "C", "F","D","A","C","E","B","C","D","E"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set
e

graphviz.plot(e, layout = "dot")
graphviz.plot(e, layout = "fdp")
# Plotting http://bnlearn.com/examples/graphviz-plot/
