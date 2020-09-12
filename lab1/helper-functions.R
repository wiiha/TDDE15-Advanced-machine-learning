# Creating own function for prediction on graph
predictfunction <- function(junction.tree, data, target, bn.for.markowblanket = NULL){
  predictions <- c()
  
  # Setting observation variables
  observ.nodes <- names(data)[-which(names(data) == target)]
  if(!is.null(bn.for.markowblanket)){
    print("With Markow blanket")
    observ.nodes <- mb(bn.for.markowblanket,target)
  }
  
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
    predictions[r] = ifelse (probability >= 0.5, "yes", "no")
    
  }
  return(predictions)
}


make.confusion.matrix <- function(true.values,predicted.values){
  return(table(true=true.values,prediction=predicted.values))
}

info <- function(bn, score.method = ""){
  plot(bn,main=score.method)
  print(bn)
}