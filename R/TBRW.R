#' Simulate a TBRW instance with a given number of
#' steps and a prescribed vertex sequence
#'
#' @param steps integer number total number of steps
#' @param vertex_seq a vector of the number of vertices to be add at each step
#'
#' @return A list with the tree, walker position, total number of vertices
#' and distance after `steps`
#'
#'
#' @export
tbrw.with_vertex_seq <- function(steps = 100, vertex_seq = rep(1,100)) {
  if(length(vertex_seq) < steps) stop("vertex_seq must have length >= the number of steps")
  # Initialize the tree with a single edge
  tree <- list(2,1)
  distance <- 0
  num_vertices <- 2
  # Initialize walker position
  walker <- 1

  for (i in 1:steps) {
    # Check if new vertices must be added
    if (vertex_seq[i]>0) {
      # Generating new vertices
      new_vertices <- (num_vertices+1):(num_vertices + vertex_seq[i])
      # Updating the list of neighbors at the walker's position
      tree[[walker]] <- c(tree[[walker]], new_vertices)
      # Adding the new vertices to the tree
      tree <- c(tree, rep(walker,vertex_seq[i]))
      # Updating the total number of vertices
      num_vertices <- num_vertices + vertex_seq[i]
    }
    # If there are possible moves, choose one
    if(length(tree[[walker]]) > 1) {
      jump <- sample(tree[[walker]], size = 1)
      distance <- distance + ifelse(walker < jump, 1, -1)
      walker <- jump

    } else {
      distance <- distance + ifelse(walker == 1, 1,-1)
      walker <- tree[[walker]]
    }

  }

  # Return tree, walker and distance
  return(list(tree = tree, walker = walker, total_vertices = num_vertices, distance = distance))
}
#' Bernoulli Growth Random Walk
#' Simulate an instance with a given number of steps and a probability p
#'
#' @param steps integer number total number of steps
#' @param prob probability of add a vertex at each step
#'
#' @return A list with the tree, walker position and distance after `steps`
#'
#' @importFrom stats rbinom
#'
#' @export
bgrw <- function(steps = 100, prob = 1) {
  if(prob > 1 || prob < 0) stop("prob must be between 0 and 1")
  # Initialize the vertex sequence
  vertex_seq <- rbinom(steps,1,prob)
  results <- tbrw.with_vertex_seq(steps, vertex_seq)
  return(results)
}
