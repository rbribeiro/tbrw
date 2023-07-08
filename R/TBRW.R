#' Simulate a TBRW instance with a given number of
#' steps and a prescribed vertex sequence
#'
#' @param steps integer number total number of steps
#' @param vertex_seq a vector of the number of vertices to be add at each step
#'
#' @return A list with the tree, walker position, total number of vertices,
#' degree sequence saw by the walker, and distance after `steps`
#'
#'
#' @export
tbrw.with_vertex_seq <- function(steps = 100, vertex_seq = rep(1,100)) {
  if(length(vertex_seq) < steps) stop("vertex_seq must have length >= the number of steps")
  # Initialize
  tree <- list(2,1)
  distance <- 0
  num_vertices <- 2
  walker <- 1
  deg_profile <- c(1)

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
    # Store the degree of the vertex for which the walker jumped to
    curr_deg <- length(tree[[walker]])
    deg_profile[curr_deg] <- 1 + ifelse(is.na(deg_profile[curr_deg]),0,deg_profile[curr_deg])
  }

  # Return tree, walker and distance
  return(list(tree = tree, walker = walker, total_vertices = num_vertices, distance = distance,
              degree_profile = deg_profile))
}
#' Bernoulli Growth Random Walk
#' Simulate an instance with a given number of steps and a probability p
#'
#' @param steps integer number total number of steps
#' @param prob probability of add a vertex at each step
#'
#' @return A list with the tree, walker position and distance after `steps`
#'
#'
#' @export
bgrw <- function(steps = 100, prob = 1) {
  if(prob > 1 || prob < 0) stop("prob must be between 0 and 1")
  # Initialize the vertex sequence
  vertex_seq <- sample(c(0,1),steps,replace = TRUE, prob = c(1-prob,prob))
  results <- tbrw.with_vertex_seq(steps, vertex_seq)
  return(results)
}
# Function to add two vectors without recycling
add_vectors <- function(v1, v2) {
  len <- min(length(v1), length(v2))
  new_v1 <- c(v1, rep(0, max(0, length(v2) - len)))
  new_v2 <- c(v2, rep(0, max(0, length(v1) - len)))
  return(new_v1 + new_v2)
}
#' Bernoulli Growth Random Walk Average
#' Replicate the BGRW for different p values
#'
#' @param iterations integer number total number of iterations
#' @param steps integer number total number of steps
#' @param p_values vector of probabilities
#'
#' @return the average distance and average degree profile after `steps`
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
bgrw.average <- function(iterations = 100, steps = 100, p_values = c(0.25,0.5,0.75,1)) {
  p_length <- length(p_values)
  av_speed <- rep(0,p_length)
  av_deg_profile <- rep(list(0),p_length)

  pb <- txtProgressBar(0,p_length, style = 3)

  for(j in 1:p_length) {
    setTxtProgressBar(pb,j)
    p <- p_values[j]
    for(i in 1:iterations) {
      results <- bgrw(steps, p)
      av_speed[j] <- av_speed[j] + results$distance/(steps*iterations)
      av_deg_profile[[j]] <- add_vectors(av_deg_profile[[j]], results$degree_profile/(steps*iterations))
    }

  }
  return(list(av_speed = av_speed, av_deg_profile = av_deg_profile))
}
