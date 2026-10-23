
#' The function calculates maximum knapsack value of the knapsack problem
#' using the brute force algoritm. The only solution that is guaranteed to give
#' a correct answer in all situations for the knapsack problem is using brute-
#' force search, i.e. going through all possible alternatives and return the
#' maximum value found.
#'
#' @param x data.frame two variables v and w positive numerical values
#' @param W double Maximum weight admitted by the knapsack
#'
#' @returns double the maximum knapsack value and the positions of the elements
#' @export
#'
#' @examples
#'n <- 2000
#'knapsack_objects <- data.frame(
#'  w = sample(1:4000, size = n, replace = TRUE),
#'  v = runif(n = n, 0, 10000))
#'brute_force_knapsack(knapsack_objects[1:8, ], W = 3500)
brute_force_knapsack <- function(x, W) {

  # Check the input data
  if (!is.data.frame(x) ||
      ncol(x) != 2 ||
      !setequal(names(x), c("v", "w"))) {
    stop("x must be a data.frame with variables v and w")
  }

  if (!is.numeric(x$v) || !is.numeric(x$w) ||
      any(!is.finite(x$v)) || any(!is.finite(x$w)) ||
      any(x$v <= 0) || any(x$w <= 0)) {
    stop("Values and weights must be positive numeric values")
  }

  if (!is.numeric(W) || length(W) != 1 ||
      !is.finite(W) || W <= 0) {
    stop("W must be a positive number")
  }

  n <- nrow(x)

  # Initialize the best solution
  best_value <- 0
  best_elements <- integer(0)

  # Examine all possible combinations
  for (i in 0:(2^n - 1)) {

    # Convert the number to a binary representation
    bits <- intToBits(i)[1:n]

    # Find the selected elements
    elements <- which(bits == 1)

    # Calculate total weight and value
    total_weight <- sum(x$w[elements])
    total_value <- sum(x$v[elements])

    # Update the best solution if the weight limit is satisfied
    if (total_weight <= W && total_value > best_value) {
      best_value <- total_value
      best_elements <- elements
    }
  }

  # Return the maximum value and selected row positions
  return(list(
    value = best_value,
    elements = best_elements
  ))
}

#' The function calculates maximum knapsack value of the knapsack problem
#' using the greedy algoritm. The idea behind a greedy algorithm is that in each
#' step you choose the best item. The best item is here defined as the item
#' with the highest value/weight ratio, chosen from the items not already packed
#'
#' @param x data.frame two variables v and w positive numerical values
#' @param W double Maximum weight admitted by the knapsack
#'
#' @returns double the maximum knapsack value and the positions of the elements
#' @export
#'
#' @examples
#' n <- 2000
#' knapsack_objects <- data.frame(
#'  w = sample(1:4000, size = n, replace = TRUE),
#'  v = runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack <- function(x, W) {

  # Check the input data
  if (!is.data.frame(x) ||
      ncol(x) != 2 ||
      !setequal(names(x), c("v", "w"))) {
    stop("x must be a data.frame with variables v and w")
  }

  if (!is.numeric(x$v) || !is.numeric(x$w) ||
      any(!is.finite(x$v)) || any(!is.finite(x$w)) ||
      any(x$v <= 0) || any(x$w <= 0)) {
    stop("Values and weights must be positive numeric values")
  }

  if (!is.numeric(W) || length(W) != 1 ||
      !is.finite(W) || W <= 0) {
    stop("W must be a positive number")
  }

  # Calculate the value-to-weight ratio
  ratio <- x$v / x$w

  # Sort items by decreasing ratio
  order_items <- order(ratio, decreasing = TRUE)

  # Initialize the knapsack
  total_weight <- 0
  total_value <- 0
  elements <- integer(0)

  # Examine items in sorted order
  for (i in order_items) {

    # Check if the item fits
    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      elements <- c(elements, i)
    }
  }

  # Return the total value and selected row positions
  return(list(
    value = total_value,
    elements = elements
  ))
}
