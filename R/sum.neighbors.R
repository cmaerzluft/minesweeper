#########################################################################################################################
#                                                                                                                       #
#  Title:
#  Author:
#  Description:
#  Last Edit:
#                                                                                                                       #
#########################################################################################################################
sum.neighbors <- function(board) {
  counter <- board
  # Add rows
  counter[-1, ] <- counter[-1, ] + board[-dim(board)[1], ]
  counter[-dim(counter)[1], ] <- counter[-dim(counter)[1], ] + board[-1, ]
  # Add columns
  counter[, -1] <- counter[, -1] + board[, -dim(board)[2]]
  counter[, -dim(counter)[2]] <- counter[, -dim(counter)[2]] + board[, -1]
  # Add corners
  counter[-1, -1] <- counter[-1, -1] + board[-dim(board)[1], -dim(board)[2]]
  counter[-1, -dim(counter)[2]] <- counter[-1, -dim(counter)[2]] + board[-dim(board)[1], -1]
  counter[-dim(counter)[1], -dim(counter)[2]] <- counter[-dim(counter)[1], -dim(counter)[2]] + board[-1, -1]
  counter[-dim(counter)[1], -1] <- counter[-dim(counter)[1], -1] + board[-1, -dim(board)[2]]
  
  return(counter)
}