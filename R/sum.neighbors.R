#########################################################################################################################
#                                                                                                                       #
#  Title: Sum Neighbors
#  Author: Chris Maerzluft
#  Description: Sums up the values of every piece touching a given cell
#  Last Edit: 6/15/18
#                                                                                                                       #
#########################################################################################################################
#
# Summary: This function takes a matrix and then adds each cells neighbors to it. a neighbor is defined as any cell that
#           shares a border with the cell (including corners).
# 
# Requires: 
#
# Inputs:
#   board           a matrix that you would like to sum over
#
# Outputs:
#   A matrix with the same dimensions of the input that has all the sums for a given cell
#
########################################################################################################################
sum.neighbors <- function(board) {
  # So we only include direct neighbors in the count
  counter <- board
  
  # Add north and south neighbors
  counter[-1, ] <- counter[-1, ] + board[-dim(board)[1], ]
  counter[-dim(counter)[1], ] <- counter[-dim(counter)[1], ] + board[-1, ]
  
  # Add east and west neighbors
  counter[, -1] <- counter[, -1] + board[, -dim(board)[2]]
  counter[, -dim(counter)[2]] <- counter[, -dim(counter)[2]] + board[, -1]
  
  # Add north east, north west, south west, and south east respectively
  counter[-1, -1] <- counter[-1, -1] + board[-dim(board)[1], -dim(board)[2]]
  counter[-1, -dim(counter)[2]] <- counter[-1, -dim(counter)[2]] + board[-dim(board)[1], -1]
  counter[-dim(counter)[1], -dim(counter)[2]] <- counter[-dim(counter)[1], -dim(counter)[2]] + board[-1, -1]
  counter[-dim(counter)[1], -1] <- counter[-dim(counter)[1], -1] + board[-1, -dim(board)[2]]
  
  return(counter)
}