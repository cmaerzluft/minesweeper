########################################################################################################################
#
# Title: Cell Bomb Probability
# Author: Chris Maerzluft
# Date:
#
########################################################################################################################
#
# Summary:
#
# Inputs:
#
# Outputs:
#
########################################################################################################################
board.probability <- function(game) {
  # Create probability matrix
  game$proba <- matrix(NA, nrow = dim(game$cover)[1], ncol = dim(game$cover)[2])
  
  ##### Ideas for calculating #####
  # 1. Find numbers that touch the same number of covered cells as their number
  # 2. how to find patterns simply?
  #   a. pattern 1: 1,2,1
  #   b. pattern 2: 1,2,2,1
  #################################
  
}