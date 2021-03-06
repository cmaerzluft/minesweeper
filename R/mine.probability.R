#########################################################################################################################
#                                                                                                                       #
#  Title: mine probability
#  Author: Chris Maerzluft
#  Description: Calculates the probability of a cell having a mine
#  Last Edit: 6/15/18
#                                                                                                                       #
#########################################################################################################################
#
# Summary: Basic idea so far. STILL IN PROGRESS
#    1) Flag all cells as either being solved, touching a number or neither
#      a) If cell has a number or flag it is solved
#      b) Everything else - check the borders for accuracy
#      c) If cell is touching a number it is touching a number
#    2) Probability of 1a group should be zero or 1 based on whether it is a number of flag
#    3) Probability of 1c will take some thought but breaks into three general groups
#      a) Certainty: We are absolutely certain that a piece is a bomb or not
#        i) If an unopened cell is touched by an open cell whose number of unopened neighbors equals its number then it 
#            is a bomb - give it a probability of 1.
#        ii) If an unopened cell is touched by an open cell whose number of flagged neighbors equals its number then it
#            is not a bomb - give it a probability of 0.
#        iii) If a shared neighbor has a different potential bombs, and the difference equals the number of neighbors that
#              are not being shared, those non-shared neighbors have a probability of 1.
#            DO THIS ONE FIRST AND MAKE SURE TO RESET CALCULATION IN BETWEEN
#            because this one has less strict rules for working, it can help reveal cases for iv) where iv) couldn't if
#            iv) had gone first
#        iv) use logic (written on paper) to determine that a cell(s) cannot have a bomb(s)
#      b) Certain probability:
#        i) IP: When we know that a bomb exists between a set number of unopened cells we can determine a definite 
#            probability. The easiest example of this is when there are only two unopened squares left and one bomb - We
#            know that the probability here is 0.5
#        ii) We will need to track these groups so we can remove them from the uncertainty group. We need to know the 
#            number of mines that we are certain we found and the number of unopened cells they could be in. In the 
#            example we have 1 mine and 2 unopened cells (Change cell.type to 4, count mines in a separate object)
#      c) Uncertain probability:
#        i) IP: When we can't determine the probability because there are too many unknowns - hopefully we can find a
#            statistical solution to our guessing. If one unopened cell suggests the probability is 0.5 but another
#            suggests it is 0.333 can we use that information to estimate a new probability perhaps a weighted average?
#            This will take some research.
#    4) Probability of 1b depends on the other groups
#      a) For all the unopened cells with ONLY unopened neighbors the probability should be the following formula
#         Total number of mines on board - total number of mines discovered - number of certain probability mines / 
#         Total number of spaces on board - total number of spaces opened - number of certain probability unopened cells
#        i) Total number of mines = total mines (can be pulled from game board)
#        ii) Total number of spaces = length(game$cover)
#        iii) total number of mines discovered = # of flags + # of tiles with probability equal to 1
#        iv) total number of spaces opened = # of opened squares + # of titles with probability equal to 0
#        v) total number of certain probability mines = # of mines "trapped" in a given section
#        vi) totla number of certain probability cells = # of unopened cells with "trapped" mines
#
# Inputs:
#   game            The game board the user is playing on
#
# Outputs:
#   The game but with an updated probability matrix
#
########################################################################################################################
mine.probability <- function(game) {
  # General variables
  board.rows <- nrow(game$cover)
  board.cols <- ncol(game$cover)
  
  ##### Cell State #####
  # 1) Flag all cells as either being solved, touching a number or neither
  #   a) If cell has a number or flag it is solved
  cell.type <- +!is.na(game$cover)
  #   b) Everything else - check the borders for accuracy
  counter <- sum.neighbors(cell.type)
  ind <- which(counter == 0, arr.ind = TRUE)
  cell.type[ind] <- 2
  #   c) If cell is touching a number it is touching a number
  ind <- which(cell.type < 1, arr.ind = TRUE)
  cell.type[ind] <- 3
  
  ##### Cell probabilities #####
  # 2) Probability of 1a group should be zero or 1 based on whether it is a number of flag
  game$probability[cell.type == 1 & game$cover != "X"] <- 0
  game$probability[cell.type == 1 & game$cover == "X"] <- 1
  
  # 3) Probability of 1c will take some thought but breaks into three general groups
  #   a) Certainty: We are absolutely certain that a piece is a bomb or not
  # NOTE: leaving in the game$probability == 0/1 because I think it will only help as things iterate through but
  #       we should keep an eye on how the solver assigns probabilities once we start using it in more cases
  game.info <- cell.information(game)
  #     i) If an unopened cell is touched by an open cell whose number of unopened neighbors equals its number then it 
  #         is a bomb - give it a probability of 1.
  group <- (game.info$covered_neighbors + game.info$flagged_neighbors == game$cover) & !is.na(game$cover)
  solved <- sum.neighbors(+group) > 0
  game$probability[cell.type == 3 & solved] <- 1
  cell.type[cell.type == 3 & solved] <- 1
  game.info <- cell.information(game)
  #     ii) If an unopened cell is touched by an open cell whose number of flagged neighbors equals its number then it
  #         is not a bomb - give it a probability of 0.
  group <- (game.info$flagged_neighbors == game$cover) & !is.na(game$cover)
  solved <- sum.neighbors(+group) > 0
  game$probability[cell.type == 3 & solved] <- 0
  cell.type[cell.type == 3 & solved] <- 1
  game.info <- cell.information(game)
  #     iii) If a shared neighbor has a different potential bombs, and the difference equals the number of neighbors that
  #           are not being shared, those non-shared neighbors have a probability of 1.
  #         DO THIS ONE FIRST AND MAKE SURE TO RESET CALCULATION IN BETWEEN
  #         because this one has less strict rules for working, it can help reveal cases for iv) where iv) couldn't if
  #         iv) had gone first
  # Only need to do for cells that have opened neighbors
  # # Neighbor IDs
  # cell.ids <- expand.grid(1:board.rows, 1:board.cols)
  # tmp <- matrix(paste(cell.ids[, 1], cell.ids[, 2], sep = "x"), ncol = board.cols)
  # neighbors.ids <- array(NA, dim = c(board.rows, board.cols, 9))
  # neighbors.ids[-1, -1, 1] <- tmp[-board.rows, -board.cols] # NE
  # neighbors.ids[-1, , 2] <- tmp[-board.rows, ]              # N
  # neighbors.ids[-1, -board.cols, 3] <- tmp[-board.rows, -1] # NW
  # neighbors.ids[, -1, 4] <- tmp[, -board.cols]              #  E
  # neighbors.ids[, , 5] <- tmp                               # cell reference
  # neighbors.ids[, -board.cols, 6] <- tmp[, -1]              #  W
  # neighbors.ids[-board.rows, -1, 7] <- tmp[-1, -board.cols] # SE
  # neighbors.ids[-board.rows, , 8] <- tmp[-1, ]              # S
  # neighbors.ids[-board.rows, -board.cols, 9] <- tmp[-1, -1] # SW
  # # Neighbor Mines
  # tmp <- game.info$remaining_mines
  # neighbors.rmi <- array(NA, dim = c(board.rows, board.cols, 9))
  # neighbors.rmi[-1, -1, 1] <- tmp[-board.rows, -board.cols] # NE
  # neighbors.rmi[-1, , 2] <- tmp[-board.rows, ]              # N
  # neighbors.rmi[-1, -board.cols, 3] <- tmp[-board.rows, -1] # NW
  # neighbors.rmi[, -1, 4] <- tmp[, -board.cols]              #  E
  # neighbors.rmi[, , 5] <- tmp                               # remaining mines
  # neighbors.rmi[, -board.cols, 6] <- tmp[, -1]              #  W
  # neighbors.rmi[-board.rows, -1, 7] <- tmp[-1, -board.cols] # SE
  # neighbors.rmi[-board.rows, , 8] <- tmp[-1, ]              # S
  # neighbors.rmi[-board.rows, -board.cols, 9] <- tmp[-1, -1] # SW
  # 
  # # Will be looped? maybe vectorized
  # ind <- which(neighbors.rmi > 0 & !is.na(neighbors.rmi), arr.ind = T)
  # ind <- ind[which(ind[, 1] == 2 & ind[, 2] == 14), ]
  # large.neighbor <- ind[which(neighbors.rmi[ind] == max(neighbors.rmi[ind])), , drop = FALSE]
  # small.neighbor <- ind[which(neighbors.rmi[ind] == min(neighbors.rmi[ind])), , drop = FALSE]
  # diff <- neighbors.rmi[large.neighbor] - neighbors.rmi[small.neighbor]
  # 
  # # need shared neighbors
  # ln.row <- as.numeric(gsub("x[0-9]+", "", neighbors.ids[large.neighbor]))
  # ln.col <- as.numeric(gsub("[0-9]+x", "", neighbors.ids[large.neighbor]))
  # large.neighbor.rows <- (max(1, ln.row - 1)):(min(dim(game$cover)[1], ln.row + 1))
  # large.neighbor.cols <- (max(1, ln.col - 1)):(min(dim(game$cover)[2], ln.col + 1))
  # large.neighbor <- expand.grid(large.neighbor.rows, large.neighbor.cols)
  # sn.row <- as.numeric(gsub("x[0-9]+", "", neighbors.ids[small.neighbor]))
  # sn.col <- as.numeric(gsub("[0-9]+x", "", neighbors.ids[small.neighbor]))
  # small.neighbor.rows <- (max(1, sn.row - 1)):(min(dim(game$cover)[1], sn.row + 1))
  # small.neighbor.cols <- (max(1, sn.col - 1)):(min(dim(game$cover)[2], sn.col + 1))
  # small.neighbor <- expand.grid(small.neighbor.rows, small.neighbor.cols)
  
  #     iv) use logic (written on paper) to determine that a cell(s) cannot have a bomb(s)
  #   b) Certain probability:
  #     i) IP: When we know that a bomb exists between a set number of unopened cells we can determine a definite 
  #         probability. The easiest example of this is when there are only two unopened squares left and one bomb - We
  #         know that the probability here is 0.5
  #     ii) We will need to track these groups so we can remove them from the uncertainty group. We need to know the 
  #         number of mines that we are certain we found and the number of unopened cells they could be in. In the 
  #         example we have 1 mine and 2 unopened cells (Change cell.type to 4, count mines in a separate object)
  #   c) Uncertain probability:
  #     i) IP: When we can't determine the probability because there are too many unknowns - hopefully we can find a
  #         statistical solution to our guessing. If one unopened cell suggests the probability is 0.5 but another
  #         suggests it is 0.333 can we use that information to estimate a new probability perhaps a weighted average?
  #         This will take some research.
  # 4) Probability of 1b depends on the other groups
  #   a) For all the unopened cells with ONLY unopened neighbors the probability should be the following formula
  #     Total number of mines on board - total number of mines discovered - number of certain probability mines / 
  #     Total number of spaces on board - total number of spaces opened - number of certain probability unopened cells
  #     i) Total number of mines = total mines (can be pulled from game board)
  #     ii) Total number of spaces = length(game$cover)
  #     iii) total number of mines discovered = # of flags + # of tiles with probability equal to 1
  #     iv) total number of spaces opened = # of opened squares + # of titles with probability equal to 0
  #     v) total number of certain probability mines = # of mines "trapped" in a given section
  #     vi) totla number of certain probability cells = # of unopened cells with "trapped" mines
  
  return(game)
}