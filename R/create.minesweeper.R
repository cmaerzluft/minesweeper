########################################################################################################################
#
# Title: Minesweeper Game Creation
# Author: Chris Maerzluft
# Description: Create a minesweeper game than can be played
# Date: 6/15/18
#
########################################################################################################################
#
# Summary: This function generates a Minesweeper board for a given difficulty and first move.
# 
# Requires: sum.neighbors, click.on.board, mine.probability
#
# Inputs:
#   difficulty      5 default board sizes:
#                     Beginner - 9 x 9 with 10 mines
#                     Classic Beginner - 8 x 8 with 10 mines (I've seen some say beginner is 8 x 8, my phone has 9 x 9)
#                     Intermediate - 16 x 16 with 40 mines
#                     Expert - 16 X 30 with 99 mines
#                     Custom - reads the row, column and mines inputs (if Custom is selected and no row, column, and mines
#                               are input then it defaults to Expert level)
#   row             If Custom is selected, the number of rows on the board
#   column          If Custom is selected, the number of columns on the board
#   mines           If Custom is selected, the number of mines on the board
#   first_move      A vector of length two containing the row and column of a first move e.g. c(12, 15). Must be a valid
#                     location based on the board size. If NULL, guarantee will be treated as NULL as well
#   guarantee       Select whether your first move will have a specified outcome:
#                     Zero - ensures the first move and all its neighbors do not have bombs
#                     One - ensures the first move does not have a bomb but says nothing of its neighbors
#                     None - randomly places mines on the board. Can also leave it as NULL
#
# Outputs:
#   A list of game information:
#   cover           matrix of size row x column. The game board that a user would typically see. All probabilites are 
#                     calculated based on this matrix
#   board           matrix of size row x column. The final solution to the game. This is where all the mines and cell 
#                     counts are stored until a piece is uncovered on the cover
#   probability     matrix of size row x column. This is a matrix that stores the likelihood a given cell has a bomb. 
#                     See mine.probability for that calculation
#   winner          TRUE if the game has been won
#   loser           TRUE if the game has been lost
#
########################################################################################################################
create.minesweeper <- function(difficulty = "Expert", 
                               row = 16, 
                               column = 30, 
                               mines = 99, 
                               first_move = NULL, 
                               guarantee = NULL) {
  
  ##### Initialize Game Pieces #####
  difficulties <- c("Beginner", "Classic Beginner", "Intermediate", "Expert", "Custom")
  row_vals <-   c( 9,  8, 16, 16, row)
  col_vals <-   c( 9,  8, 16, 30, column)
  mine_vals <-  c(10, 10, 40, 99, mines)
  rows <- row_vals[difficulties %in% difficulty]
  columns <- col_vals[difficulties %in% difficulty]
  mine_num <- mine_vals[difficulties %in% difficulty]
  cells <- rows*columns
  cover_mat <- matrix(rep(NA, cells), nrow = rows, ncol = columns) # What the user will see
  board_mat <- matrix(rep(0, cells), nrow = rows, ncol = columns) # What the solution is
  prob_mat <- matrix(-1, nrow = rows, ncol = columns) # The probability of a given cell having a mine 
  # not inherently a game feature but had to include it somewhere so why not with the game
  
  ##### Place Mines #####
  if (is.null(first_move) | (!is.null(first_move) & guarantee == "None")) { # First selected cell can be anything
    # uniform distribution
    ind <- arrayInd(sample(length(board_mat), mine_num), dim(board_mat))
  } else {
    if (guarantee == "One") { # First selected cell must be >= 0
      ind <- matrix(rep(TRUE, cells), nrow = rows, ncol = columns)
      ind[first_move[1], first_move[2]] <- FALSE
      ind <- which(ind, arr.ind = T)
      # uniform over everything but the first_move
      ind <- ind[sample(1:dim(ind)[1], mine_num), ]
    } else if (guarantee == "Zero") { # First selected cell must be == 0
      ind <- matrix(rep(TRUE, cells), nrow = rows, ncol = columns)
      neighbor_rows <- intersect(1:dim(board_mat)[1], (first_move[1] - 1):(first_move[1] + 1))
      neighbor_cols <- intersect(1:dim(board_mat)[2], (first_move[2] - 1):(first_move[2] + 1))
      ind[neighbor_rows, neighbor_cols] <- FALSE
      ind <- which(ind, arr.ind = T)
      # uniform over everything but the first_move and its neighbors
      ind <- ind[sample(1:dim(ind)[1], mine_num), ]
    } else {
      stop("invalid first_move or guarantee")
    }
  }
  board_mat[ind] <- -1
  
  ##### Create Cell Counts #####
  # NOTE: This was my first attempt. I am leaving it here for now because when I write this up I want to show the 
  #       difference between these two methods (I would imagine the current method is a lot faster over larger size boards)
  # This essentially loops over each cell counting all the -1 that are touching a given cell
  # board <- do.call(rbind, lapply(1:dim(board_mat)[1], FUN = function(row_index) {
  #   do.call(cbind, lapply(1:dim(board_mat)[2], FUN = function(col_index) {
  #     val <- board_mat[row_index, col_index]
  #     if (val != -1) {
  #       row1 <- max((row_index - 1), 1):min((row_index + 1), dim(board_mat)[1])
  #       col1 <- max((col_index - 1), 1):min((col_index + 1), dim(board_mat)[2])
  #       neighbors <- board_mat[row1, col1]
  #       board_mat[row_index, col_index] <- sum(neighbors == -1)
  #     } else {
  #       board_mat[row_index, col_index] <- -1
  #     }
  #   }))
  # }))
  # Sum neighbors (works because mines are -1 and everything else starts as zero) - see sum.neighbors for how it works
  counter <- sum.neighbors(board_mat)
  counter <- abs(counter)
  # Replace non-bombs with their counts
  ind <- which(board_mat != -1, arr.ind = T)
  board_mat[ind] <- counter[ind]
  
  ##### Cover Board #####
  game <- list(cover = cover_mat, 
               board = board_mat, 
               probability = prob_mat, 
               winner = FALSE, loser = FALSE)
  
  ##### Perform First Move #####
  if (!is.null(first_move)) {
    game <- click.on.board(position = first_move, click = "left", game)
  }
  
  ##### Calculate Probability Matrix ####
  # game <- mine.probability(game)
  
  return(game)
}

########################################################################################################################