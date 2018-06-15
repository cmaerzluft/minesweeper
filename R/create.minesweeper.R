########################################################################################################################
#
# Title: Minesweeper Game Creation
# Author: Chris Maerzluft
# Date: 8/7/17
#
########################################################################################################################
#
# Summary: This function generates a Minesweeper board for a given difficulty and first selection.
# 
# Requires: click
#
# Inputs:
#   first_move -  a number between 1 and "row"x"column indicating where the first click is made, 
#     if 0 the game will still generate a board but it is possible to lose on the first guess
#   difficulty -  "Beginner" (9 x 9 board, 10 mines) 
#                 "Classic Beginner" (8 x 8 board, 10 mines)
#                 "Intermediate" (16 x 16 board, 40 mines)
#                 "Expert" (16 x 30 board, 99 mines)
#                 "Custom" ("row" x "column" board, "mines" mines)
#   row - number of rows in a board if custom difficulty is selected
#   column - number of columns in a board if custom difficulty is selected
#   mines - number of mines in a board if custom difficulty is selected
#
# Outputs:
#   A list with two matrices:
#     Matrix 1 - contains Xs for squares that are unselected, and corresponding matrix 2 values for squares that are
#       selected. Thus if first_move is set to 0, it will be a matrix of all Xs.
#     Matrix 2 - contains the values of the board where "b" represents a bomb and 0 - 8 represent the number of bombs
#       touching the give square.
#
########################################################################################################################
create.minesweeper <- function(difficulty = "Expert", row = 16, column = 30, mines = 99, first_move = NULL, guarantee = NULL) {
  ##### Initial Values #####
  difficulties <- c("Beginner", "Classic Beginner", "Intermediate", "Expert", "Custom")
  row_vals <-   c( 9,  8, 16, 16, row)
  col_vals <-   c( 9,  8, 16, 30, column)
  mine_vals <-  c(10, 10, 40, 99, mines)
  rows <- row_vals[difficulties %in% difficulty]
  columns <- col_vals[difficulties %in% difficulty]
  mine_num <- mine_vals[difficulties %in% difficulty]
  cells <- rows*columns
  cover_mat <- matrix(rep(NA, cells), nrow = rows, ncol = columns)
  board_mat <- matrix(rep(0, cells), nrow = rows, ncol = columns)
  prob_mat <- matrix(-1, nrow = rows, ncol = columns)
  
  ##### Place Mines #####
  if (is.null(first_move) | (!is.null(first_move) & guarantee == "None")) { # First selected cell can be anything
    ind <- arrayInd(sample(length(board_mat), mine_num), dim(board_mat))
  } else {
    # NOTE: Not sure the below method is "best" for ensuring specific cells are excluded from bomb placement. Given it 
    #       is not using a loop and ensures randomness across the matrix I am not sure what else to do though or that
    #       I really care/want to put the effort in the research alternatives.
    if (guarantee == "One") { # First selected cell must be >= 0
      ind <- matrix(rep(TRUE, cells), nrow = rows, ncol = columns)
      ind[first_move[1], first_move[2]] <- FALSE
      ind <- which(ind, arr.ind = T)
      ind <- ind[sample(1:dim(ind)[1], mine_num), ]
    } else if (guarantee == "Zero") { # First selected cell must be == 0
      ind <- matrix(rep(TRUE, cells), nrow = rows, ncol = columns)
      valid_rows <- intersect(1:dim(board_mat)[1], (first_move[1] - 1):(first_move[1] + 1))
      valid_cols <- intersect(1:dim(board_mat)[2], (first_move[2] - 1):(first_move[2] + 1))
      ind[valid_rows, valid_cols] <- FALSE
      ind <- which(ind, arr.ind = T)
      ind <- ind[sample(1:dim(ind)[1], mine_num), ]
    } else {
      stop("invalid first_move or guarantee")
    }
  }
  board_mat[ind] <- -1
  
  ##### Create Cell Counts #####
  # NOTE: This was my first attempt. I am leaving it here for now because when I write this up I want to show the 
  #       difference between these two methods (I would imagine the current method is a lot faster over larger size boards)
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
  # Sum neighbors (works because mines are -1 and everything else starts as zero)
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
  game <- mine.probability(game)
  
  return(game)
}

########################################################################################################################