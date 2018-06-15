########################################################################################################################
#
# Title: Click on Board
# Author: Chris Maerzluft
# Date: 8/7/17
#
########################################################################################################################
#
# Summary: The takes a click location and plays it on the current board, opening any relevant cells including those
#   surround any zeros and the entire board if we lose.
#
# Inputs:
#   click - the location of the click, will run convert.location so it can be in any format accepted by that function
#   game - the game board (with cover, board, and whether it is a winner or loser)
#
# Outputs:
#   The game board with the clicked tile (on other relevant tiles) revealed on the cover piece. In the event of a bomb 
#     being selected the entire board turns to "Loser". In the event all non-bomb tiles have been selected the entire
#     board turns to "Winner".
#
########################################################################################################################
click.on.board <- function(position, click = "left", game) {
  selected_cover <- game$cover[position[1], position[2]]
  selected_board <- game$board[position[1], position[2]]
  
  ##### Click that can't do anything #####
  # left click on flag or right click on an opened piece or trying to play on a lost game
  if ((click == "left" & selected_cover == "X" & !is.na(selected_cover)) | 
      (click == "right" & selected_cover != "X" & !is.na(selected_cover)) |
      game$loser == TRUE) {
    return(game)
  }
  
  ##### Place/Remove flag on board #####
  # right click on covered piece/flag
  if (click == "right" & is.na(selected_cover)) {
    game$cover[position[1], position[2]] <- "X"
    return(game)
  }
  if (click == "right" & selected_cover == "X" & !is.na(selected_cover)) {
    game$cover[position[1], position[2]] <- NA
    return(game)
  }
  
  ##### Reveal clicked cell #####
  # Keep track of what is already opened
  opened <- !is.na(game$cover) & game$cover == "X"
  game$cover[position[1], position[2]] <- selected_board
  
  ##### Process the cover/game based on board value #####
  if (selected_board == -1) { # If move is a bomb
    # Reveal everything and flag position of Loser; return game
    game$cover <- game$board
    game$cover[position[1], position[2]] <- "Loser"
    game$loser <- TRUE
    return(game)
    
  } else if (selected_board == 0) { # If move is a non-bomb
    # Find first zero to open and non-zeros that were also opened:
    #   the non-zeros will be a table of all FALSEs because you can't get here unless you open a zero and everything that
    #   is opened before this click is tracked through the opened matrix
    zeros <- game$cover == 0 & !is.na(game$cover) & !opened
    non_zeros <- game$cover != 0 & !is.na(game$cover) & !opened 
    focus <- matrix(which(zeros, arr.ind = TRUE), ncol = 2)
    # Loop through until no new zeros are opened
    while (length(focus) > 0) {
      # Open all spaces touching a given zero
      ind <- matrix(FALSE, nrow = nrow(game$cover), ncol = ncol(game$cover))
      valid_rows <- intersect(1:dim(game$cover)[1], (focus[1, 1] - 1):(focus[1, 1] + 1))
      valid_cols <- intersect(1:dim(game$cover)[2], (focus[1, 2] - 1):(focus[1, 2] + 1))
      ind[valid_rows, valid_cols] <- TRUE
      ind[game$cover == "x" & !is.na(game$cover)] <- FALSE
      ind <- which(ind, arr.ind = T)
      game$cover[ind] <- game$board[ind]
      # Record what has been opened
      opened <- opened | non_zeros
      opened[focus[1, 1], focus[1, 2]] <- TRUE
      # Find next set of values to explore
      zeros <- game$cover == 0 & !is.na(game$cover) & !opened
      non_zeros <- game$cover != 0 & !is.na(game$cover) & !opened
      focus <- matrix(which(zeros, arr.ind = TRUE), ncol = 2)
    }
  }
  
  ##### Check if game has been won #####
  non_bombs <- game$board >= 0
  uncovered <- !is.na(game$cover) & game$cover != "X"
  safe <- non_bombs == uncovered
  if (all(safe) & !game$loser) {
    game$winner <- TRUE
    print(game)
  }

  Sys.sleep(0.25)
  return(game)
}

########################################################################################################################