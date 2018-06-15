#########################################################################################################################
#                                                                                                                       #
#  Title: cell information
#  Author: Chris Maerzluft
#  Description: Calculates general information about a cell that is useful to certain calculations
#  Last Edit: 6/15/18
#                                                                                                                       #
#########################################################################################################################
#
# Summary: Calculates information about the number of neighbors that are uncovered, flagged, or still covered as well as
#           the number of mines still touching the cell which we haven't found yet.
#
# Inputs:
#   game            The game board the user is playing on
#
# Outputs:
#   a list of matrices of the same size as the game board
#   uncovered_neighbors     The number of neighboring cells that are opened
#   flagged_neighbors       The number of neighboring cells that we are confident have a mine
#   covered_neighbors       The number of cells that we aren't sure about yet
#   remaining_mines         The number of mines that we haven't found for a given cell
#
########################################################################################################################
cell.information <- function(game) {
  # Uncovered Neighbors
  group <- (game$cover != "X" & !is.na(game$cover)) | game$probability == 0
  uncovered_neighbors <- sum.neighbors(+group)
  uncovered_neighbors[group] <- uncovered_neighbors[group] - 1
  # Flagged Neighbors
  group <- (game$cover == "X" & !is.na(game$cover)) | game$probability == 1
  flagged_neighbors <- sum.neighbors(+group)
  flagged_neighbors[group] <- flagged_neighbors[group] - 1
  # Everyone else
  group <- is.na(game$cover) & game$probability != 0 & game$probability != 1
  covered_neighbors <- sum.neighbors(+group)
  covered_neighbors[group] <- covered_neighbors[group] - 1
  # Remaining Mines
  remaining_mines <- game$cover - flagged_neighbors
  return(list(uncovered_neighbors = uncovered_neighbors,
              flagged_neighbors = flagged_neighbors,
              covered_neighbors = covered_neighbors,
              remaining_mines = remaining_mines))
}