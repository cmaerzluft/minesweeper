#########################################################################################################################
#                                                                                                                       #
#  Title:
#  Author:
#  Description:
#  Last Edit:
#                                                                                                                       #
#########################################################################################################################
cell.information <- function(game) {
  group <- (game$cover != "X" & !is.na(game$cover)) | game$probability == 0
  uncovered_neighbors <- sum.neighbors(+group)
  uncovered_neighbors[group] <- uncovered_neighbors[group] - 1
  group <- (game$cover == "X" & !is.na(game$cover)) | game$probability == 1
  flagged_neighbors <- sum.neighbors(+group)
  flagged_neighbors[group] <- flagged_neighbors[group] - 1
  group <- is.na(game$cover)
  covered_neighbors <- sum.neighbors(+group)
  covered_neighbors[group] <- covered_neighbors[group] - 1
  
  return(list(uncovered_neighbors = uncovered_neighbors,
              flagged_neighbors = flagged_neighbors,
              covered_neighbors = covered_neighbors))
}