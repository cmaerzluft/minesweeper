#########################################################################################################################
#                                                                                                                       #
#  Title: Play Minesweeper
#  Author: Christopher Maerzluft
#  Description: Plays the minesweeper created using the scripts in the library directory
#  Last Edit: 9/22/17
#                                                                                                                       #
#########################################################################################################################
##### Libraries, Directories, and Other #####
rm(list = ls(all = TRUE))
gc()
setwd("~/Desktop/Miscellaneous/Data Blog/Minesweeper")
library.dir   <- "Library/"
library <- file.path(library.dir, "library.R")
source(library)

##### Start game #####
set.seed(1)
first_move <- c(sample(1:16, 1), sample(1:30, 1))
game <- create.minesweeper(difficulty = "Expert",
                           first_move = first_move,
                           guarantee = "Zero")

emergency.stop <- 100
while (game$loser == FALSE & emergency.stop > 0) {
  print(emergency.stop)
  emergency.stop <- emergency.stop - 1
  flag.these <- which(game$probability == 1 & is.na(game$cover), arr.ind = TRUE)
  open.these <- which(game$probability == 0 & is.na(game$cover), arr.ind = TRUE)
  if (length(flag.these) == 0 & length(open.these) == 0) {
    break
  }
  
  if (length(flag.these) > 0) {
    for (i1 in 1:nrow(flag.these)) {
      game <- click.on.board(flag.these[i1, ], game = game, click = "right")
    }
  }
  if (length(open.these) > 0) {
    for (i1 in 1:nrow(open.these)) {
      game <- click.on.board(open.these[i1, ], game = game, click = "left")
    }
  }
  game <- mine.probability(game)
  print(game$cover)
  
}




# To do:
# write up a blog entry for what I did to create the create.minesweeper() function
# Create a probability of bomb calculator
# Create a solver
# Create a machine learning algorithm

##### Solve game manually #####
set.seed(1)
first_move <- c(sample(1:16, 1), sample(1:30, 1))
game <- create.minesweeper(difficulty = "Expert",
                           first_move = first_move,
                           guarantee = "Zero")
game$cover
game <- click.on.board(c(3, 13), "right", game)
game$cover
game <- click.on.board(c(3, 14), "left", game)
game$cover
game <- click.on.board(c(4, 14), "right", game)
game$cover
game <- click.on.board(c(5, 14), "right", game)
game$cover
game <- click.on.board(c(6, 14), "left", game)
game$cover
game <- click.on.board(c(5, 15), "left", game)
game$cover
game <- click.on.board(c(6, 15), "left", game)
game$cover
game <- click.on.board(c(7, 15), "left", game)
game$cover
game <- click.on.board(c(7, 14), "left", game)
game$cover
game <- click.on.board(c(7, 13), "left", game)
game$cover
game <- click.on.board(c(7, 12), "right", game)
game$cover
game <- click.on.board(c(7, 10), "left", game)
game$cover
game <- click.on.board(c(7, 11), "left", game)
game$cover
game <- click.on.board(c(4, 15), "left", game)
game$cover
game <- click.on.board(c(4, 16), "left", game)
game$cover
game <- click.on.board(c(3, 15), "right", game)
game$cover
game <- click.on.board(c(1, 15), "right", game)
game$cover
game <- click.on.board(c(2, 15), "left", game)
game$cover
game <- click.on.board(c(1, 16), "left", game)
game$cover
game <- click.on.board(c(2, 16), "left", game)
game$cover
game <- click.on.board(c(3, 16), "left", game)
game$cover
game <- click.on.board(c(1, 17), "left", game)
game$cover
game <- click.on.board(c(3, 17), "left", game)
game$cover
game <- click.on.board(c(4, 17), "left", game)
game$cover
game <- click.on.board(c(5, 17), "left", game)
game$cover
game <- click.on.board(c(5, 16), "left", game)
game$cover
game <- click.on.board(c(3, 18), "right", game)
game$cover
game <- click.on.board(c(4, 18), "right", game)
game$cover
game <- click.on.board(c(5, 18), "left", game)
game$cover
game <- click.on.board(c(4, 19), "left", game)
game$cover
game <- click.on.board(c(3, 19), "left", game)
game$cover
game <- click.on.board(c(2, 19), "left", game)
game$cover
game <- click.on.board(c(2, 20), "left", game)
game$cover
game <- click.on.board(c(3, 20), "left", game)
game$cover
game <- click.on.board(c(4, 20), "left", game)
game$cover
game <- click.on.board(c(5, 20), "left", game)
game$cover
game <- click.on.board(c(5, 19), "left", game)
game$cover
game <- click.on.board(c(6, 18), "left", game)
game$cover
game <- click.on.board(c(6, 19), "left", game)
game$cover
game <- click.on.board(c(6, 20), "left", game)
game$cover
game <- click.on.board(c(1, 19), "right", game)
game$cover
game <- click.on.board(c(1, 20), "right", game)
game$cover
game <- click.on.board(c(7, 16), "right", game)
game$cover
game <- click.on.board(c(7, 17), "left", game)
game$cover
game <- click.on.board(c(7, 18), "right", game)
game$cover
game <- click.on.board(c(7, 19), "left", game)
game$cover
game <- click.on.board(c(7, 20), "right", game)
game$cover
game <- click.on.board(c(8, 15), "left", game)
game$cover
game <- click.on.board(c(8, 14), "left", game)
game$cover
game <- click.on.board(c(8, 16), "left", game)
game$cover
game <- click.on.board(c(8, 17), "left", game)
game$cover
game <- click.on.board(c(8, 18), "left", game)
game$cover
game <- click.on.board(c(8, 19), "left", game)
game$cover
game <- click.on.board(c(8, 20), "left", game)
game$cover
game <- click.on.board(c(9, 14), "left", game)
game$cover
game <- click.on.board(c(9, 15), "left", game)
game$cover
game <- click.on.board(c(8, 13), "right", game)
game$cover
game <- click.on.board(c(7, 21), "left", game)
game$cover
game <- click.on.board(c(8, 21), "left", game)
game$cover
game <- click.on.board(c(9, 21), "left", game)
game$cover
game <- click.on.board(c(9, 20), "left", game)
game$cover
game <- click.on.board(c(7, 22), "left", game)
game$cover
game <- click.on.board(c(8, 22), "left", game)
game$cover
game <- click.on.board(c(6, 21), "right", game)
game$cover
game <- click.on.board(c(6, 22), "right", game)
game$cover
game <- click.on.board(c(5, 21), "left", game)
game$cover
game <- click.on.board(c(4, 21), "right", game)
game$cover
game <- click.on.board(c(3, 21), "left", game)
game$cover
game <- click.on.board(c(2, 21), "left", game)
game$cover
game <- click.on.board(c(1, 21), "right", game)
game$cover
game <- click.on.board(c(2, 22), "left", game)
game$cover
game <- click.on.board(c(3, 22), "left", game)
game$cover
game <- click.on.board(c(4, 22), "left", game)
game$cover
game <- click.on.board(c(5, 22), "left", game)
game$cover
game <- click.on.board(c(1, 22), "right", game)
game$cover
game <- click.on.board(c(2, 23), "left", game)
game$cover
game <- click.on.board(c(3, 23), "left", game)
game$cover
game <- click.on.board(c(1, 23), "right", game)
game$cover
game <- click.on.board(c(1, 24), "left", game)
game$cover
game <- click.on.board(c(1, 25), "left", game)
game$cover
game <- click.on.board(c(1, 28), "right", game)
game$cover
game <- click.on.board(c(2, 28), "right", game)
game$cover
game <- click.on.board(c(3, 25), "right", game)
game$cover
game <- click.on.board(c(3, 26), "left", game)
game$cover
game <- click.on.board(c(3, 27), "right", game)
game$cover
game <- click.on.board(c(3, 28), "left", game)
game$cover
game <- click.on.board(c(5, 23), "right", game)
game$cover
game <- click.on.board(c(6, 23), "left", game)
game$cover
game <- click.on.board(c(5, 24), "right", game)
game$cover
game <- click.on.board(c(4, 25), "right", game)
game$cover
game <- click.on.board(c(5, 25), "left", game)
game$cover
game <- click.on.board(c(6, 24), "left", game)
game$cover
game <- click.on.board(c(6, 25), "right", game)
game$cover
game <- click.on.board(c(6, 26), "left", game)
game$cover
game <- click.on.board(c(6, 27), "left", game)
game$cover
game <- click.on.board(c(4, 26), "left", game)
game$cover
game <- click.on.board(c(4, 27), "left", game)
game$cover
game <- click.on.board(c(5, 26), "right", game)
game$cover
game <- click.on.board(c(5, 27), "left", game)
game$cover
game <- click.on.board(c(4, 28), "right", game)
game$cover
game <- click.on.board(c(5, 28), "right", game)
game$cover
game <- click.on.board(c(6, 28), "left", game)
game$cover
game <- click.on.board(c(7, 28), "left", game)
game$cover
game <- click.on.board(c(8, 28), "right", game)
game$cover
game <- click.on.board(c(9, 28), "right", game)
game$cover
game <- click.on.board(c(10, 28), "right", game)
game$cover
game <- click.on.board(c(10, 23), "right", game)
game$cover
game <- click.on.board(c(11, 23), "right", game)
game$cover
game <- click.on.board(c(12, 23), "left", game)
game$cover
game <- click.on.board(c(13, 25), "right", game)
game$cover
game <- click.on.board(c(13, 24), "left", game)
game$cover
game <- click.on.board(c(13, 23), "right", game)
game$cover
game <- click.on.board(c(14, 23), "left", game)
game$cover
game <- click.on.board(c(14, 24), "left", game)
game$cover
game <- click.on.board(c(14, 25), "left", game)
game$cover
game <- click.on.board(c(14, 26), "left", game)
game$cover
game <- click.on.board(c(14, 27), "right", game)
game$cover
game <- click.on.board(c(14, 28), "left", game)
game$cover
game <- click.on.board(c(10, 22), "left", game)
game$cover
game <- click.on.board(c(10, 21), "left", game)
game$cover
game <- click.on.board(c(10, 20), "right", game)
game$cover
game <- click.on.board(c(11, 20), "left", game)
game$cover
game <- click.on.board(c(11, 21), "left", game)
game$cover
game <- click.on.board(c(11, 22), "right", game)
game$cover
game <- click.on.board(c(12, 22), "left", game)
game$cover
game <- click.on.board(c(12, 21), "left", game)
game$cover
game <- click.on.board(c(12, 20), "left", game)
game$cover
game <- click.on.board(c(12, 19), "left", game)
game$cover
game <- click.on.board(c(13, 22), "left", game)
game$cover
game <- click.on.board(c(13, 21), "left", game)
game$cover
game <- click.on.board(c(14, 22), "left", game)
game$cover
game <- click.on.board(c(14, 21), "left", game)
game$cover
game <- click.on.board(c(15, 21), "left", game)
game$cover
game <- click.on.board(c(15, 21), "left", game)
game$cover
game <- click.on.board(c(14, 29), "right", game)
game$cover
game <- click.on.board(c(15, 28), "left", game)
game$cover
game <- click.on.board(c(15, 29), "left", game)
game$cover
game <- click.on.board(c(16, 28), "right", game)
game$cover
game <- click.on.board(c(16, 29), "right", game)
game$cover
game <- click.on.board(c(13, 29), "left", game)
game$cover
game <- click.on.board(c(12, 29), "right", game)
game$cover
game <- click.on.board(c(11, 29), "left", game)
game$cover
game <- click.on.board(c(10, 29), "left", game)
game$cover
game <- click.on.board(c(13, 20), "right", game)
game$cover
game <- click.on.board(c(16, 19), "right", game)
game$cover
game <- click.on.board(c(15, 19), "right", game)
game$cover
game <- click.on.board(c(14, 19), "right", game)
game$cover
game <- click.on.board(c(13, 19), "left", game)
game$cover
game <- click.on.board(c(12, 18), "right", game)
game$cover
game <- click.on.board(c(13, 18), "right", game)
game$cover
game <- click.on.board(c(14, 18), "left", game)
game$cover
game <- click.on.board(c(15, 18), "left", game)
game$cover
game <- click.on.board(c(16, 18), "left", game)
game$cover
game <- click.on.board(c(16, 17), "left", game)
game$cover
game <- click.on.board(c(14, 17), "left", game)
game$cover
game <- click.on.board(c(13, 17), "left", game)
game$cover
game <- click.on.board(c(12, 17), "left", game)
game$cover
game <- click.on.board(c(12, 16), "left", game)
game$cover
game <- click.on.board(c(13, 16), "left", game)
game$cover
game <- click.on.board(c(14, 16), "right", game)
game$cover
game <- click.on.board(c(12, 15), "right", game)
game$cover
game <- click.on.board(c(13, 15), "right", game)
game$cover
game <- click.on.board(c(14, 15), "left", game)
game$cover
game <- click.on.board(c(12, 14), "left", game)
game$cover
game <- click.on.board(c(12, 13), "right", game)
game$cover
game <- click.on.board(c(13, 13), "left", game)
game$cover
game <- click.on.board(c(13, 14), "left", game)
game$cover
game <- click.on.board(c(14, 13), "left", game)
game$cover
game <- click.on.board(c(14, 14), "left", game)
game$cover
game <- click.on.board(c(15, 14), "right", game)
game$cover
game <- click.on.board(c(16, 14), "left", game)
game$cover
game <- click.on.board(c(16, 13), "left", game)
game$cover
game <- click.on.board(c(15, 13), "left", game)
game$cover
game <- click.on.board(c(10, 12), "left", game)
game$cover
game <- click.on.board(c(11, 12), "left", game)
game$cover
game <- click.on.board(c(12, 12), "left", game)
game$cover
game <- click.on.board(c(13, 12), "left", game)
game$cover
game <- click.on.board(c(14, 12), "left", game)
game$cover
game <- click.on.board(c(15, 12), "right", game)
game$cover
game <- click.on.board(c(16, 12), "left", game)
game$cover
game <- click.on.board(c(16, 11), "right", game)
game$cover
game <- click.on.board(c(15, 11), "right", game)
game$cover
game <- click.on.board(c(8, 12), "left", game)
game$cover
game <- click.on.board(c(9, 12), "right", game)
game$cover
game <- click.on.board(c(12, 11), "right", game)
game$cover
game <- click.on.board(c(13, 11), "left", game)
game$cover
game <- click.on.board(c(14, 11), "right", game)
game$cover
game <- click.on.board(c(11, 11), "left", game)
game$cover
game <- click.on.board(c(10, 11), "left", game)
game$cover
game <- click.on.board(c(9, 11), "left", game)
game$cover
game <- click.on.board(c(8, 11), "left", game)
game$cover
game <- click.on.board(c(8, 10), "right", game)
game$cover
game <- click.on.board(c(9, 10), "left", game)
game$cover
game <- click.on.board(c(10, 10), "left", game)
game$cover
game <- click.on.board(c(11, 10), "right", game)
game$cover
game <- click.on.board(c(12, 10), "left", game)
game$cover
game <- click.on.board(c(4, 9), "right", game)
game$cover
game <- click.on.board(c(5, 9), "left", game)
game$cover
game <- click.on.board(c(6, 9), "right", game)
game$cover
game <- click.on.board(c(7, 9), "left", game)
game$cover
game <- click.on.board(c(8, 9), "left", game)
game$cover
game <- click.on.board(c(4, 8), "left", game)
game$cover
game <- click.on.board(c(5, 8), "left", game)
game$cover
game <- click.on.board(c(6, 8), "left", game)
game$cover
game <- click.on.board(c(4, 7), "left", game)
game$cover
game <- click.on.board(c(2, 6), "right", game)
game$cover
game <- click.on.board(c(1, 6), "left", game)
game$cover
game <- click.on.board(c(1, 5), "left", game)
game$cover
game <- click.on.board(c(2, 5), "left", game)
game$cover
game <- click.on.board(c(1, 4), "left", game)
game$cover
game <- click.on.board(c(6, 1), "right", game)
game$cover
game <- click.on.board(c(7, 2), "right", game)
game$cover
game <- click.on.board(c(7, 1), "left", game)
game$cover
game <- click.on.board(c(8, 1), "left", game)
game$cover
game <- click.on.board(c(9, 1), "left", game)
game$cover
game <- click.on.board(c(8, 2), "left", game)
game$cover
game <- click.on.board(c(8, 3), "left", game)
game$cover
game <- click.on.board(c(9, 2), "left", game)
game$cover
game <- click.on.board(c(9, 3), "left", game)
game$cover
game <- click.on.board(c(7, 7), "right", game)
game$cover
game <- click.on.board(c(7, 8), "left", game)
game$cover
game <- click.on.board(c(8, 4), "right", game)
game$cover
game <- click.on.board(c(9, 4), "right", game)
game$cover
game <- click.on.board(c(8, 5), "left", game)
game$cover
game <- click.on.board(c(8, 6), "left", game)
game$cover
game <- click.on.board(c(8, 7), "left", game)
game$cover
game <- click.on.board(c(8, 8), "right", game)
game$cover
game <- click.on.board(c(9, 9), "right", game)
game$cover
game <- click.on.board(c(9, 8), "right", game)
game$cover
game <- click.on.board(c(9, 7), "left", game)
game$cover
game <- click.on.board(c(9, 6), "left", game)
game$cover
game <- click.on.board(c(9, 5), "left", game)
game$cover
game <- click.on.board(c(10, 4), "left", game)
game$cover
game <- click.on.board(c(10, 5), "left", game)
game$cover
game <- click.on.board(c(10, 6), "left", game)
game$cover
game <- click.on.board(c(10, 9), "left", game)
game$cover
game <- click.on.board(c(11, 9), "left", game)
game$cover
game <- click.on.board(c(10, 8), "left", game)
game$cover
game <- click.on.board(c(10, 7), "right", game)
game$cover
game <- click.on.board(c(11, 8), "left", game)
game$cover
game <- click.on.board(c(11, 7), "left", game)
game$cover
game <- click.on.board(c(11, 6), "left", game)
game$cover
game <- click.on.board(c(11, 5), "left", game)
game$cover
game <- click.on.board(c(11, 4), "left", game)
game$cover
game <- click.on.board(c(10, 1), "right", game)
game$cover
game <- click.on.board(c(10, 3), "right", game)
game$cover
game <- click.on.board(c(10, 2), "left", game)
game$cover
game <- click.on.board(c(11, 1), "left", game)
game$cover
game <- click.on.board(c(11, 2), "left", game)
game$cover
game <- click.on.board(c(11, 3), "left", game)
game$cover
game <- click.on.board(c(12, 1), "left", game)
game$cover
game <- click.on.board(c(12, 2), "left", game)
game$cover
game <- click.on.board(c(12, 3), "left", game)
game$cover
game <- click.on.board(c(12, 4), "left", game)
game$cover
game <- click.on.board(c(13, 2), "right", game)
game$cover
game <- click.on.board(c(13, 1), "left", game)
game$cover
game <- click.on.board(c(14, 2), "right", game)
game$cover
game <- click.on.board(c(14, 1), "left", game)
game$cover
game <- click.on.board(c(15, 1), "left", game)
game$cover
game <- click.on.board(c(16, 1), "left", game)
game$cover
game <- click.on.board(c(15, 3), "right", game)
game$cover
game <- click.on.board(c(16, 3), "right", game)
game$cover
game <- click.on.board(c(15, 4), "left", game)
game$cover
game <- click.on.board(c(15, 5), "right", game)
game$cover
game <- click.on.board(c(16, 4), "left", game)
game$cover
game <- click.on.board(c(16, 5), "left", game)
game$cover
game <- click.on.board(c(16, 6), "right", game)
game$cover
game <- click.on.board(c(15, 6), "right", game)
game$cover
game <- click.on.board(c(14, 6), "left", game)
game$cover
game <- click.on.board(c(13, 6), "left", game)
game$cover
game <- click.on.board(c(12, 6), "right", game)
game$cover
game <- click.on.board(c(12, 7), "left", game)
game$cover
game <- click.on.board(c(12, 8), "left", game)
game$cover
game <- click.on.board(c(12, 9), "right", game)
game$cover
game <- click.on.board(c(13, 7), "left", game)
game$cover
game <- click.on.board(c(13, 8), "left", game)
game$cover
game <- click.on.board(c(13, 9), "left", game)
game$cover
game <- click.on.board(c(14, 7), "left", game)
game$cover
game <- click.on.board(c(14, 8), "left", game)
game$cover
game <- click.on.board(c(14, 9), "left", game)
game$cover
game <- click.on.board(c(13, 10), "right", game)
game$cover
game <- click.on.board(c(14, 10), "left", game)
game$cover
game <- click.on.board(c(15, 10), "left", game)
game$cover
game <- click.on.board(c(16, 10), "left", game)
game$cover
game <- click.on.board(c(16, 9), "left", game)
game$cover
game <- click.on.board(c(15, 9), "left", game)
game$cover
game <- click.on.board(c(15, 8), "right", game)
game$cover
game <- click.on.board(c(16, 8), "left", game)
game$cover
game <- click.on.board(c(16, 7), "left", game)
game$cover
game <- click.on.board(c(15, 7), "left", game)
game$cover
game <- click.on.board(c(3, 3), "right", game)
game$cover
# HAVE TO GUESS
game <- click.on.board(c(16, 30), "left", game)
game$cover
game <- click.on.board(c(15, 30), "left", game)
game$cover
game <- click.on.board(c(14, 30), "right", game)
game$cover
game <- click.on.board(c(13, 30), "left", game)
game$cover
game <- click.on.board(c(12, 30), "left", game)
game$cover
game <- click.on.board(c(11, 30), "right", game)
game$cover
game <- click.on.board(c(10, 30), "right", game)
game$cover
game <- click.on.board(c(9, 30), "left", game)
game$cover
game <- click.on.board(c(9, 29), "left", game)
game$cover
# HAVE TO GUESS
game <- click.on.board(c(1, 30), "left", game)
game$cover
game <- click.on.board(c(3, 30), "right", game)
game$cover
game <- click.on.board(c(3, 29), "right", game)
game$cover
game <- click.on.board(c(4, 29), "left", game)
game$cover
# HAVE TO GUESS
game <- click.on.board(c(4, 30), "right", game)
game$cover
game <- click.on.board(c(5, 29), "left", game)
game$cover
game <- click.on.board(c(5, 30), "left", game)
game$cover
game <- click.on.board(c(6, 30), "left", game)
game$cover
game <- click.on.board(c(6, 29), "left", game)
game$cover
game <- click.on.board(c(7, 29), "right", game)
game$cover
game <- click.on.board(c(7, 30), "right", game)
game$cover
game <- click.on.board(c(8, 29), "left", game)
