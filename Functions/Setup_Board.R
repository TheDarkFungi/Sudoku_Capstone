# Start here!
# This file sets up the formatting that will be used to display full graphs.
# (Subgraphs will define their own formatting, but still rely on an empty board
# to serve as a template.)
library("igraph")

#==============MAKING THE GRAPH===================
#get_pos()
# A function that takes in an integer (1-81) and returns the row, column, and box
# of the cell.
get_pos <- function(v) {
  row <- floor((v-1)/9) + 1
  col <- (v-1) %% 9 + 1 
  box_row <- floor((row-1)/3)+1
  box_col <- floor((col-1)/3)+1
  box <- (box_col) + (box_row-1)*3
  return(list(row = row, col = col, box = box, box_row = box_row, box_col = box_col))
}

#make_empty_sudoku()
# A function that returns an igraph graph of 81 nodes, connected like a sudoku board.
# Each node will be given an ID from 1 to 81, read left to right, top to bottom.
# When plotting a full board, plot the output of this function, and 
#   set layout, vertex.labels, and edge.curved manually.
make_empty_sudoku <- function(){
  g <- make_empty_graph(81, directed = FALSE)
  all_edges <- list()
  for (v1 in 1:80){
    pos1 <- get_pos(v1)
    for (v2 in (v1+1):81){
      pos2 <- get_pos(v2)
      if (pos1$row == pos2$row || pos1$col == pos2$col || pos1$box == pos2$box){
        all_edges[[length(all_edges)+1]] <- c(v1, v2)
      }
    }
  }
  g <- add_edges(g, unlist(all_edges))
  V(g)$ID <- 1:81
  return (g)
}

#make_empty_sudoku_opt()
# Gives the option to remove types of adjacencies on a sudoku board.
# Was a feature I coded for a rabbit hole that didn't end up going anywhere.
# make_empty_sudoku_opt() will function identically to make_empty_sudoku().
make_empty_sudoku_opt <- function(box = TRUE, row = TRUE, col = TRUE){
  g <- make_empty_graph(81, directed = FALSE)
  all_edges <- list()
  for (v1 in 1:80){
    pos1 <- get_pos(v1)
    for (v2 in (v1+1):81){
      pos2 <- get_pos(v2)
      if (((row || pos1$box_col == pos2$box_col) && pos1$row == pos2$row) || 
          ((col || pos1$box_row == pos2$box_row) && pos1$col == pos2$col) || 
          (box && pos1$box == pos2$box)){
        all_edges[[length(all_edges)+1]] <- c(v1, v2)
      }
    }
  }
  g <- add_edges(g, unlist(all_edges))
  V(g)$ID <- 1:81
  return (g)
}
#======================MADE GRAPH==================
#Board should be used as a static, global variable. No need to change it.
board <- make_empty_sudoku()

# These make the edges curve outwards as the nodes they connect are further away.
# I found it to be the cleanest-looking display solution that still lets you see
# each individual edge.
# Edge Curvatures should be used as a static, global variable. No need to change it.
edges <- as_edgelist(board)
edge_curvatures <- rep(0, ecount(board))
for (i in 1:ecount(board)){
  pos1 <- get_pos(edges[i,1])
  pos2 <- get_pos(edges[i,2])
  edge_curvatures[i] <- 0.04 * (abs(pos1$row-pos2$row) + abs(pos1$col-pos2$col)-1) * ((abs(pos1$row-pos2$row) + abs(pos1$col-pos2$col)) %% 2 - 0.5)
}

# This will fix the positions of the nodes to a 9x9 grid.
# Should be used as a static, global variable. No need to change it.
fixed_layout <- layout_on_grid(board, width = 9)
fixed_layout[,2] <- -fixed_layout[,2] 
