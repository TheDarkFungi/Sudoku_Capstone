library("igraph")

#==============MAKING THE GRAPH===================
get_pos <- function(v) {
  row <- floor((v-1)/9) + 1
  col <- (v-1) %% 9 + 1 
  box_row <- floor((row-1)/3)+1
  box_col <- floor((col-1)/3)+1
  box <- (box_col) + (box_row-1)*3
  return(list(row = row, col = col, box = box, box_row = box_row, box_col = box_col))
}

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
  return (g)
}

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
  return (g)
}
#======================MADE GRAPH==================
board <- make_empty_sudoku()
board_no_box <- make_empty_sudoku_no_box()

edge_curvatures <- rep(0, ecount(board))

edges <- as_edgelist(board)

for (i in 1:ecount(board)){
  pos1 <- get_pos(edges[i,1])
  pos2 <- get_pos(edges[i,2])
  edge_curvatures[i] <- 0.04 * (abs(pos1$row-pos2$row) + abs(pos1$col-pos2$col)-1) * ((abs(pos1$row-pos2$row) + abs(pos1$col-pos2$col)) %% 2 - 0.5)
}

example_subBoard <- generateSubBoard(sudoku_full_003, 7,9)$board

V(board)$ID <- 1:81

fixed_layout <- layout_on_grid(board, width = 9)
fixed_layout[,2] <- -fixed_layout[,2] 

boardConnectedness <- aggregateConnectedness(sudoku_full_003)

load_board(sudoku_solvable_103)
par(mfrow=c(2,2))
plot(
  board, 
  layout = fixed_layout, 
  vertex.color = vertex_colors,
  vertex.label = vertex_labels,
  edge.curved = edge_curvatures
  )

plot(
  board_no_box,
  layout = fixed_layout,
  vertex.color = vertex_colors,
  vertex.label = vertex_labels,
  edge.curved = edge_curvatures
)
