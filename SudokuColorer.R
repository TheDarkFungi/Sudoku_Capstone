color_palette <- c("white", "pink", "tomato1", "orange", "yellow", "green", "darkcyan", "turquoise", "cornflowerblue", "mediumpurple1")
vertex_colors <- character(81)
load_board <- function(boardID){
  for (i in 1:81) {
    cell_value <- boardID[i]
    vertex_colors[i] <<- color_palette[cell_value + 1]
  }
  vertex_labels <<- as.character(boardID)
  vertex_labels[vertex_labels == "0"] <<- ''
}

print_loaded_board_val <- function(){
  outputBoard <- as.integer(vertex_labels)
  outputBoard[is.na(outputBoard)] <- 0
  outputBoard <- paste0(outputBoard, collapse = ",")
  return(outputBoard)
}

plot_loaded_board <- function(){
  plot(
    board, 
    layout = fixed_layout, 
    vertex.color = vertex_colors,
    vertex.label = vertex_labels,
    edge.curved = edge_curvatures
  )
}

plot_subBoard <- function(subBoard){
  #subBoard is a list(board = sub_board, layout = sub_layout, colors = sub_colors, labels = sub_labels, answerKey = fullBoardID)
  plot(
    subBoard$board, 
    layout = subBoard$layout, 
    vertex.color = subBoard$colors,
    vertex.label = subBoard$labels,
    edge.curved = 0
  )
}
