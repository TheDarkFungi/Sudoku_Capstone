


example_subBoard <- generateSubBoard(sudoku_full_003, 7,9)$board



load_board(sudoku_solvable_103)
par(mfrow=c(1,1))
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
