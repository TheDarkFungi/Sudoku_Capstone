library("igraph")

make_sudoku <- function(size) {
  s <- make_(lattice(length = 9, dim = 2), with_vertex_(name = as.character(1:81)))
  s1 <- make_(full_graph(9), with_vertex_(name = as.character(1:9)))
  s2 <- make_(full_graph(9), with_vertex_(name = as.character(10:18)))
  s <- s+s1
  return (s)
}

a <- make_(full_graph(4), with_vertex_(name = as.character(1:4)))

for (i in 1:3){
  a <- a + make_(full_graph(4))
}
a <- set_vertex_attrs(a, name = as.character(1:16))
plot(a, main = "4x4 Sudoku", layout = layout_on_grid(a, width = 4), edge.curved=0.2)
a <- a+make_(full_graph(4), with_vertex_(name = as.character(seq(1,13,4))))
a <- a+make_(full_graph(4), with_vertex_(name = as.character(seq(2,14,4))))
a <- a+make_(full_graph(4), with_vertex_(name = as.character(seq(3,15,4))))
a <- a+make_(full_graph(4), with_vertex_(name = as.character(seq(4,16,4))))
a <- a+make_(full_graph(4), with_vertex_(name = c('1','2','5','6')))
a <- a+make_(full_graph(4), with_vertex_(name = c('3','4','7','8')))
a <- a+make_(full_graph(4), with_vertex_(name = c('9','10','13','14')))
a <- a+make_(full_graph(4), with_vertex_(name = c('11','12','15','16')))

test <- make_(lattice(length = 4, dim = 2), with_vertex_(name = seq(from = 1, to = 16, by = 1)))

plot(a)
plot(a + b)

layout

sudoku <- make_sudoku()
grid <- layout_on_grid(sudoku, width = 9)
plot(sudoku, layout = grid, edge.curved = 0.3)
