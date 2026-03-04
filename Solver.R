# This file is for solving three-colored subgraphs.
# For two colored subgraphs, determining solvability is as easy as 
#         "does each component have at least one colored vertex"
# Having all two-colored subgraphs be solvable is a REQUIREMENT for the entire
# board be solvable. Therefore, one of my hypothesis was that all three colored
# subgraphs also needed to be solvable. I wrote these functions to check that.
# The hypothesis was proven to be false.

#threeColorSolver()
# Given a three colored subgraph, determines if the graph can be uniquely colored.
# If plot@ is set to true, it plots every step of its iterations.
threeColorSolver <- function(subBoardG, plotQ = FALSE){
  #subBoardID is a list(board = sub_board, layout = sub_layout, colors = sub_colors, labels = sub_labels, answerKey = fullBoardID)
  coloredCells <- as.integer(subBoardG$labels)
  colorSet <- unique(coloredCells)
  colorSet <- colorSet[!is.na(colorSet)]
  coloredCells[is.na(coloredCells)] <- 0
  
  # print(coloredCells)
  
  workBoard <- subBoardG$board
  V(workBoard)$color <- coloredCells
  if (plotQ){
    V(workBoard)$labels <- coloredCells
    V(workBoard)$labels[V(workBoard)$labels == 0] <- ''
  }
    
  trueColor <- subBoardG$answerKey[workBoard$ID]
  
  if (plotQ){
    plot(
      workBoard,
      layout = subBoardG$layout,
      vertex.color = color_palette[V(workBoard)$color + 1],
      vertex.label = V(workBoard)$labels,
      edge.curved = 0,
      main = sprintf("Iteration 0: ")
    )
  }
  
  hasChanged <- TRUE
  x <- 0
  while (hasChanged && x < 8){
    hasChanged <- FALSE
    iter <- 0
    x <- x+1
    for (cell in V(workBoard)){
      iter <- iter +1
      if (V(workBoard)$color[iter] == 0){
        cell_adjs <- neighbors(workBoard, cell)$color
        cell_adjs <- cell_adjs[cell_adjs != 0]
        if (length(unique(cell_adjs)) == 2){
          V(workBoard)[iter]$color = trueColor[iter]
          V(workBoard)[iter]$labels = trueColor[iter]
          hasChanged = TRUE
        }
      }
    }
    # print(sprintf("Iteration %d: ", x))
    # print(V(workBoard)$color)
    if (plotQ){
      plot(
        workBoard,
        layout = subBoardG$layout,
        vertex.color = color_palette[V(workBoard)$color + 1],
        vertex.label = V(workBoard)$labels,
        edge.curved = 0,
        main = sprintf("Iteration %d: ", x)
      )
    }
  }

  # print(V(workBoard)$color)
  # print(trueColor)
  return(V(workBoard)$color)
}

solveAllThrees <- function(boardID, cluesID){
  listOfFailiures <- list()
  for (i in 1:7){
    for (j in (i+1):8){
      for (k in (j+1):9){
        test <- generateClueSubBoardN(boardID, cluesID, c(i,j,k), FALSE)
        if (any(threeColorSolver(test) == 0)){
          listOfFailiures <- append(listOfFailiures, list(c(i,j,k)))
        }
      }
    }
  }
  return(listOfFailiures)
}
