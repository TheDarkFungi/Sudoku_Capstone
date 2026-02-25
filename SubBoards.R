
generateSubBoard <- function(boardID, c1, c2, plotQ = TRUE){
  load_board(boardID)
  target_values <- c(c1,c2)
  vec_to_keep <- which(boardID %in% target_values)
  sub_board <- induced_subgraph(board, vec_to_keep)
  sub_layout <- fixed_layout[vec_to_keep, ]
  sub_colors <- vertex_colors[vec_to_keep]
  sub_labels <- vertex_labels[vec_to_keep]
  sub_board$ID <- vec_to_keep
  
  if (plotQ){
    plot(sub_board,
         layout = sub_layout,
         vertex.color = sub_colors,
         vertex.label = sub_labels,
         edge.curved = 0,
         main = sprintf("Subgraph of %d and %d", c1, c2)
    )
  }
  return(list(board = sub_board, layout = sub_layout, colors = sub_colors, labels = sub_labels))
}

generateSubBoardN <- function(boardID, target_values, plotQ = TRUE){
  load_board(boardID)
  N <- length(target_values)
  vec_to_keep <- which(boardID %in% target_values)
  sub_board <- induced_subgraph(board, vec_to_keep)
  sub_layout <- fixed_layout[vec_to_keep, ]
  sub_colors <- vertex_colors[vec_to_keep]
  sub_labels <- vertex_labels[vec_to_keep]
  #sub_labels <- degree(sub_board)
  sub_board$ID <- vec_to_keep
  
  if (plotQ){
    title <- paste(target_values[1:(N-1)], collapse = ", ")
    title <- paste0(title, ", and ", target_values[N])
    title <- paste("Subgraph of", title)
    plot(sub_board,
         layout = sub_layout,
         vertex.color = sub_colors,
         edge.curved = 0,
         vertex.label = sub_labels,
         main = title
    )
  }
  return(list(board = sub_board, layout = sub_layout, colors = sub_colors, labels = sub_labels, answerKey = boardID))
}

generateClueSubBoardN <- function(fullBoardID, clueBoardID, target_values, plotQ = TRUE){
  load_board(fullBoardID)
  N <- length(target_values)
  vec_to_keep <- which(fullBoardID %in% target_values)
  sub_board <- induced_subgraph(board, vec_to_keep)
  sub_layout <- fixed_layout[vec_to_keep, ]
  sub_board$ID <- vec_to_keep
  
  load_board(clueBoardID)
  sub_colors <- vertex_colors[vec_to_keep]
  sub_labels <- vertex_labels[vec_to_keep]
  
  if (plotQ){
    title <- paste(target_values[1:(N-1)], collapse = ", ")
    title <- paste0(title, ", and ", target_values[N])
    title <- paste("Subgraph of", title)
    plot(sub_board,
         layout = sub_layout,
         vertex.color = sub_colors,
         edge.curved = 0,
         vertex.label = sub_labels,
         main = title
    )
  }
  return(list(board = sub_board, layout = sub_layout, colors = sub_colors, labels = sub_labels, answerKey = fullBoardID))
}

generateAllSubBoards <- function(boardID){
  par(mfrow=c(2,2))
  for (i in 1:8){
    for (j in (i+1):9){
      generateSubBoard(boardID, i, j)
    }
  }
}

generateAllDisCoSubBoard <- function(boardID){
  par(mfrow=c(2,2))
  for (i in 1:8){
    for (j in (i+1):9){
      curBoard <- generateSubBoard(boardID, i, j, FALSE)
      if (!is_connected(curBoard$board)){
        plot(curBoard$board,
          layout = curBoard$layout,
          vertex.color = curBoard$colors,
          edge.curved = 0,
          vertex.label = curBoard$labels,
          main = sprintf("Subgraph of %d and %d", i, j)
        )
      }
    }
  }
}

calculateConnectedness <- function(subBoard){
  tempBoard <- empty_board
  boardComponents <- components(subBoard)
  compID <- boardComponents$membership
  comp_sizes <- boardComponents$csize
  for (i in 1:vcount(subBoard)){
    tempBoard[V(subBoard)$ID[i]] <- comp_sizes[compID[i]]/2 -2
  }
  return(tempBoard)
}

aggregateConnectedness <- function(boardID){
  initBoard <- empty_board
  for (i in 1:8){
    for (j in (i+1):9){
      temp_board <- generateSubBoard(boardID, i, j, FALSE)$board
      initBoard <- initBoard + calculateConnectedness(temp_board)
    }
  }
  return(initBoard)
}
