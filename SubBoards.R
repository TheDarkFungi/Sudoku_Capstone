#SubBoards.R
#Has all the tools to make sub-boards from larger Sudoku boards

#generateSubBoard
#OBSOLETE
#All functionality of this function is covered with more options by generateSubBoardN
#Takes in a board and two numbers
#Set plotQ to FALSE if you do not want it to be plotted immediately
#Returns the two-colored sub-board as:
#     list(board, layout, colors, labels)
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


#generateSubBoardN
#A version of generateSubBoard generalized to any amount of numbers
# target_values should be a vector of numbers
#Set plotQ to FALSE if you do not want it to be plotted immediately
#Returns the N-colored sub-board as:
#     list(board, layout, colors, labels, answerKey)
#answerKey is unneccessary here, but included so that the output of this function
#   matches the output of generateClueSubBoardN()
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

#generateClueSubBoardN
#takes in a full board, and then a set of clues on that board, and then a set of numbers
#returns the N-colored sub-board of those numbers for the full board, but with only the values
#   from the clue board filled in
#Set plotQ to FALSE if you do not want it to be plotted immediately
#Returns the N-colored sub-board as:
#     list(board, layout, colors, labels, answerKey)
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

#generateAllSubBoards()
#A simple function that simply iterates through every pair of numbers, and plots
#all of the two-colored sub-board for the input board
# 4 plots per page
#Note that this will set the plot default to 4 plots per page, I have not found 
#   a way to reset this automatically. Just manually run par(mfrow=c(1,1)) afterwards to fix
generateAllSubBoards <- function(boardID){
  par(mfrow=c(2,2))
  for (i in 1:8){
    for (j in (i+1):9){
      generateSubBoard(boardID, i, j)
      #OBSOLETE! Should be: generateSubBoardN(boardID, c(i,j))
    }
  }
}

#generateALlDisCoSubBoard()
#Same as the previous function, but ONLY plots the subBoards that are disconnected.
generateAllDisCoSubBoard <- function(boardID){
  par(mfrow=c(2,2))
  for (i in 1:8){
    for (j in (i+1):9){
      curBoard <- generateSubBoard(boardID, i, j, FALSE)
      #OBSOLETE! Should be: curBoard <- generateSubBoardN(boardID, c(i,j), FALSE)
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
