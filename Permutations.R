#permutationer()
# Will return a boardID with all instances of the two given colors swapped
permutationer <- function(boardID, c1, c2){
  if (c1 == c2){
    stop("Please give two different numbers")
  }
  else if (c1 > 9 | c2 > 9 | c1 < 1 | c2 < 1) {
    stop("Only accepts numbers 1-9")
  }
  else {
    outputBoard <- replace(boardID, boardID == c1, c2)
    outputBoard <- replace(outputBoard, boardID == c2, c1)
    return(outputBoard)
  }
}

#swapRows()
# Will return a boardID with the two given rows swapped
# r1 and r2 should be integers from 1-9, and be in the same "block"
swapRows <- function(boardID, r1, r2){
  if (floor(r1/3) != floor(r2/3)){
    errorCondition("Rows are in different blocks")
  }
  outputBoard <- boardID
  
  range1 <- ((r1-1) * 9 + 1) : (r1 * 9)
  range2 <- ((r2-1) * 9 + 1) : (r2 * 9)
  
  outputBoard[range2] <- boardID[range1]
  outputBoard[range1] <- boardID[range2]
  
  return(outputBoard)
}

#swapCols()
# Will return a boardID with the two given columns swapped
# c1 and c2 should be integers from 1-9, and be in the same "block"
swapCols <- function(boardID, c1, c2){
  if (floor(c1/3) != floor(c2/3)){
    errorCondition("Cols are in different blocks")
  }
  outputBoard <- boardID
  
  range1 <- seq(c1,81,9)
  range2 <- seq(c2,81,9)
  
  outputBoard[range2] <- boardID[range1]
  outputBoard[range1] <- boardID[range2]
  
  return(outputBoard)
}

#standardizeNumbers()
# Returns a board that is logically identical to your input board, but with the
# colors permuted in a way such that the top left box is in numerical order
standardizeNumbers <- function(boardID){
  count <- 1
  for (i in c(1,2,3,10,11,12,19,20,21)){
    outputBoard <- replace(outputBoard, boardID == boardID[i], count)
    count <- count + 1
  }
  return(outputBoard)
}
