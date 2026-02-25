permutationer <- function(boardID, c1, c2){
  if (c1 == c2){
    stop("Give two different numbers plz")
  }
  else if (c1 + c2 > 17) {
    stop("Only accepts numbers 1-9")
  }
  else {
    outputBoard <- replace(boardID, boardID == c1, c2)
    outputBoard <- replace(outputBoard, boardID == c2, c1)
    return(outputBoard)
  }
}

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

standardizeNumbers <- function(boardID){
  count <- 1
  for (i in c(1,2,3,10,11,12,19,20,21)){
    outputBoard <- replace(outputBoard, boardID == boardID[i], count)
    count <- count + 1
  }
  return(outputBoard)
}
