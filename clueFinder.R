bruteClues <- function(boardID){
  outputBoard <- rep(0,81)
  outputColors <- rep("white", 81)
  
  constraints <- list()
  constrPtr <- 1
  
  for (i in 1:8){
    for (j in (i+1):9){
      currentSubgraph <- generateSubBoard(boardID, i, j, FALSE)$board
      comp_list <- components(currentSubgraph)
      for (comp_num in 1:(comp_list$no)){
        cells_in_comp <- which(comp_list$membership == comp_num)
        cellIDs <- V(currentSubgraph)[cells_in_comp]$ID
        constraints[[constrPtr]] <- cellIDs
        constrPtr <- constrPtr +1 
      }
    }
  }
  
  componentBoard <- check4AllComponents(boardID)
  for (vh in 1:2){
    for (b in 0:2){
      for (s1 in 1:2){
        for (s2 in (s1+1):3){
          
          comp_list <- componentBoard[sprintf("[%d,%d]",(b*3)+s1,(b*3)+s2),,vh]
          for (k in 1:max(comp_list)){
            cells_in_strip_comp <- unname(which(comp_list == k)) #Returns places in strip
            if (length(cells_in_strip_comp) > 0){
              if (vh == 1){ #Rows
                  cellIDs <- c(((b*3)+s1-1)*9+cells_in_strip_comp, #1st Row
                               ((b*3)+s2-1)*9+cells_in_strip_comp) #2nd Row
              } else if (vh == 2){ #Cols
                cellIDs <- c((b*3)+s1+(cells_in_strip_comp-1)*9, #1st Col
                             (b*3)+s2+(cells_in_strip_comp-1)*9) #2nd Col
              }
              constraints[[constrPtr]] <- cellIDs
              constrPtr <- constrPtr +1
            }
          }
          # print(sprintf("[%d,%d], 1...9, %d",(b*3)+s1,(b*3)+s2, vh))
          
        }
      }
    }
  }
  
  print(constraints)
  
  #Constraint-Meeting algorithm goes here:
  clueBools <- generateConstrainedSet(constraints)
  
  for (x in 1:81){
    if (clueBools[x]){
      outputBoard[x] <- boardID[x]
    }
  }
  
  return(outputBoard)
}

check4ComponentsNRoC <- function(boardID, option, s1, s2){
  if (floor(s1/3) != floor(s2/3)){
    errorCondition("Strips in different boxes")
  }
  if (option == "R"){
    strip1 <- boardID[((s1-1)*9+1):(s1*9)]
    strip2 <- boardID[((s2-1)*9+1):(s2*9)]
  }
  else if (option == "C"){
    strip1 <- boardID[(0:8)*9+s1]
    strip2 <- boardID[(0:8)*9+s2]
  }
  #print(strip1)
  #print(strip2)
  stripCount <- 1
  outputStrip <- rep(stripCount,9)
  foundColors <- rep(0,9)
  
  
  for (i in 1:8){if (foundColors[i]==0){
    for (j in (i+1):9){if (foundColors[i]==0){
        positions1 <- which(strip1 == i | strip1 == j)
        positions2 <- which(strip2 == i | strip2 == j)
        if (identical(positions1,positions2)){
          #print(sprintf("%s: [%d,%d]- values: %d, %d",option, s1, s2, i,j))
          stripCount <- stripCount + 1
          outputStrip[positions1] <- stripCount
          foundColors[i]=i
          foundColors[j]=j
        }
        
    }}
  }}
  for (i in 1:7){if (foundColors[i]==0){
    for (j in (i+1):8){if (foundColors[j]==0){
      for (k in (j+1):9){if (foundColors[k]==0){
        positions1 <- which(strip1 == i | strip1 == j | strip1 == k)
        positions2 <- which(strip2 == i | strip2 == j | strip2 == k)
        if (identical(positions1,positions2)){
          #print(sprintf("%s: [%d,%d]- values: %d, %d, %d",option, s1, s2, i,j,k))
          stripCount <- stripCount + 1
          outputStrip[positions1] <- stripCount
          foundColors[i]=i
          foundColors[j]=j
          foundColors[k]=k
        }
      }}
    }}
  }}
  for (i in 1:6){if (foundColors[i]==0){
    for (j in (i+1):7){if (foundColors[j]==0){
      for (k in (j+1):8){if (foundColors[k]==0){
        for (l in (k+1):9){if (foundColors[l]==0){
          #print(sprintf("Entered- %s: [%d,%d]- values: %d, %d, %d, %d",option, s1, s2, i,j,k,l))
          positions1 <- which(strip1 == i | strip1 == j | strip1 == k | strip1 == l)
          positions2 <- which(strip2 == i | strip2 == j | strip2 == k | strip2 == l)
          if (identical(positions1,positions2)){
            #print(sprintf("%s: [%d,%d]- values: %d, %d, %d, %d",option, s1, s2, i,j,k,l))
            stripCount <- stripCount + 1
            outputStrip[positions1] <- stripCount
            foundColors[i]=i
            foundColors[j]=j
            foundColors[k]=k
            foundColors[l]=l
          }
        }}#else print(sprintf("Did not enter- %s: [%d,%d]- values: %d, %d, %d, (%d)",option, s1, s2, i,j,k,l))}
      }}#else print(sprintf("Did not enter- %s: [%d,%d]- values: %d, %d, (%d), -",option, s1, s2, i,j,k))}
    }}#else print(sprintf("Did not enter- %s: [%d,%d]- values: %d, (%d), -, -",option, s1, s2, i,j))}
  }}#else print(sprintf("Did not enter- %s: [%d,%d]- values: %d, -, -, -",option, s1, s2, i))}
  
  return(outputStrip)
}

check4AllComponents <- function(boardID){
  outputGrid <- array(rep(0,9*18), dim = c(9,9,2), dimnames = list(c("[1,2]","[1,3]","[2,3]", "[4,5]","[4,6]","[5,6]", "[7,8]","[7,9]","[8,9]"),1:9,c("Row","Col")))
                          
  for (b in 0:2){
    for (s1 in 1:2){
      for (s2 in (s1+1):3){
        #print(sprintf("%d, %d", s1, s2))
        outputGrid[b*3+s1+s2-2,,1] <- check4ComponentsNRoC(boardID, "R", (b*3)+s1, (b*3)+s2)
      }
    }
  }
  for (b in 0:2){
    for (s1 in 1:2){
      for (s2 in (s1+1):3){
        #print(sprintf("%d, %d", s1, s2))
        outputGrid[b*3+s1+s2-2,,2] <- check4ComponentsNRoC(boardID, "C", (b*3)+s1, (b*3)+s2)
      }
    }
  }
  return(outputGrid)
}

generateConstrainedSet <- function(listOfCons){
  # listOfCons is a list of lists. The interior lists contain integers from 1-81
  hashTable <- array(rep(FALSE, 81), dim = 81)
  # hashTable[unlist(sample(listOfCons[[1]],1))] <- TRUE
  
  for (subList in listOfCons){
    skip <- FALSE
    for (ele in subList){
      if (skip == FALSE && hashTable[ele] == TRUE){
        skip <- TRUE
      }
    }
    if (skip == FALSE){
      hashTable[unlist(sample(subList,1))] <- TRUE
    }
    # print(subList)
    # print(hashTable[subList])
  }
  
  return(hashTable)
}
