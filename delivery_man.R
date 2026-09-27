myFunction <- function(roads, car, packages) {
  
  if (car$load == 0) {
    
    available <- which(packages[, 5] == 0)
    
    distances <- abs(packages[available, 1] - car$x) +
      abs(packages[available, 2] - car$y)
    
    targetPackage <- available[which.min(distances)]
    
    targetX <- packages[targetPackage, 1]
    targetY <- packages[targetPackage, 2]
    
  } else {
    
    targetPackage <- car$load
    
    targetX <- packages[targetPackage, 3]
    targetY <- packages[targetPackage, 4]
  }
  
  if (car$x == targetX && car$y == targetY) {
    car$nextMove <- 5
    return(car)
  }
  
  startH <- abs(targetX - car$x) +
    abs(targetY - car$y)
  
  startNode <- list(
    x = car$x,
    y = car$y,
    g = 0,
    h = startH,
    f = startH,
    first_move = 0
  )
  
  frontier <- list(startNode)
  nodes <- list()
  
  addNeighbor <- function(newX, newY, roadCost, moveNumber) {
    
    newG <- current$g + roadCost
    
    newH <- abs(targetX - newX) +
      abs(targetY - newY)
    
    newF <- newG + newH
    
    if (current$first_move == 0) {
      firstMove <- moveNumber
    } else {
      firstMove <- current$first_move
    }
    
    newNode <- list(
      x = newX,
      y = newY,
      g = newG,
      h = newH,
      f = newF,
      first_move = firstMove
    )
    
    explored <- any(
      sapply(
        nodes,
        function(item)
          item$x == newX && item$y == newY
      )
    )
    
    if (!explored) {
      
      matches <- sapply(
        frontier,
        function(item)
          item$x == newX && item$y == newY
      )
      
      if (!any(matches)) {
        
        frontier[[length(frontier) + 1]] <<- newNode
        
      } else {
        
        i <- which(matches)[1]
        
        if (newG < frontier[[i]]$g) {
          frontier[[i]] <<- newNode
        }
      }
    }
  }
  
  while (length(frontier) > 0) {
    
    scores <- sapply(
      frontier,
      function(item) item$f
    )
    
    best <- which.min(scores)
    
    current <- frontier[[best]]
    
    frontier <- frontier[-best]
    
    if (current$x == targetX &&
        current$y == targetY) {
      
      car$nextMove <- current$first_move
      return(car)
    }
    
    nodes[[length(nodes) + 1]] <- current
    
    if (current$x < 10) {
      addNeighbor(
        current$x + 1,
        current$y,
        roads$hroads[current$x, current$y],
        6
      )
    }
    
    if (current$x > 1) {
      addNeighbor(
        current$x - 1,
        current$y,
        roads$hroads[current$x - 1, current$y],
        4
      )
    }
    
    if (current$y < 10) {
      addNeighbor(
        current$x,
        current$y + 1,
        roads$vroads[current$x, current$y],
        8
      )
    }
    
    if (current$y > 1) {
      addNeighbor(
        current$x,
        current$y - 1,
        roads$vroads[current$x, current$y - 1],
        2
      )
    }
  }
  
  car$nextMove <- 5
  return(car)
}