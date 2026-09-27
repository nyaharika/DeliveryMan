manhattan <- function(x1, y1, x2, y2) {
  abs(x1 - x2) + abs(y1 - y2)
}

findPath <- function(startX, startY, targetX, targetY, roads) {
  boardSize <- nrow(roads$vroads)
  
  if (startX == targetX && startY == targetY) {
    return(5)
  }
  
  startH <- manhattan(startX, startY, targetX, targetY)
  startNode <- list(
    x = startX,
    y = startY,
    g = 0,
    h = startH,
    f = startH,
    first_move = 0
  )
  
  #frontier = discovered yet not explored, nodes = already explored
  frontier <- list(startNode)
  nodes <- list()
  
  #create and evaulate a neighboring node
  addNeighbor <- function(newX, newY, roadCost, moveNumber) {
    newG <- current$g + roadCost
    newH <- manhattan(newX, newY, targetX, targetY)
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
      sapply(nodes, function(item)
        item$x == newX && item$y == newY)
    )
    
    #if node already explored, ignore
    if (explored) {
      return()
    }
    matches <- sapply(
      frontier,
      function(item)
        item$x == newX && item$y == newY
    )
    
    if (!any(matches)){
      frontier[[length(frontier) + 1]] <<- newNode
      return()
    }
    
    i <- which(matches)[1]
    
    #keep the cheaper node
    if(newG < frontier[[i]]$g){
      frontier[[i]]<<- newNode
    }
  }
  while (length(frontier) > 0) {
    scores <- sapply(frontier, function(item) item$f)
    best <- which.min(scores)
    current <- frontier[[best]]
    frontier <- frontier[-best]
    
    if (current$x == targetX &&
        current$y == targetY) {
      return(current$first_move)
    }
    
    nodes[[length(nodes) + 1]] <- current
    
    if (current$x < boardSize) {
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
    
    if (current$y < boardSize) {
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
  
  return(5)
}

#decide which package or located to target, use A*to get there
myFunction <- function(roads, car, packages) {
  if (car$load == 0) {
    available_packages <- which(packages[, 5] == 0)
    
    pickup_distances <- sapply(
      available_packages,
      function(p)
        manhattan(
          car$x,
          car$y,
          packages[p, 1],
          packages[p, 2]
        )
    )
    
    targetPackage <- available_packages[which.min(pickup_distances)]
    targetX <- packages[targetPackage, 1]
    targetY <- packages[targetPackage, 2]
  } else {
    targetPackage <- car$load
    targetX <- packages[targetPackage, 3]
    targetY <- packages[targetPackage, 4]
  }
  
  car$nextMove <- findPath(
    car$x,
    car$y,
    targetX,
    targetY,
    roads
  )
  
  return(car)
}