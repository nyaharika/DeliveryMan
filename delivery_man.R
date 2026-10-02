manhattan <- function(x1, y1, x2, y2) {
  abs(x1 - x2) + abs(y1 - y2)
}

findPath <- function(start_x, start_y, target_x, target_y, roads) {
  boardSize <- nrow(roads$vroads)
  
  if (start_x == target_x && start_y == target_y) {
    return(5)
  }
  
  start_h <- manhattan(start_x, start_y, target_x, target_y) #heuristic at start
  startNode <- list(
    x = start_x,
    y = start_y,
    g = 0,
    h = start_h,
    f = start_h,
    first_move = 0
  )
  
  # frontier = discovered but not yet explored, nodes = already explored
  frontier <- list(startNode)
  nodes <- list()
  
  # create and evaluate a neighboring node
  addNeighbor <- function(new_x, new_y, roadCost, moveNumber) {
    new_g <- current$g + roadCost
    new_h <- manhattan(new_x, new_y, target_x, target_y)
    new_f <- new_g + new_h
    
    if (current$first_move == 0) {
      initial_move <- moveNumber
    } else {
      initial_move <- current$first_move
    }
    
    newNode <- list(
      x = new_x,
      y = new_y,
      g = new_g,
      h = new_h,
      f = new_f,
      first_move = initial_move
    )
    
    explored <- any(
      sapply(nodes, function(item)
        item$x == new_x && item$y == new_y)
    )
    
    #if node already explored, ignore
    if (explored) {
      return()
    }
    matches <- sapply(
      frontier,
      function(item)
        item$x == new_x && item$y == new_y
    )
    
    if (!any(matches)){
      frontier[[length(frontier) + 1]] <<- newNode
      return()
    }
    
    i <- which(matches)[1]
    
    #keep the cheaper node
    if(new_g < frontier[[i]]$g){
      frontier[[i]]<<- newNode
    }
  }
  
  # Explore frontier nodes in order of lowest f = g + h
  while (length(frontier) > 0) {
    scores <- sapply(frontier, function(item) item$f)
    best <- which.min(scores)
    current <- frontier[[best]]
    frontier <- frontier[-best]
    
    if (current$x == target_x &&
        current$y == target_y) {
      return(current$first_move)
    }
    
    nodes[[length(nodes) + 1]] <- current
    
    # Generate valid neighboring nodes
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

# Decide which package/location to target, then use A* to move toward it
myFunction <- function(roads, car, packages) {
  if (car$load == 0) {
    available_packages <- which(packages[, 5] == 0)
    
    pickup_distances <- manhattan(
      car$x,
      car$y,
      packages[available_packages, 1],
      packages[available_packages, 2]
    )
    
    target_package <- available_packages[which.min(pickup_distances)]
    target_x <- packages[target_package, 1]
    target_y <- packages[target_package, 2]
  } else {
    target_package <- car$load
    target_x <- packages[target_package, 3]
    target_y <- packages[target_package, 4]
  }
  
  car$nextMove <- findPath(
    car$x,
    car$y,
    target_x,
    target_y,
    roads
  )
  
  return(car)
}