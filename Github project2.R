makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL  
  evn <- environment()  
  y<-NULL 
  setmatrix<-function(y){  
    x<<-y 
    m<<-NULL 
  }
  
  getmatrix<-function() x 
  setinverse<-function(solve) m<<- solve 
  getinverse<-function() m
  getenv<- function() environment()
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

cacheSolve <- function(xMat= m(), ...) {
  m <- xMat$getinverse() 
  if(!is.null(m)){
    if(xMat$setmatrix() == xMat$getmatrix()) {
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    y <- xMat$getmatrix() 
    xMat$setmatrix(y) 
    m <- solve(y, ...) 
    xMat$setinverse(m) 
    m 
  }
}
