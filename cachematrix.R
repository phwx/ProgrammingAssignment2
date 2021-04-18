

##makeCacheMatrix loads a matrix and has the sub-functions setinv, getinv, set and get
##setinv saves a inverted matrix in caches (no caculation)
##getinv takes the displays the inverted function (if available)
##set can be used to change the initally loaded matrix
##get shows the currently loaded matrix


makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  setinv <- function(inv) y <<- inv
  getinv <- function() y
  set <- function(i){
    y <<- NULL
    x <<- i
  }
  get <- function() x
  list(setinv = setinv, getinv = getinv, get = get, set = set)
    }

##cacheSolve loads a function, in this case the makeCahceMatrix function
##it checks if there is a inverted matrix saved (through getinv function), if so, the inverted function is displayed
##if not the inverted function is calculated, saved (through setinv function) and returned
    
cacheSolve <- function(x, ...){
  y <- x$getinv()
  if(!is.null(y)){
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data)
  x$setinv(y)
  return(y)
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m2 <- matrix(c(1/2, -1/4, -1, 3/4, 4, 7, 0, 17, 1), nrow = 3, ncol = 3)

## those are my test cases

test <- makeCacheMatrix(m1) 
test$get()
test$getinv()      ##no Invertd Matrix has been calculated yet, therefore no answer expected
test$set(m1)
cacheSolve(test)
cacheSolve(test)   ##second time cacheSolve to see if its calculated or taken out of the "cache" therefore the message "getting cached data"
test$getinv()      ##as Inverted Matrix was calcualted and saved with function cacheSolve, now here is an answer expected
test$set(m2)       ##Loading new Matrix M2
cacheSolve(test)   ##Same Game as above just with M2
test$getinv        ##Same Game as above just with M2