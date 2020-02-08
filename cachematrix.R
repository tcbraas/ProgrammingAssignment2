#PROGRAMMING ASSIGNMENT 2, THOMAS BRAAS

#lexical scoping rules:
#Global environment > packages > base package
#free variables - values of free variables searched for in environment in which
#the function was defined. 


#testable testmatrix (inversable)
testmatrix <- matrix(runif(16), 4, 4)
testmatrix      
solve(testmatrix)

#first function - creates four functions and a cashable object for a given 
#(if inversable) matrix.
makeCacheMatrix <- function(u = matrix()) {
      #cashable object
      r <- NULL
      set <- function(p) {
            u <<- p
            r <<- NULL
      }
      get <- function() u
      setsolve <- function(solve) r <<- solve
      getsolve <- function() 
            r
            list(set = set, get = get, 
                 setsolve = setsolve, getsolve = getsolve)
}

class(makeCacheMatrix)

#solve-function using new matrix or returning cashed solve-function
cacheSolve <- function(u, ...) {
      r <- u$getsolve()
      if(!is.null(r)) {
            message("getting cashed data again")
            return(r)
      }
      data <- u$get()
      r <- solve(data, ...)
      u$setsolve(r)
      r
}

class(cacheSolve)


#functions work
cacheSolve(makeCacheMatrix(testmatrix))
#and now with cached-functionality
h <- makeCacheMatrix(testmatrix)
cacheSolve(h)
cacheSolve(h)


#test to see how much quicker the cashed version is
testmatrix1 <- matrix(runif(64), 8, 8)
solve(testmatrix1)
v <- makeCacheMatrix(testmatrix1)
cacheSolve(v)
cacheSolve(v)
#honestly doesn't make that much of a difference here but fair enough
testmatrix2 <- matrix(runif(144), 12, 12)
solve(testmatrix2)
w <- makeCacheMatrix(testmatrix2)
cacheSolve(w)
cacheSolve(w)
