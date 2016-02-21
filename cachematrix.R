## Assignment2: 
## Write the following functions:
## 1.	makeCacheMatrix: 
##    This function creates a special "matrix" object that can cache its inverse.
## 2.	cacheSolve: 
##    This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

## To create special "YOUR_MATRIX", just enter "YOUR_MATRIX<-makeCacheMatrix".
## It gets added to a list, along with its inverse.
## YOUR_MATRIX and its inverse can be called up by entering "YOUR_MATRIX$get()" or(once solved) "YOUR_MATRIX$getinverse"
## Any number of matrices can be added to the list, but name them distinctly (e.g., YOUR_MATRIX2, YOUR_MATRIX3, etc.)

makeCacheMatrix <- function(x = matrix()) {
    nvrs <- NULL                               ## This first part sets inverse to NULL for a clear start and 
    set <- function(y) {                      ## sets the value for YOUR_MATRIX<-makeCacheMatrix  in the list.  
      xp <<- y                               ## the value of the matrix and its inverse will be held in the
      nvrs <<- NULL                         ## parent environment. The various matrices will not show up as data
    }                                      ## but rather as Values, as a List of 4: set, get, setinverse, getinverse.
    get <- function() { x }
    setinverse <- function(solve) {  
      nvrs<<-solve(x) }
    getinverse <- function() { nvrs }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## The "cacheSolve" function below is meant to work with "makeCacheMatrix" above.
## First use "makeCacheMatrix" to cache a matrix and its inverse for later return. 
## Then enter "cacheSolve(YOUR_MATRIX)" to calculate the inverese or return the cached inverse, if available.
## Cached inverses are returned without the need to repeat the inversion calculations, thus freeng up resources. 

cacheSolve <- function(x, ...) {
    nvrs <- x$getinverse()
    if (!is.null(nvrs)) {             ## Checks to see if the inverse has already been solved and is avaiable.
      message("getting cached data")
      return(nvrs)                   ## If inverse is available, cached solution gets returned.  
    }
    data <- x$get()                 ## If not in cache, i.e., this is first time to solve, the matrix is retrieved,
    nvrs <- solve(data, ...)       ## the inverse calculated and then saved to the list for later recall.
    x$setinverse(nvrs)
    nvrs
  }  

## Testing: 
## Test using different matrices 
matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(matrix1)

matrix2 <- makeCacheMatrix(matrix(4:1, 2, 2))
cacheSolve(matrix2)

## If we want to get either the matrix or its inverse, we can do that.
matrix1$get()
matrix1$getinverse()

## Now call again for the inverse of the matrices. 
cacheSolve(matrix1)
cacheSolve(matrix2)
## We see (red message) that the cached solution was used.

## As a check, take the inverse of matrix1, call it matrix1i, and see if we get matrix1 in return.
matrix1i <- makeCacheMatrix(matrix(c(-2,1,1.5,-.5), 2, 2))
cacheSolve(matrix1i)

## From further testing, I found that matrix multiplication doesn't work:
matrix1%*%matrix1i
##bacause the 'matrices' are now lists.
class(matrix1)

## However, one can still multiply them by doing the following:
matrix1solution <- cacheSolve(matrix1)%*%cacheSolve(matrix1i)
matrix1solution
## This shows that matrix1i is indeed the inverse.  The solution matrix is class 'matrix'.
class(matrix1solution)

## What if the matrix has changed since the time it was originally "made" with makeCacheMatrix? 
## If the matrix changes but is still called the same, cacheSolve won't work.  Consider the following example:
## matrix1 is multiplied by 10, then cacheSolve(matrix1) is tried. 
matrix1 <- matrix1$get()*10
cacheSolve(matrix1)
## matrix1 has become a new object, as seen under Data in Environemnt. 
## An Error message gets returned: "Error in x$getinverse : $ operator is invalid for atomic vectors"
## So if a matrix gets changed, that matrix needs to become listed via makeCacheMatrix before cacheSolve will work for it.





