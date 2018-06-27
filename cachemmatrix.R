# 'x' represented by 'cmatrix' 
# inverse matrix is represented by 'im' 

# Creates matrix

makeCacheMatrix <- function(cmatrix=matrix())
  im <- NULL
  set <- function(i){
    cmatrix <<- i
    im <<- NULL
  }
# cache inverse of matrix
  get <- function() cmatrix
  set_inverse <- function(inverse) im <<- inverse
  get_inverse <- function() im
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
  
# Computes the inverse
cmatrix <- NULL
cacheSolve <- function(cmatrix, ...){
  
  im <- cmatrix$get_inverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  
  } 
  
  invert_matrix <- cmatrix$get()
  im <- solve(invert_matrix, ...)
  cmatrix$set_inverse(im)
  im
}  
