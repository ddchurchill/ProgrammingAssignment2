
## checkCache will create a new environment called my_cache, that is persisted
## between calls.  Once created, there is no need to recreate.
# 
checkCache <- function()
{
  #
  # create a cache in a new environment.  This is a soloton pattern.
  # create the cache if the cache does not exist 
  # (This happens the first time this function is called)
  # 
  if( ! exists('my_cache'))  # see if the lable exists in the environments
  {
    # create the cache one time only by creating a new environment
    my_cache <<- new.env() # must use <<- to persist. Solution fails with <-
    my_cache$hash = list() # a list to store inverse arrays in
    message('checkCache: created cache')
  }
  else message('checkCache: cache is ready')
}
#
# makeCacheMatrix - computes the inverse of a matrix and stores the result
# in the cached hash.
#
makeCacheMatrix <- function(x = matrix()) 
  {
     
      #
      # add the inverse of this matrix to the cache as a hash.
      #
      x.digest <- digest(x) # make a MD5 digest of this matrix
      x.inverse <- solve(x) # compute the inverse of x
      my_cache$hash[[x.digest]] <- x.inverse # store the matrix indexed by hash
      message('stored matrix with hash')
      message(x.digest)

}

## Write a short comment describing this function
# cacheSolve is called with the matrix you want to invert. A hash of the
# matrix is computed, and used to lookup the inverse in the persisted hash.
# If not found in the hash, the inverse is computed, added to hash.
# Finally the inverse is returned.
#
cacheSolve <- function(x, ...) {
        
    checkCache() # make sure cache is initialized
    x.digest <- digest(x) # compute the digest
    
    message('cacheSolve: computed digest')
    # if the digest exists in the hash, fetch result from cache
    if( is.null(my_cache$hash[[x.digest]])) 
    {
      message("cacheSolve: calling makeCacheMatrix")
      makeCacheMatrix(x)
    }
    else
    {
      message('cacheSolve: found inverse in cache')
   
    }
  

  return( my_cache$hash[[x.digest]])
}
