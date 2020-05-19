#' Compute binary search
#'
#' this fuction computes binary search
#'
#' @examples
#' a<-1:100
#' BinSearch(a,50,1,length(a))
BinSearch <- function(A, value, low, high) {
  if ( high < low ) {
    return(NULL)
  } else {
    mid <- floor((low + high) / 2)
    if ( A[mid] > value )
      BinSearch(A, value, low, mid-1)
    else if ( A[mid] < value )
      BinSearch(A, value, mid+1, high)
    else
      mid
  }
}

