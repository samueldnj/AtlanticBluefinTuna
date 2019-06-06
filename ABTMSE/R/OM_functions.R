#' Remove process error from an operating model projections (no recruitment deviations)
#'
#' @param OM an operating model object of class OM.
#' @return an object of class OM without process errors \code{x, dset}.
#' @export
#' @examples
#' OM_1d<-Deterministic(OM_1)
Deterministic<-function(OM){
  OM@Reccv<-array(1E-10,c(OM@nsim,OM@npop))
  OM@Recdevs<-array(rep(apply(OM@Recdevs,2:3,mean),each=OM@nsim),dim(OM@Recdevs))
  OM
}
