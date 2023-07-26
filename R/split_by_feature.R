#' split_by_feature
#' 
#' Helper function to split coordinate vector for Pedigree Point generation.
#' 
#' @param vector A vector containing data for drawing GeomPedigreePoint.
#' @param features Character vector of feature names used to split the vector.
#' @param factor A scale factor (i.e. desired point size)
#' @example 
#' \dontrun{
#' split_by_feature(sin(range),unique(data$feature.name),7)
#' }
#' @keyword Internal
split_by_feature<-function(vector,
                           features,
                           factor){
  if (!is.numeric(vector) && !is.integer(vector)) {
    stop("Input 'vector' must be a numeric or integer vector.")
  }
  # Check if 'features' is a non-empty character vector
  if (!is.character(features) || length(features) == 0) {
    stop("Input 'features' must be a non-empty character vector.")
  }
  
  # Check if 'factor' is numeric
  if (!is.numeric(factor)) {
    stop("Input 'factor' must be a numeric value.")
  }
  suppressWarnings({
    out<-matrix(vector,ncol=length(features))
    colnames(out)<-features
    out<-out*factor
    if(length(features)>1){
      rbind(rep(0,length(features)),
            out)
    }else{
      out
    }
  })
}
