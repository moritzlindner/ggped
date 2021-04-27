#' split_by_feature
#' 
#' Helper function to split coordinate vector for Pedigree Point generation.
#' 
#' @param vector A vector containing data for drawing GeomPedigreePoint
#' @param features Features to split along
#' @param factor A scale factor (i.e. desired point size)
#' @example 
#' \dontrun{
#' split_by_feature(sin(range),unique(data$feature.name),7)
#' }
#' @keyword Internal
split_by_feature<-function(vector,
                           features,
                           factor){
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
