#' Scale constructors
#' 
#' Discrete scale constructors for \link{geom_pedigreepoint}
#' @param ... Additional parameters passed on to \link{ggplot2:::discrete_scale}.
#' @inheritParams ggplot2::scale_shape_manual
#' @param na.value What aesthetic value should the missing values be displayed as?
#' @param set  A palette name from the lists in \link{RColorBrewer::brewer.pal}.
#' @param main.feature.black Should the main (first) feature always be displayed as black?
#' @param name Legend title.
#' @name Scales
NULL

#' @describeIn Scales Scale constructor for the \var{feature.name} argument.
#' @importFrom ggplot2 discrete_scale
#' @importFrom RColorBrewer brewer.pal
#' @export
scale_feature.name_discrete<-function(...,
                                      set = "Set2",
                                      na.value = "grey50",
                                      main.feature.black=TRUE,
                                      name="Features") {
  ggplot2:::discrete_scale(
    aesthetics = "feature.name", 
    scale_name = "feature.name_d", 
    palette = function(x){
      palette<-suppressWarnings(brewer.pal(x,set))
      if(main.feature.black){
        palette<-c("#000000",palette[-1])
      }else{
        palette
      }
    },
    name=name,
    drop=F,
    na.value = na.value,
    ...
  )
}

#' @describeIn Scales Alias for \code{scale_feature.name_discrete}.
#' @export
scale_feature.name<-scale_feature.name_discrete

#' @describeIn Scales Scale constructor for the \var{sex} argument.
#' @importFrom ggplot2 discrete_scale
#' @importFrom utils getFromNamespace
#' @export
scale_sex_discrete<-function(...,
                             values=c(22,22,22,22,21,21,21,21,0),
                             breaks = c("1","M","Male","male","2","F","Female","female","3"),
                             name="Sex") 
{
  manual_scale<-utils::getFromNamespace("manual_scale", "ggplot2")
  manual_scale(aesthetic="sex",
                         name=name,
                           labels = function(x){
                             x<-as.factor(x)
                             levels(x)[levels(x)=="1"] <- "Male"
                             levels(x)[levels(x)=="2"] <- "Female"
                             levels(x)[levels(x)=="M"] <- "Male"
                             levels(x)[levels(x)=="F"] <- "Female"
                             levels(x)[levels(x)=="male"] <- "Male"
                             levels(x)[levels(x)=="female"] <- "Female"
                             levels(x)[levels(x)==3] <- "Unknown"
                             x
                           },values = values,
                         breaks = breaks, ...)
}

#' @describeIn Scales Alias for \code{scale_sex_discrete}.
#' @export
scale_sex<-scale_sex_discrete

#' @describeIn Scales Scale constructor for the \var{isdead} argument.
#' @importFrom utils getFromNamespace
#' @export  
scale_isdead_discrete<-function (..., values=c(0,0,47,47,63,63,63),
                                 breaks = c("0",FALSE,"1",TRUE,"2","Unknown","NA"),
                                 name="Status") 
{
  manual_scale<-utils::getFromNamespace("manual_scale", "ggplot2")
  manual_scale(aesthetic="isdead",
               name=name,
               labels = function(x){
                 x<-as.factor(x)
                 levels(x)[levels(x)=="FALSE"] <- "Alive"
                 levels(x)[levels(x)==0] <- "Alive"
                 levels(x)[levels(x)=="TRUE"] <- "Dead"
                 levels(x)[levels(x)==1] <- "Dead"
                 levels(x)[!(levels(x) %in% c("Alive","Dead"))] <- "Unknown"
                 x
               },values = values,
               breaks = breaks, ...)
}

#' @describeIn Scales Alias for \code{scale_isdead_discrete}.
#' @export
scale_isdead<-scale_isdead_discrete
