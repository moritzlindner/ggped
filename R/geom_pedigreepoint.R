#' geom_pedigreepoint
#'
#' The pedigreepoint geom is used to create the symbols on a pedigree chart representing individuals. Points are shaped by gender, can be marked as deceased and tagged with multiple colours.
#' 
#' @inheritParams ggplot2::geom_point
#' @name geom_pedigreepoint
#' @section Aesthetics:
#' \code{geom_pedigreepoint()} understands the following aesthetics (required aesthetics are in bold - data type is critical for correct plotting):
#' \itemize{
#'  \item \strong{x} Coordinate. Numeric.
#'  \item \strong{y} Coordinate. Numeric.
#'  \item \strong{sex} Gender. Factor. 1 or M is Male, 2 or F is Female
#'  \item \strong{isdead} Status. Factor or Logical. 0 or FALSE is Alive, 1 or TRUE is Dead, all others are Unknown.
#'  \item feature.name Features to plot.
#'  \item feature.value Corresponding status of feature.
#'  \item colour Stroke colour. Default: "black".
#'  \item alpha  Symbol alpha. Default: 1.
#'  \item stroke Stroke thickness.
#' }
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 unit
#' @importFrom grid unit
#' @importFrom grid polygonGrob
#' @importFrom grid pointsGrob
#' @seealso Scales
#' @export
NULL
GeomPedigreePoint <- ggproto("GeomPedigreePoint",
                             Geom,
                             required_aes = c("x", "y", "sex","isdead"),
                             optional_aes =c("feature.name","feature.value","colour","alpha","stroke"),
                             default_aes = aes(feature.name="#FFFFFF", feature.value=NA,sex=22,colour = "black",alpha=1,stroke=1,isdead=0),
                             setup_data = function(data,param){
                               if(class(data$sex)!="factor"){stop("Sex is not of class factor.")}
                               if(!(class(data$isdead) %in% c("factor","logical"))){stop("isdead is not of class factor or logical.")}
                               if(class(data$feature.value)!="logical"){stop("feature.value is not of class logical")}
                               data
                             },
                             draw_panel = function(data,
                                                   panel_scales,
                                                   coord,
                                                   feature.colours,
                                                   na.colour,
                                                   size
                             ) {
                               ## Transformations
                               coords <- coord$transform(data[,c("x","y")], panel_scales)
                               coords$x<-unit(coords$x,"npc")
                               coords$y<-unit(coords$y,"npc")
                               ## Make arc and rect template
                               range<-seq(0,2*pi,pi/100)
                               circh<-split_by_feature(sin(-range),
                                                       unique(data$feature.name),
                                                       size)
                               circv<-split_by_feature(cos(-range),
                                                       unique(data$feature.name),
                                                       size)
                               recth<-split_by_feature(c(seq(0,-12.5,by=-0.5),
                                                         rep(-12.5,50),
                                                         seq(-12,12.5,by=0.5),
                                                         rep(12.5,50),
                                                         seq(12,0.5,by=-0.5))/12.5,
                                                       unique(data$feature.name),
                                                       size)
                               rectv<-split_by_feature(c(rep(12.5,25),
                                                         seq(12,-12.5,by=-0.5),
                                                         rep(-12.5,50),
                                                         seq(-12,12.5,by=0.5),
                                                         rep(12.5,25))/12.5,
                                                       unique(data$feature.name),
                                                       size)
                               tokeep<-sort(unique(c(as.vector(apply(cbind(recth,rectv),2,function(x){c(which.max(x),#[-1]
                                                                                                        which.min(x)#,[-1]
                                                                                                        )}
                                                                     )
                                                               ),
                                                     1,
                                                     dim(recth)[1]
                                                     )
                                                   )
                                            )
                               
                               h=dim(circh)[1]
                               pieid<-rep(1:length(unique(data$feature.name)),each=h)
                               ## Calculate arcs or rects around each midpoint
                               id<-NULL
                               cid<-0
                               fill<-NULL
                               alpha<-NULL
                               stroke<-NULL
                               colour<-NULL
                               for (i in 1: dim(coords)[1]){
                                 if (i==1){
                                   if (data$sex[i]==22){
                                     x<-coords$x[i]+unit(recth[,data$feature.name[i]],"points")
                                     y<-coords$y[i]+unit(rectv[,data$feature.name[i]],"points")
                                     id<-rep(i,dim(recth)[1])
                                   }else{
                                     if (data$sex[i]==0){
                                       print("Gender not specified for some subjects. Drawing as female.")
                                     }
                                     x<-coords$x[i]+unit(circh[,data$feature.name[i]],"points")
                                     y<-coords$y[i]+unit(circv[,data$feature.name[i]],"points")
                                     id<-rep(i,dim(circh)[1])
                                   }
                                 }else{
                                   if (data$sex[i]==22){
                                     x<-unit.c(x,coords$x[i]+unit(recth[,data$feature.name[i]],"points"))
                                     y<-unit.c(y,coords$y[i]+unit(rectv[,data$feature.name[i]],"points"))
                                     id<-c(id,rep(i,dim(recth)[1]))
                                   }else{
                                     if (data$sex[i]==0){
                                       warning("Gender not specified for some subjects. Drawing as female.")
                                     }
                                     x<-unit.c(x,coords$x[i]+unit(circh[,data$feature.name[i]],"points"))
                                     y<-unit.c(y,coords$y[i]+unit(circv[,data$feature.name[i]],"points"))
                                     id<-c(id,rep(i,dim(circh)[1]))
                                   }
                                 }
                                 # Feature status (Fill)
                                 if (is.na(data$feature.value[i])){
                                   fill<-c(fill,na.colour)
                                 }else{
                                   if (data$feature.value[i]){
                                     fill<-c(fill,data$feature.name[i])
                                   }else{
                                     fill<-c(fill,"#FFFFFF")
                                   }
                                 }
                                 
                                 # Generic point aesthetics
                                 alpha<-c(alpha,data$alpha[i])
                                 stroke<-c(stroke,data$stroke[i])
                                 colour<-c(colour,data$colour[i])
                               }
                               
                               # subset "non-alive"
                               if(any(data$isdead!=0)){
                                 status<-cbind(coords[data$isdead!=0,],
                                               data[data$isdead!=0,!(colnames(data) %in% c("x","y"))])
                                 status$cex<-1
                                 status$cex[status$isdead==47]<-2.25
                               }
                               offset<-unit(0.5,"points")
                               # Draw
                               if(any(data$isdead!=0)){ # glist if any dead, else enough to make polygonGrob
                                 gList(
                                   polygonGrob(
                                     x = x,
                                     y = y,
                                     id = id,
                                     default.units="npc",
                                     gp = gpar(fill = fill, alpha=alpha,
                                               lwd=stroke, col=colour)
                                     ),
                                   pointsGrob(
                                     x = status$x,
                                     y = status$y,
                                     size = unit(7, "char"),
                                     gp =gpar(cex=status$cex,alpha=status$alpha,
                                              lwd=status$stroke, col=status$colour),
                                     pch=status$isdead
                                     )
                                   )
                               }else{
                                 polygonGrob(
                                   x = x,
                                   y = y,
                                   id = id,
                                   default.units="npc",
                                   gp = gpar(fill = fill, alpha=alpha,
                                             lwd=stroke, col=colour)
                                 )
                               }
                             },
                             draw_key = function (data, params, size)  {
                               
                               if(data$isdead==0){
                                 shape<-data$sex
                               }else{
                                 shape<-data$isdead
                               }
                               pointsGrob(0.5,
                                          0.5,
                                          pch = shape,
                                          gp = grid::gpar(
                                            fill=data$feature.name,
                                            alpha=data$alpha,
                                            lwd=data$stroke, 
                                            col=data$colour
                                          )
                               )
                             }
)


#' @describeIn geom_pedigreepoint 
#' @param \strong{na.colour} Colour to fill symbols with if feature is \code{NA}.
#' @param \strong{size} Symbol size.
#' @importFrom ggplot2 layer
#' @examples
#' tmp<-data.frame(x=c(1,1,2),y=c(1,2,1),sex=as.factor(c(1,2,1)),status=as.factor(c(2,1,1)),feature.name=as.factor(c("test","test","test")),feature.value=c(TRUE,FALSE,TRUE))
#' ggplot()+
#'   geom_pedigreepoint(tmp,aes(x=x,y=y,sex=sex, isdead=status, feature.name=feature.name, feature.value=feature.value),
#'                        size=7,
#'                        na.colour="#808080")
#' @export
geom_pedigreepoint <- function( data = NULL, mapping = NULL, stat = "identity",
                                position = "identity", ... , na.rm = FALSE, 
                                na.colour = "#080808", size = 5, show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,  
    stat = stat,
    geom = GeomPedigreePoint,
    position = position, 
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, 
                  na.colour = na.colour, 
                  size = size,
                  ...)
  )
}
