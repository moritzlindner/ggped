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
#'  \item \strong{isdead} Factor or Logical. 0 or FALSE is Alive, 1 or TRUE is Dead. If factor, 2 is Stillbirth or Miscarriage. All others are Unknown.
#'  \item feature.name Features to plot.
#'  \item feature.value Corresponding status of feature.
#'  \item adopted Logical. Depicted as brackets around symbol.
#'  \item pregnancy Logical. Depicted as P in symbol.
#'  \item colour Stroke colour. Default: "black".
#'  \item alpha Symbol alpha. Default: 1.
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
GeomPedigreePoint <- ggproto(
  "GeomPedigreePoint",
  Geom,
  required_aes = c("x", "y", "sex", "isdead"),
  optional_aes = c(
    "feature.name",
    "feature.value",
    "adopted",
    "pregnancy",
    "colour",
    "alpha",
    "stroke"
  ),
  default_aes = aes(
    feature.name = "#FFFFFF",
    feature.value = NA,
    adopted = FALSE,
    pregnancy = FALSE,
    sex = 22,
    colour = "black",
    alpha = 1,
    stroke = 1,
    isdead = 0
  ),
  setup_data = function(data, param) {
    # Check if 'sex' is a factor
    if (!is.factor(data$sex)) {
      stop("The 'sex' column must be of class 'factor'.")
    }
    
    # Check if 'isdead' is either a factor or a logical vector
    if (!is.factor(data$isdead) && !is.logical(data$isdead)) {
      stop("The 'isdead' column must be of class 'factor' or 'logical'.")
    }
    
    # Check if 'feature.value' is a logical vector
    if (!is.logical(data$feature.value)) {
      stop("The 'feature.value' column must be of class 'logical'.")
    }
    data
  },
  draw_panel = function(data,
                        panel_scales,
                        coord,
                        feature.colours,
                        na.colour,
                        size) {
    ## Transformations
    coords <-
      coord$transform(data[, c("x", "y")], panel_scales)
    coords$x <- unit(coords$x, "npc")
    coords$y <- unit(coords$y, "npc")
    ## Make arc and rect template
    range <- seq(0, 2 * pi, pi / 100)
    
    circh <-
      split_by_feature(sin(-range),
                       unique(data$feature.name),
                       size)
    circv <-
      split_by_feature(cos(-range),
                       unique(data$feature.name),
                       size)
    
    recth <-
      split_by_feature(c(
        seq(0,-12.5, by = -0.5),
        rep(-12.5, 50),
        seq(-12, 12.5, by =
              0.5),
        rep(12.5, 50),
        seq(12, 0.5, by =
              -0.5)
      ) / 12.5,
      unique(data$feature.name),
      size)
    rectv <-
      split_by_feature(c(
        rep(12.5, 25),
        seq(12,-12.5, by =
              -0.5),
        rep(-12.5, 50),
        seq(-12, 12.5, by =
              0.5),
        rep(12.5, 25)
      ) / 12.5,
      unique(data$feature.name),
      size)
    
    rhombh <-
      split_by_feature(c(
        seq(0,-12, by = -0.5),
        seq(-12.5, 12.5, by =
              0.5),
        seq(12, 0.5, by =
              -0.5)
      ) / 12.5,
      unique(data$feature.name),
      size)
    rhombv <-
      split_by_feature(c(seq(-12.5, 12, by = 0.5),
                         seq(12.5,-12, by =
                               -0.5)) / 12.5,
                       unique(data$feature.name),
                       size)
    
    tokeep <-
      sort(unique(c(as.vector(
        apply(cbind(recth, rectv), 2, function(x) {
          c(which.max(x),
            which.min(x))
        })
      ),
      1,
      dim(recth)[1])))
    h = dim(circh)[1]
    pieid <-
      rep(1:length(unique(data$feature.name)), each = h)
    
    ## Calculate arcs or rects around each midpoint
    id <- NULL
    cid <- 0
    fill <- NULL
    alpha <- NULL
    stroke <- NULL
    colour <- NULL
    for (i in 1:dim(coords)[1]) {
      if (i == 1) {
        if (data$sex[i] == 22) {
          x <- coords$x[i] + unit(recth[, data$feature.name[i]], "points")
          y <-
            coords$y[i] + unit(rectv[, data$feature.name[i]], "points")
          id <- rep(i, dim(recth)[1])
        } else{
          if (data$sex[i] == 0) {
            x <- coords$x[i] + unit(rhombh[, data$feature.name[i]], "points")
            y <-
              coords$y[i] + unit(rhombv[, data$feature.name[i]], "points")
            id <- rep(i, dim(rhombh)[1])
          } else{
            x <- coords$x[i] + unit(circh[, data$feature.name[i]], "points")
            y <-
              coords$y[i] + unit(circv[, data$feature.name[i]], "points")
            id <- rep(i, dim(circh)[1])
          }
        }
      } else{
        if (data$sex[i] == 22) {
          x <-
            unit.c(x, coords$x[i] + unit(recth[, data$feature.name[i]], "points"))
          y <-
            unit.c(y, coords$y[i] + unit(rectv[, data$feature.name[i]], "points"))
          id <-
            c(id, rep(i, dim(recth)[1]))
        } else{
          if (data$sex[i] == 0) {
            x <-
              unit.c(x, coords$x[i] + unit(rhombh[, data$feature.name[i]], "points"))
            y <-
              unit.c(y, coords$y[i] + unit(rhombv[, data$feature.name[i]], "points"))
            id <-
              c(id, rep(i, dim(rhombh)[1]))
          } else{
            x <-
              unit.c(x, coords$x[i] + unit(circh[, data$feature.name[i]], "points"))
            y <-
              unit.c(y, coords$y[i] + unit(circv[, data$feature.name[i]], "points"))
            id <-
              c(id, rep(i, dim(circh)[1]))
            
          }
        }
      }
      # Feature status (Fill)
      if (is.na(data$feature.value[i])) {
        fill <- c(fill, na.colour)
      } else{
        if (data$feature.value[i]) {
          fill <- c(fill, data$feature.name[i])
        } else{
          fill <- c(fill, "#FFFFFF")
        }
      }
      
      # Generic point aesthetics
      alpha <- c(alpha, data$alpha[i])
      stroke <- c(stroke, data$stroke[i])
      colour <- c(colour, data$colour[i])
    }
    
    # subset "non-alive"
    if (any(data$isdead != 0)) {
      status <- cbind(coords[data$isdead != 0, ],
                      data[data$isdead != 0, !(colnames(data) %in% c("x", "y"))])
      status$cex <- 2.25
    }
    # subset "adopted"
    if (any(data$adopted)) {
      adopted <- cbind(coords[data$adopted, ],
                       data[data$adopted, !(colnames(data) %in% c("x", "y"))])
      adopted$cex <- .5
    }
    # subset "pregnancy"
    if (any(data$pregnancy)) {
      pregnancy <- cbind(coords[data$pregnancy, ],
                         data[data$pregnancy, !(colnames(data) %in% c("x", "y"))])
      pregnancy$cex <- .5
    }
    
    offset <- unit(0.5, "points")
    # Draw
    obj <- polygonGrob(
      # subject
      x = x,
      y = y,
      id = id,
      default.units = "npc",
      gp = gpar(
        fill = fill,
        alpha = alpha,
        lwd = stroke,
        col = colour
      )
    )
    
    if (any(data$isdead != 0)) {
      obj <- gList(
        obj,
        pointsGrob(
          x = status$x,
          y = status$y,
          size = unit(7, "char"),
          gp = gpar(
            cex = status$cex,
            alpha = status$alpha,
            lwd = status$stroke,
            col = status$colour
          ),
          pch = status$isdead
        )
      )
    }
    
    if (any(data$pregnancy)) {
      obj <- gList(obj,
                   pointsGrob(
                     x = pregnancy$x,
                     y = pregnancy$y,
                     size = unit(7, "char"),
                     gp = gpar(
                       cex = pregnancy$cex,
                       alpha = pregnancy$alpha,
                       lwd = pregnancy$stroke,
                       col = pregnancy$colour
                     ),
                     pch = 65
                   ))
    }
    
    if (any(data$adopted)) {
      obj <- gList(obj,
                   # pointsGrob(
                   #   x = adopted$x,
                   #   y = adopted$y,
                   #   size = unit(7, "char"),
                   #   gp =gpar(cex=adopted$cex,alpha=adopted$alpha,
                   #            lwd=adopted$stroke, col=adopted$colour),
                   #   pch=80)
                   textGrob(
                     "[  ]",
                     x = adopted$x,
                     y = adopted$y,
                     vjust = 0.4,
                     gp = gpar(
                       cex = adopted$cex * 2.75,
                       alpha = adopted$alpha,
                       lwd = adopted$stroke,
                       col = adopted$colour
                     )
                   ))
    }
    obj
  },
  draw_key = function (data, params, size)  {
    if (data$isdead == 0) {
      shape <- data$sex
    } else{
      shape <- data$isdead
    }
    pointsGrob(
      0.5,
      0.5,
      pch = shape,
      gp = grid::gpar(
        fill = data$feature.name,
        alpha = data$alpha,
        lwd = data$stroke,
        col = data$colour
      )
    )
  }
)

#' @describeIn geom_pedigreepoint
#' @param na.colour \strong{Colour to fill symbols with if feature is \code{NA}}.
#' @param size \strong{Symbol size}.
#' @importFrom ggplot2 layer
#' @examples
#' tmp<-data.frame(x=c(1,1,2),y=c(1,2,1),sex=as.factor(c(1,2,1)),status=as.factor(c(2,1,1)),feature.name=as.factor(c("test","test","test")),feature.value=c(TRUE,FALSE,TRUE))
#' ggplot()+
#'   geom_pedigreepoint(tmp,aes(x=x,y=y,sex=sex, isdead=status, feature.name=feature.name, feature.value=feature.value),
#'                        size=7,
#'                        na.colour="#808080")
#' @export
geom_pedigreepoint <-
  function(data = NULL,
           mapping = NULL,
           stat = "identity",
           position = "identity",
           ... ,
           na.rm = FALSE,
           na.colour = "#080808",
           size = 5,
           show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomPedigreePoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        na.colour = na.colour,
        size = size,
        ...
      )
    )
  }
