#' Plot Pedigree Using ggplot2
#'
#' This function generates a pedigree plot using ggplot2.
#'
#' @param dat A pedigree object as returned by \link[kinship2]{pedigree} or data frame in the format as returned by \link{dfalign.pedigree}.
#' @param features Names of columns containing binary features (e.g. affected status) in \var{dat}.
#' @param features.as.lables Names of columns containing nominal features for plotting as labels under the subject's name.
#' @param plot.names Whether to plot names of subjects or not. Requires a defined column in dat containing names.
#' @param plot.kinship.label Whether to calculate and plot degree of kinship for inbred matings.
#' @param column.names If \var{dat} is a data frame, then column name definitions. Standard are those created by \link{dfalign.pedigree}. \var{adopted} and \var{pregancy} are additional optional logical columns.
#' @inheritParams kinship2::align.pedigree
#' @param col.palette Palette to use for text features.
#' @param col.lables Font colour for subject labels. Per default, sets *wt* to black, *het* to first colour of palette and *hom* to the second colour of the palette. Accepts any arguments that is understood by the parameter *values* of \link[ggplot2]{scale_colour_manual}.
#' @param col.tree Line colour for tree.
#' @param col.double Line colour for lines between repeatedly plotted subjects.
#' @inheritParams kinship2::kinship
#' @importFrom RColorBrewer brewer.pal
#' @import ggplot2
#' @param ... Further arguments passed to \link{geom_pedigreepoint}.
#' @return A ggplot object containing the pedigree
#' @examples
#' data(minnbreast)
#' bpeds <- with(minnbreast,pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))
#' # pedigree with id=8
#' bped.id8 <- bpeds['8']
#' # convert into ggplot-compatible data frame
#' df<-dfalign.pedigree(bped.id8)
#' cartesian<-ggdraw.pedigree(dat=df,features = c("affected"))
#' cartesian+
#'  scale_x_continuous(expand=expansion(add = 0.25))+
#'   scale_y_reverse(expand=expansion(add = 1))+
#'   coord_polar()
#' @export
ggdraw.pedigree <- function(dat = NULL,
                            features = c("affected"),
                            features.as.lables = NULL,
                            plot.names = T,
                            plot.kinship.label = T,
                            column.names = list(
                              x = "x",
                              y = "y",
                              Name = "Name",
                              mate.id = "mate.id",
                              twin.id = "twin.id",
                              mate.centerpoint = "mate.centerpoint",
                              family.centerpoint = "family.centerpoint",
                              twin.centerpoint = "twin.centerpoint",
                              sex = "sex",
                              adopted = "adopted",
                              pregnancy = "pregnancy"
                            ),
                            col.palette = suppressWarnings(brewer.pal(length(unique(dat[, features.as.lables])), "Set2")),
                            col.lables = c(
                              "wt/wt" = "black",
                              "wt" = "black",
                              "+/+" = "black",
                              "wt/mut" = col.palette[1],
                              "het" = col.palette[1],
                              "+/-" = col.palette[1],
                              "mut/mut" = col.palette[2],
                              "hom" = col.palette[2],
                              "-/-" = col.palette[2]
                            ),
                            col.tree = "#000000",
                            col.double = "#808080",
                            chrtype = "autosome",
                            packed = TRUE,
                            align = TRUE,
                            width = 10,
                            ...)
{
  
  if (is.null(dat)) {
    cli::cli_abort("Input data 'dat' cannot be NULL.")
  }
  
  if (nrow(dat) < 2) {
    cli::cli_abort("The pedigree must contain at least two individuals for meaningful plotting.")
  }
  
  if (is.null(features)) {
    cli::cli_abort("'features' cannot be NULL. Please specify at least one feature.")
  }
  
  
  if (class(dat) == "pedigree") {
    dat <- dfalign.pedigree(
      ped = dat,
      chrtype = chrtype,
      packed = packed,
      align = align,
      width = width
    )
  } else {
    if (!("data.frame" %in% class(dat))) {
      cli::cli_abort("'dat' has to be either of class 'data.frame' or 'pedigree'. Provided class: {.val {class(dat)}}")
    }
  }
  
  if (length(unique(names(column.names))) != length(names(column.names))) {
    cli::cli_abort("Column names provided are not unique. Duplicate names found: {.val {names(column.names)[duplicated(names(column.names))]}}")
  }
  
  if (length(unique(features)) != length(features)) {
    cli::cli_abort("Feature column names must be unique. Duplicate features found: {.val {features[duplicated(features)]}}")
  }
  
  for (i in features) {
    if (is.character(dat[, i])) {
      if (length(unique(dat[!(is.na(dat[, i])), i])) < 3) {
        dat[, i] <- dat[, i] == dat[1, i]
        cli::cli_warn("Feature '{.val {i}}' automatically converted into logical by setting '{.val {dat[1, i]}}' as TRUE.")
      }
    }
    if (is.numeric(dat[, i])) {
      if (length(unique(dat[!(is.na(dat[, i])), i])) < 3) {
        dat[, i] <- dat[, i] == max(dat[, i])
        cli::cli_warn("Feature '{.val {i}}' automatically converted into logical by setting max value '{.val {max(dat[, i])}}' as TRUE.")
      }
    }
  }
  
  tolabel <- as.vector(unlist(apply(dat[features.as.lables], 2, unique)))
  if (!all(tolabel %in% names(col.lables))) {
    whichmissing <- tolabel[!tolabel %in% names(col.lables)]
    cli::cli_abort("The following features selected in 'features.as.lables' have no colour assigned in 'col.labels': {.val {paste(whichmissing, collapse = ', ')}}")
  }
  
  for (i in names(column.names)) {
    # rename columns
    if (!(i %in% c("adopted", "pregnancy"))) {
      # only non optional
      colnames(dat)[colnames(dat) == i] <- column.names[i]
    } else{
      if (i %in% colnames(dat)) {
        colnames(dat)[colnames(dat) == i] <- column.names[i]
      } else{
        dat[, i] <- F
      }
    }
  }
  
  shape.size = 7
  text.size = 8
  voffset = 0 # offset f each row of lables
  dat$y <- (-dat$y)
  if (length(features) > 1) {
    # only utilize features of class logical
    dat <-
      features_to_long(dat, features[lapply(dat[, features], class) == "logical"])
  } else{
    dat <-
      features_to_long(dat, features[class(dat[, features]) == "logical"])
  }
  
  # matings
  plt <- ggplot(dat, aes(x = x, y = y, label = "id")) +
    geom_line(data = dat[!is.na(dat$mate.id),], aes(group = floor(mate.id)), colour =
                col.tree) # floor allows drawing of line to more than one mating partner, indicated by half values
  if (any(dat$kinship > 0 & !is.na(dat$mate.id))) {
    # FIXME mating with multiple partners whereof some are related does not print labels accurately
    plt <-
      plt + geom_line(data = dat[(dat$mate.id %in% dat$mate.id[dat$kinship > 0]) &
                                   !is.na(dat$mate.id) & !is.na(dat$mate.id),],
                      aes(group = mate.id, y = y + 0.02),
                      colour = col.tree)
    if (plot.kinship.label) {
      plt <-
        plt + geom_text(
          data = dat[(dat$mate.id %in% dat$mate.id[dat$kinship > 0]) &
                       !is.na(dat$mate.id) & 
                       !is.na(dat$mate.centerpoint),],
          aes(
            label = paste0("Kinship:\n", as.character(kinship)),
            x = mate.centerpoint
          ),
          vjust = (shape.size / 2 / min(dat$y)),
          size = text.size / ggplot2:::.pt
        )
    }
  }
  
  # tree - descending parts and repeated subjects
  if (length(dat$Name) != length(unique(dat$Name))) {
    #connect repeated subjects
    plt <- plt +
      geom_line(
        data = dat,
        aes(group = ID, y = y - 0.02),
        linetype = 2,
        colour = col.double
      )
  }
  plt <- plt +
    geom_segment(
      data = dat[!is.na(dat$mate.id) & !is.na(dat$mate.centerpoint),],
      aes(
        group = mate.id,
        x = mate.centerpoint,
        xend = mate.centerpoint,
        yend = y - 0.5
      ),
      colour = col.tree
    ) + #top part of descending
    geom_segment(
      data = dat[!is.na(dat$mate.id) & !is.na(dat$mate.centerpoint),],
      aes(
        group = mate.id,
        x = mate.centerpoint,
        xend = family.centerpoint,
        y = y - 0.5,
        yend = y - 0.75
      ),
      colour = col.tree
    ) # middle part of descending
  
  #children
  ## twins
  if (any(!is.na(dat$twin.id))) {
    dat$xfambarcoord <-
      unlist(apply(dat[, c("x", "twin.centerpoint", "family.centerpoint")],
                   1,
                   function(x) {
                     if (x["x"] >= x["family.centerpoint"]) {
                       if (!is.na(x["twin.centerpoint"])) {
                         max(x["x"], x["twin.centerpoint"])
                       } else{
                         x["x"]
                       }
                     } else{
                       if (!is.na(x["twin.centerpoint"])) {
                         min(x["x"], x["twin.centerpoint"])
                       } else{
                         x["x"]
                       }
                     }
                   }))
    plt <-
      plt + geom_line(
        data = dat[dat$family %in% dat$family[!is.na(dat$twin.id) & !is.na(dat$twin.id)], ],
        aes(
          group = family,
          y = y + 0.25,
          x = xfambarcoord
        ),
        colour = col.tree
      )
    plt <-
      plt + geom_segment(
        data = dat[dat$family %in% dat$family[!is.na(dat$twin.id)] & !is.na(dat$twin.id), ],
        aes(
          group = family,
          xend = twin.centerpoint,
          x = x,
          yend = y + 0.25
        ),
        colour = col.tree
      )
    
    plt <-
      plt + geom_segment(
        data = dat[dat$family %in% dat$family[!is.na(dat$twin.id)] &
                     !is.na(dat$twin.type), ],
        aes(
          group = twin.id,
          linetype = twin.type,
          y = y + 0.125,
          yend = y + 0.125,
          x = twin.centerpoint + 0.25,
          xend = twin.centerpoint - 0.25
        ),
        colour = col.tree
      )
  }
  
  ## all non-twins
  plt <-
    plt + geom_line(data = dat[!is.na(dat$family) &
                                 is.na(dat$twin.id), ],
                    aes(group = family, y = y + 0.25, x = x),
                    colour = col.tree) +
    geom_segment(data = dat[is.na(dat$twin.id) &
                              !is.na(dat$family), ],
                 aes(
                   group = family,
                   xend = x,
                   yend = y + 0.25
                 ),
                 colour = col.tree) #descending for siblings
  # individuals
  plt <-
    plt + geom_pedigreepoint(
      mapping = aes(
        sex = as.factor(sex),
        isdead = as.factor(status),
        feature.name = feature.name,
        feature.value = feature.value,
        adopted = adopted,
        pregnancy = pregnancy
      ),
      ...
    )
  # lables
  if ("Name" %in% colnames(dat) && plot.names) {
    plt <- plt + geom_text(
      data = dat,
      aes(label = Name),
      vjust = -(shape.size * 1.5 / min(dat$y)),
      hjust = "outward",
      size = text.size / ggplot2:::.pt
    )
    voffset <- -(shape.size * 1.5 / min(dat$y)) + text.size / ggplot2:::.pt * 1.01
  }
  # draw character features
  for (i in features.as.lables) {
    plt <- plt + geom_text(
      data = dat,
      aes_string(label = i, colour = i),
      vjust = voffset,
      hjust = "outward",
      size = text.size / ggplot2:::.pt
    ) +
      scale_colour_manual(values = col.lables, guide = FALSE)
    
    voffset <- voffset + text.size / ggplot2:::.pt * 1.01
  }
  # formatting
  plt <- plt +
    theme_void() +
    labs(linetype="Twin Zycocity")+
    scale_linetype_manual(labels = c("Monozygotic", "Dizygotic", "Unknown"), values = c(1, 2, 3)) +
    theme(legend.position = "bottom", legend.box = "vertical")
  plt
}
