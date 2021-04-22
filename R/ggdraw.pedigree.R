#' ggdraw.pedigree
#' 
#' Draws a pedigree using ggplot 
#' 
#' @param dat A pedigree as returned by \link{kinship2::pedigree} or data frame in the format as returned by \link{dfalign.pedigree}.
#' @param features Names of columns containing binary features (e.g. affected status) in \var{dat}. 
#' @param features.as.lables Names of columns containing nominal features for plotting as labels under the subject's name.
#' @param plot.names Whether to plot names of subjects or not. Requires a defined column in dat containing names. 
#' @param plot.kinship.label Whether to calculate and plot degree of kinship for inbred matings.
#' @param column.names If \var{dat} is a data frame, then column name definitions. standart are those created by \link{dfalign.pedigree}.
#' @inheritParams kinship2::align.pedigree 
#' @param col.palette Palette to use for text features.
#' @param col.lables Font colour for subject labels. Per default, sets *wt* to black, *het* to first colour of palette and *hom* to the second colour of the palette. Accepts any arguments that is understood by the parameter *values* of \link{ggplot2:scale_colour_manual}.
#' @param col.tree Line colour for tree.
#' @param col.double Line colour for lines between repeatedly plotted subjects.
#' @inheritParams kinship2::kinship
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
ggdraw.pedigree<-function(dat=NULL,
                          features=c("affected"),
                          features.as.lables=NULL,
                          plot.names=T,
                          plot.kinship.label=T,
                          column.names=list(x="x",
                                    y="y",
                                    Name="Name",
                                    mcenterpoint="mcenterpoint",
                                    fcenterpoint="fcenterpoint",
                                    sex="sex"
                          ),
                          col.palette=suppressWarnings(brewer.pal(length(unique(dat[,features.as.lables])),"Set2")),
                          col.lables=c("wt/wt"="black",
                                       "wt"="black",
                                       "+/+"="black",
                                       "wt/mut"=col.palette[1],
                                       "het"=col.palette[1],
                                       "+/-"=col.palette[1],
                                       "mut/mut"=col.palette[2],
                                       "hom"=col.palette[2],
                                       "-/-"=col.palette[2]),
                          col.tree="#000000",
                          col.double="#808080", 
                          chrtype="autosome",
                          packed = TRUE,
                          align = TRUE,
                          width = 10,
#  add options what to show (inbred status, kinship degree, )
                          ...
                          )
{
  if(class(dat)=="pedigree"){
    dat<-dfalign.pedigree(ped, chrtype=chrtype, packed = packed, align = align, width = width)
  }else{
    if(class(dat)!="data.frame"){
      stop("dat has to be eihter of class data.frame or pedigree.")
    }
  }
  
  if(length(unique(column.names))!=length(column.names)){
    stop("Column names provided are not unique.")
  }
  if(length(unique(features))!=length(features)){
    stop("Column names for features variables are not unique.")
  }
  for (i in features){
    if(is.character(dat[,i])){
      if(length(unique(dat[!(is.na(dat[,i])),i]))<3){
        dat[,i]<-dat[,i]==dat[1,i]
        warning(paste0("Feature ",i," automatically converted into logical by setting ",dat[1,i]," as TRUE. "))
      }
    }
    if(is.numeric(dat[,i])){
      if(length(unique(dat[!(is.na(dat[,i])),i]))<3){
        dat[,i]<-dat[,i]==max(dat[,i])
        warning(paste0("Feature ",i," automatically converted into logical by setting ",max(dat[,i])," as TRUE. "))
      }
    }
  }

  for (i in names(column.names)){ # rename columns
    colnames(dat)[colnames(dat)==i]<-column.names[i]
  }
  
  shape.size=7
  text.size=8
  voffset=0 # offset f each row of lables
  dat$y<-(-dat$y)
  if(length(features)>1){ # only utilize features of class logical
    dat<-features_to_long(dat,features[lapply(dat[,features],class)=="logical"])
  }else{
    dat<-features_to_long(dat,features[class(dat[,features])=="logical"])
  }
  # matings
  plt<-ggplot(dat,aes(x=x,y=y, label="id"))+
    geom_line(data=dat[!is.na(dat$mateid),],aes(group=floor(mateid)), colour=col.tree) # floor allows drawing of line to more than one mating partner, indicated by half values
  if(any(dat$kinship>0)){ # FIXME mating with multiple partners whereof some are related does not print lablles accuratly
    plt<-plt+geom_line(data=dat[(df$mateid %in% df$mateid[df$kinship>0]),],aes(group=mateid,y=y+0.02), colour=col.tree)
    if (plot.kinship.label){
      plt<-plt+geom_text(data=dat[(df$mateid %in% df$mateid[df$kinship>0]) & !is.na(dat$mateid),],aes(label=paste0("Kinship:\n",as.character(kinship)), x=mcenterpoint),
                         vjust=(shape.size/2/min(dat$y)),
                         size=text.size/ggplot2:::.pt)
    }
  }
  # tree
  if (length(dat$Name)!=length(unique(dat$Name))){#connect repeated subjects
    plt<-plt+
      geom_line(data=dat,aes(group=ID,y=y-0.02), linetype = 2,colour=col.double) 
  }
  plt<-plt+
    geom_segment(data=dat[!is.na(dat$mateid),],aes(group=mateid,x=mcenterpoint,xend=mcenterpoint,yend=y-0.5), colour=col.tree)+ #top part of descending
    geom_segment(data=dat[!is.na(dat$mateid),],aes(group=mateid,x=mcenterpoint,xend=fcenterpoint,y=y-0.5,yend=y-0.75), colour=col.tree)+ # middle part of descending
    geom_line(data=dat[!is.na(dat$family),],aes(group=family,y=y+0.25), colour=col.tree)+
    geom_segment(data=dat[!is.na(dat$family),],aes(group=family,xend=x,yend=y+0.25), colour=col.tree) #descending for siblings
  # individuals
  plt<-plt+geom_pedigreepoint(mapping=aes(sex=sex, isdead=status, feature.name=feature.name, feature.value=feature.value),
                              ...)
  # lables  
  if("Name" %in% colnames(dat) && plot.names){
    plt<-plt+geom_text(data=dat,aes(label=Name),
                       vjust=-(shape.size*1.5/min(dat$y)),
                       hjust="outward",
                       size=text.size/ggplot2:::.pt)
    voffset<-voffset+text.size/ggplot2:::.pt*1.75
  }
  # draw character features
  for (i in features.as.lables){
    plt<-plt+geom_text(data=dat,aes_string(label=i,colour=i),
                       vjust=voffset,
                       hjust="outward",
                       size=text.size/ggplot2:::.pt)+
      scale_colour_manual(values = col.lables,guide=FALSE)
      
    voffset<-voffset+text.size/ggplot2:::.pt*1.75
  }
  # formatting
    plt<-plt+
      theme_void()+
      theme(legend.position="bottom", legend.box = "vertical")
  plt
}
