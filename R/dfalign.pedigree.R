#' dfalign.pedigree
#'
#' This function calculates pedigree drawing coordinates (using the \link{kinship2:align.pedigree} function) and joins them with the pedigree data. This can be helpful e.g. for plotting data with \link{ggplot2} in general and the \link{ggdraw.pedigree} function in particular.  
#'
#' @inheritParams kinship2::align.pedigree
#' @inheritParams kinship2::kinship
#' @importFrom kinship2 align.pedigree
#' @return A \code{data.frame} containing pedigree drawing coordinates and data with the following columns:
#' \describe{
#'   \item{ID}{The numeric id of each subject. }
#'   \item{Name}{The name of each subject. }
#'   \item{x,y}{Drawing coordinates of each subject.}
#'   \item{family}{The numeric id of each family. }
#'   \item{spouse}{1= subject plotted to the immediate right is a spouse, 2= subject plotted to the immediate right is an inbred spouse, 0 = not a spouse. }
#'   \item{mateid}{The numeric id of each mateing.}
#'   \item{dadid,momid}{Identification variable for father and mother. Founders' parents should be coded to NA.}
#'   \item{sex}{Gender of individual noted in ‘id’. Either character ("male","female","Male","Female","M","F") or numeric (1="male", 2="female") data is understood by downstream function \link{ggdraw.pedigree}.}
#'   \item{status}{0=alive/missing and 1=death.}
#'   \item{mcenterpoint, fcenterpoint}{Centerpoints for mating (mcenterpoint) and offsprings (fcenterpoint) for drawing the tree.}
#'   \item{kinship}{kinship between mating individuals as calculated by the \link{kinship2:kinship} function.}
#'   \item{...}{Further columns of type logical, containing affected indicators.}
#' }
#' Each row represents one subject
#' @examples
#' data(minnbreast)
#' bpeds <- with(minnbreast, pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))
#' bped.id8 <- bpeds['8']
#' df<-dfalign.pedigree(bped.id8)
#' @seealso \link{kinship2:kinship}, \link{kinship2:align.pedigree}, \link{ggdraw.pedigree}
#' @export
dfalign.pedigree<-function(ped, chrtype="autosome",packed=TRUE, width=10, align=TRUE, hints=ped$hints){
  struct<-align.pedigree(ped, packed=packed, width=width, align=align, hints=hints)
  
  ckall <- ped$id[is.na(match(ped$id, ped$id[struct$nid[struct$nid!=0]]))]
  if (length(ckall > 0)) 
    cat("Did not include the following subject:", ckall, ".\n Reason: No evidence for relation to other subjects in the tree.\n")
  nvalid<-length(struct$nid[struct$nid!=0]) #length(ped$id[!is.na(match(ped$id, ped$id[struct$nid[struct$nid!=0]]))])
  out<-data.frame(ID=numeric(nvalid),
                  Name=numeric(nvalid),
                  y=numeric(nvalid),
                  x=numeric(nvalid), 
                  family=numeric(nvalid), 
                  spouse=numeric(nvalid), 
                  mateid=numeric(nvalid),
                  mtype=numeric(nvalid), 
                  dadid=numeric(nvalid), 
                  momid=numeric(nvalid), 
                  sex=numeric(nvalid), 
                  status=numeric(nvalid),
                  mcenterpoint=numeric(nvalid), 
                  fcenterpoint=numeric(nvalid), 
                  kinship=numeric(nvalid),
                  stringsAsFactors=T) 
  
  ks<-kinship(ped,chrtype)
  n=1 # count up subjects
  mateid=1 # count up matings
  make.fam.unique<-0
  for (i in 1:length(struct$n)){ #for every row of the pedigree
    #cat("Row",i,"\n") #depugging
    for (j in 1:struct$n[i]){ # do for each subject of that row
      out$ID[n]<-struct$nid[i,j]
      out$Name[n]<-ped$id[out$ID[n]]
      #cat("Subj",out$Name[n],"\n") #depugging
      out$sex[n]<-ped$sex[out$ID[n]]
      out$dadid[n]<-ped$findex[out$ID[n]]
      out$momid[n]<-ped$mindex[out$ID[n]]
      #out$status[n]<-ped$status[out$ID[n]]
      out$status[n]<-if(length(ped$status[out$ID[n]])>0){ # life status, set to life=0 if not present
        ped$status[out$ID[n]]
      }else{
        0
      }
      if(!is.null(ped$affected)){ # affected status, if not given make one column labeling all as unaffected
        if(is.vector(ped$affected)){
          out[n,"affected"]<-ped$affected[out$ID[n]]
        }
        if(is.matrix(ped$affected)){
          out[n,colnames(ped$affected)]<-ped$affected[out$ID[n],]
        }
      }else{
        out[n,"affected"]<-F
      }
      out$y[n]<-i
      out$x[n]<-struct$pos[i,j]
      if(struct$fam[i,j]>0){ # unique family identifier, necessary as align.ped seems to ambiguously name families among generations
        out$family[n]<-struct$fam[i,j]+make.fam.unique
      }else{
        out$family[n]<-struct$fam[i,j]
      }
      out$spouse[n]<-struct$spouse[i,j]
      if(n>1 && (out$spouse[n-1]>0)){ # mating info
        if(length(out$mateid[n-2])!=0 && !is.na(out$mateid[n-2]) && !is.na(out$mateid[n-1])){ # mating w more than one neigbour can only occur after the second plotted subject
          if(out$mateid[n-1]==out$mateid[n-2]){ # special case: individual participates/d in more than one mating
                       out$mateid[n-1]<-mateid-1
                       out$mateid[n]<-mateid-0.5
                       out$mtype[n-1]<-out$spouse[n-1]
                       out$mtype[n]<-out$spouse[n-1]
          }else{
            out$mateid[n-1]<-mateid
            out$mateid[n]<-mateid
            out$mtype[n-1]<-out$spouse[n-1]
            out$mtype[n]<-out$spouse[n-1]
          }
        }else{
          out$mateid[n-1]<-mateid
          out$mateid[n]<-mateid
          out$mtype[n-1]<-out$spouse[n-1]
          out$mtype[n]<-out$spouse[n-1]
        }
        out$mcenterpoint[n-1]<-mean(c(out$x[n],out$x[n-1]))
        out$mcenterpoint[n]<-NA
        out$kinship[n-1]<-ks[as.character(out$Name[n-1]),as.character(out$Name[n])]
        mateid<-mateid+1
      }else{
        out$mateid[n]<-NA
        out$mtype[n]<-NA
        out$mcenterpoint[n]<-NA
      }
      if(out$kinship[n-1]==0 && !is.na(out$mtype[n])){
        out$kinship[n-1]<-out$mtype[n]-1
      }
      n=n+1
    }
    make.fam.unique<-make.fam.unique+max(struct$fam[i,])
  }
  
  if(is.vector(ped$affected)){
    out$affected<-as.logical(out$affected)
  }else{
    for ( i in colnames(ped$affected)){
      out[,i]<-as.logical( out[,i])
    }
  }
  
  out$spouse<-as.factor(out$spouse)
  out$mtype<-NULL#as.factor(out$mtype)
  out$family<-as.factor(out$family)
  out$sex<-as.factor(out$sex)
  out$status<-as.factor(out$status)
  
  for (i in levels(out$family)){ # make centre points of families
    out$fcenterpoint[out$ID %in% c(unique(out$momid[out$family==i]), unique(out$dadid[out$family==i]))]<-mean(out$x[out$family==i],na.rm=T)
  }
  
  #print(out$ID!=0 & out$family!=0 & out$mateid!=0)
  out$family[out$family==0]<-NA
  out
  
}

