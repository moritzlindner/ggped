# ggped

A package to draw pedigree charts using ggplot2 and kinship2.

## Installation

```{r}
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
remotes::install_github("moritzlindner/ggped")
```

## Description

This Package contains a set of functions and geoms to draw pedigree charts from pedigree data created with kinship2.

## Examples

### Example 1:

```{r Example1, eval=TRUE, include=TRUE}
require(ggped)
data(minnbreast)

bpeds <- with(minnbreast,
    pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))
    
#### pedigree with id=8
bped.id8 <- bpeds['8']

#### make some twins
rel8 <- data.frame(id1=c(139,137), id2=c(140,138), code=c(1,2))
bped.id8 <- with(minnbreast[minnbreast$famid==8,],
                 pedigree(id, fatherid, motherid, sex, affected=proband,
                          relation=rel8))

#### make an inbred mating
bped.id8$mindex[bped.id8$id==159]<-bped.id8$mindex[bped.id8$id==149]
bped.id8$findex[bped.id8$id==159]<-bped.id8$findex[bped.id8$id==149]

#### convert into ggplot2-compatible data frame
df<-dfalign.pedigree(bped.id8)

#### draw and plot on a cartesian coodrinate system
cartesian<-ggdraw.pedigree(dat=df,features = c("affected"))

cartesian

#### plot on a polar coordinate system
cartesian+
  scale_x_continuous(expand=expansion(add = 0.25))+
  scale_y_reverse(expand=expansion(add = 1))+
  coord_polar()
```

Developed by [Moritz Lindner](http://lindnerlab.de)
