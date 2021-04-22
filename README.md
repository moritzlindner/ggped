# ggped
Draws pedigrees using ggplot and kinship2

## Installation
```{r}
if (!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
remotes::install_github("moritzlindner/ggped")
```

## Description

This Package contains a set of functions and geoms to draw pedigree charts from pedigree data created with \link[kinship2:pedigree]{kinship2}. 

## Examples

### Example 1:

```{r Example1, eval=TRUE, include=T}
data(minnbreast)

bpeds <- with(minnbreast,
    pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))
    
#### pedigree with id=8
bped.id8 <- bpeds['8']

#### convert into ggplot-compatible data frame
df<-dfalign.pedigree(bped.id8)

cartesian<-ggdraw.pedigree(dat=df,features = c("affected"))

cartesian+
  scale_x_continuous(expand=expansion(add = 0.25))+
  scale_y_reverse(expand=expansion(add = 1))+
  coord_polar()




```
### Example 2:

```{r Example2, eval=FALSE, include=T}

sangergenotype(dir="D:\\", link="http://www.x.zip",wtseq="ACTGAAAA",mutseq="ACCGAAAA", revcomp = TRUE, cutoff = 0.2)
as.data.frame(tmp)
```

Developed by [Moritz Lindner](http://lindnerlab.de)