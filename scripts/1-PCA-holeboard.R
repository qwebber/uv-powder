library(data.table)
library(Rfast)


hb <- fread("input/PCA raw/holeboard.csv")
str(hb)

hb <- hb[trial != 1 & trial != 2]

## summarize variables
hb$HDall <- hb$HD12 + hb$HD34
hb$latencyHDall <- rowMins(as.matrix(data.table(HD12 = as.numeric(hb$latencyHD12), 
                                                 HD34 = as.numeric(hb$latencyHD34))), value = TRUE)
## remove NAs
hb <- hb[!is.na(hb$lines)]

## run PCA on subset of variables
hb_pca <- hb[,c("lines","locomotion",  "flights", "echo", 
                  "HDall", "latencyHDall", "latencyEnter", "grooming")]

prcomp(hb_pca, scale = TRUE, center = TRUE)

modelperson=prcomp(hb_pca, scale = TRUE, center = TRUE)
summary(modelperson)
biplot(modelperson)


## summary gives sd, prop var for each principal component 
hb_sum <- hb[,c("ID", "order", "arena", "date")]
hb_sum$act=modelperson$x[,1]*-1
hb_sum$exp=modelperson$x[,2]*-1

## check to make sure direction is okay
plot(hb_pca$lines, hb_sum$act)
plot(hb_pca$HDall, hb_sum$exp)


write.csv(hb_sum,file="output/hb-pca.csv")

### run check on PCA
library(psych) ## Main package used in this annex.
require(GPArotation) ## Supplementary package - useful for rotations.

## Users should import their data set here, saving as 'df'.

# load Y-maze data
df <- hb_pca

### Inspecting the correlations between variables before testing.
r <- cor(df)

corPlot(df) ## Graphical plot of the correlation matrix.

### Testing the suitability of the data for factoring.
cortest.bartlett(r, n = 189, diag = TRUE) ## Bartlett's test that the correlation matrix is the ID matrix.
## The P value should be low, indicating that correlations are not all 1, and multiple 
## factors could be extracted.

KMO(r) ## Kaiser-Meyer-Olkin measure of sampling adequacy.
## Less than 0.5 for an item has been labelled unacceptable,
## but higher values (e.g. > 0.8) are generally preferred.

### Determining the number of factors to extract.
nfactors(df ## Replicates the style of Fig. 2.
         , n = 10 ## Sets the maximum number of factors to search for - default is 20.
         , rotate = 'oblimin' ## Default is 'varimax' - an orthogonal rotation.
)
## Output plot shows VSS, eBIC, SRMR and Complexity (a general diagnostic statistic).
## Full output is displayed in the console, and additional statistics can be explored
## and plotted, e.g.
plot(nfactors(df, n=10, rotate='oblimin')$map, type = 'b')
## Velicer's minimum average partial (MAP), which indicates the optimal number of factors
## where it reaches a minimum.

## To fully take advantage of the many nfactors statistics, we strongly recommend
## that users consult the help file:
?nfactors

## Parallel analysis of factors solutions.
fa.parallel(df
            , sim = FALSE ## Default is TRUE - FALSE replicates style of Fig. 3.
            , SMC = FALSE  ## Ensures that PA is adjusted for factors.
            , fa = 'fa' ## Plots only the factor analyses.
)
## This plots a scree plot with adjusted eigenvalues and the data for comparison,
## which are random and/or resampled. Where the adjusted eigenvalue for a given factor 
## is above the line of eigenvalues from random/resampled data, parallel analysis
## indicates that that factor ought to be retained.