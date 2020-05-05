


library(data.table)
library(Rfast)

ym <- fread("input/PCA raw/ymaze.csv")

str(ym)

ym <- ym[trial != 1 & trial != 2]

## calculate relative time spent social
ym$relativeSocialTime <- ym$time10cmStim/300

## remove NAs
ym <- ym[!is.na(ym$lines)]

## run PCA on subset of variables
ym_pca <- ym[,c("lines","locomotion",  "echo", "grooming",
                  "latency10cmStim", "relativeSocialTime")]

prcomp(ym_pca, scale = TRUE, center = TRUE)

modelperson=prcomp(ym_pca, scale = TRUE, center = TRUE)
summary(modelperson)
biplot(modelperson)

## summary gives sd, prop var for each principal component 
ym_sum <- ym[,c("ID", "order", "arena", "date")]
ym_sum$act=modelperson$x[,1]
ym_sum$soc=modelperson$x[,2]*-1

## check to make sure direction is okay
plot(ym_sum$act, ym_pca$lines)
plot(ym_sum$soc, ym_pca$relativeSocialTime)

write.csv(ym_sum,file="output/ym-pca.csv")


### run check on PCA
library(psych) ## Main package used in this annex.
require(GPArotation) ## Supplementary package - useful for rotations.

## Users should import their data set here, saving as 'df'.

# load Y-maze data
df <- ym_pca

### Inspecting the correlations between variables before testing.
r <- cor(df)

corPlot(df) ## Graphical plot of the correlation matrix.

### Testing the suitability of the data for factoring.
cortest.bartlett(r, n = 190, diag = TRUE) ## Bartlett's test that the correlation matrix is the ID matrix.
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
