# MLAT
MLAT is a work in progress R package.


# Installing
require('devtools')
devtools::devtools::install_git('PauloCirino/MLAT')

### Example
cmpTestsFuncsList <- GetAllMultClassAlgo()
task <- 'MultClass'
dataSetNames <- c('Iris', 'PimaIndiansDiabetes')
myResult <- RunTests(cmpTestsFuncsList = GetAllMultClassAlgo(),
                     task = 'MultClass',
                     dataSetNames = c('Iris', 'PimaIndiansDiabetes'))
