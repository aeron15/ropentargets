# ropentargets
R client package for Open Targets REST API

## Getting started

First, you need to install the devtools package. You can do this from CRAN. Invoke R and then type
```R
install.packages("devtools")
```
Load the devtools package and install from github directly
```R
library(devtools)
install_github("cttv/ropentargets")
```
Then head to the [scripts folder](https://github.com/CTTV/ropentargets/tree/master/scripts) and try running any of the examples contained there.

Each class is also documented, so for example to read the documentation for the `Gene` class, type:
```R
?ropentargets::Gene
```
