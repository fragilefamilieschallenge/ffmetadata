# Fragile Families R-package

This package allows one to retrieve metadata for the Fragile Families challenge 
via the Web API.  Users can select and search metadata for relevant 
variables. `select_metadata()` allows users to retrieve more information about a given variable.  `search_metadata()` allows users to search and filter for variables with certain attributes.

To install the package from github, use:

```{r}
## if devtools is not already installed
#install.packages("devtools")

## Install from github
devtools::install_github("fragilefamilieschallenge/ffmetadata")
```
## Vignette

To learn more about the how to use the package, please see the [vignette](https://fragilefamilieschallenge.github.io/ffmetadata/articles/APIUsage.html).
