# RShiny_ColiasModel

RShiny_ColiasModel is an interactive shiny app that allows any user to visualize fitness-related variables such as operataive temperature and egg viability of Colias butterflies under varying wing absorptivity. This visualization is based on [Buckley and Kingsolver (2019)](https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12953), and uses a future air temperature projection model from the CMIP5 multimodel ensemble. 

## Prerequisites for opening in Rstudio
Git and Rstudio ([Instructions](https://resources.github.com/whitepapers/github-and-rstudio/))  
Installation of the following R packages:
shiny, magrittr, shinythemes, shinyWidgets, ggplot2, dplyr, leaflet, ggmap, maps, raster, sp, rgdal, viridis, shinycssloaders, shinyjs, sortable, rnoaa, chillR, reshape2, tidyr, gridExtra, shinyBS, cicerone

```
pkgs <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap", "maps", "raster", "sp", "rgdal", "viridis", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "sortable", "rnoaa", "chillR", "reshape2", "tidyr", "gridExtra", "shinyBS", "cicerone")

lapply(pkgs, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
```

## Using RShiny_ColiasModel
* Opening in Rstudio:  
Click on "Code" on the top right to copy the link to this repository.  
Click ```File```, ```New Project```, ```Version Control```, ```Git```  
Paste the repository URL and click ```Create Project```.

* Alternatively, go to [this link](https://huckley.shinyapps.io/rshiny_coliasmodel/).

We have a google doc with questions to guide through the app for further understanding of the topic.

## Contributing to RShiny_ColiasModel
<!--- If your README is long or you have some specific process or steps you want contributors to follow, consider creating a separate CONTRIBUTING.md file--->
To contribute to RShiny_ColiasModel, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).
