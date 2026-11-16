# Install if needed

# GAP packages ------------------------------------------------------------
packages <- c("knitr", "viridis", "flextable", "tidyverse", "RODBC", "getPass", "here", "remotes")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Automatically update packages - These should just use the latest version
remotes::install_github("afsc-gap-products/akgfmaps")
remotes::install_github("afsc-gap-products/navmaps")

library(navmaps)
library(akgfmaps)

# Other R packages --------------------------------------------------------


