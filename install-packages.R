#!/usr/bin/Rscript
require(yaml)

# get installed packages
i.pkgs <- rownames(installed.packages())

# packages on CRAN
a.pkgs <- read_yaml("install.yaml")
n.pkgs <- a.pkgs[["CRAN"]]

# to collect stats
success <- c()
fail <- c()

# install CRAN based packages
if (!(is.null(n.pkgs))) {
  for (p in n.pkgs) {
    # check if already installed
    if (!(p %in% i.pkgs)){
      # try-catch install
      stat <- try(install.packages(p,
                                  dependencies = TRUE,
                                  repos = "https://ftp.acc.umu.se/mirror/CRAN/"))

      # add status
      if (!(is.null(attr(stat,"class")))) {
        fail <- c(fail,p)
        } else {
          success <- c(success,p)
        }
    } else {
      success <- c(success,p)
    }
  }
}

# Install github packages
if (!require("devtools")) install.packages("devtools")
devtools::install_github("talgalili/d3heatmap")

# log-message construction
success <- ifelse(is.null(success),
                  "NONE\n\n",
                  paste(success,collapse = "\n"))

fail <- ifelse(is.null(fail),
               "NONE\n\n",
               paste(fail,collapse = "\n")
               )

txt <- paste(c("\n----\n",
               "Successful installs:\n",
               "----\n",
               success,
               "\n\n----\n",
               "Failed installs:\n",
               "----\n",
               fail),
               collapse  = "")

# print summary message
cat(txt)
