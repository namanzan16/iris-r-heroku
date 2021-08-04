# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny","shinydashboard",
                "rsconnect",
                "ggpubr",
                "tidyverse",
                "broom",
                "AICcmodavg",
                "dplyr",
                "shinyWidgets",
                "readxl",
                "DT",
                "shinyjs",
                "dashboardthemes",
                "ggiraph",
                "ggrepel",
                "RColorBrewer",
                "dygraphs",
                "xts",
                "shinycssloaders",
                "waiter",
                "shinyalert",
                "shinythemes",
                "spsComps",
                "stringr",
                "data.table",
                "tidyverse",
                "stringr",
                "gridExtra",
                "ggplot2",
                "wesanderson",
                "RColorBrewer",
                "gridExtra",
                "zoo",
                "lubridate",
                "forecast",
                "prophet",
                "MLmetrics",
                "smooth",
                "astsa",
                "reshape",
                "sweep",
                "timetk",
                "nnfor")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
