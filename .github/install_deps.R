# R script
github_deps <- c(
    "tmelliott/surveyspec",
    # "tmelliott/gWidgets2@patch-1",
    "cran/maptools",
    "cran/rgdal",
    "cran/rgeos",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@2.0.1",
    "iNZightVIT/iNZightTS@1.5.10",
    "iNZightVIT/iNZightMR@2.3.0",
    "iNZightVIT/iNZightPlots@2.15.0",
    "iNZightVIT/iNZightRegression@1.3.3",
    "iNZightVIT/iNZight@4.4.0",
    "iNZightVIT/iNZightMaps@2.3"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        inzight = "https://r.docker.stat.auckland.ac.nz",
        CRAN = "https://cloud.r-project.org"
    )
    # install.packages.compile.from.source = "never"
)

# if (OS == "Linux" &&
#     !requireNamespace("units", quietly = TRUE) &&
#     getRversion() >= "4.3") {
#     system("sudo apt-get install libudunits2-dev")
# }

# if (OS == "Windows" && getRversion() < numeric_version("4")) {
#     install.packages("RODBC", type = "binary")
#     install.packages("rgl", type = "binary")
# }
# if (OS == "Windows") {
#     dir.create(tools::R_user_dir("iNZight", "config"), recursive = TRUE)
# }

# install broom.helpers if not already installed
# if (!requireNamespace("broom.helpers", quietly = TRUE)) {
#     install.packages("broom.helpers", repos = "https://cloud.r-project.org")
# }

# remotes::install_github(
#     github_deps,
#     INSTALL_opts = "--no-multiarch"
# )
# remotes::install_deps(
#     dependencies = TRUE,
#     INSTALL_opts = "--no-multiarch"
# )
# remotes::install_cran("rcmdcheck")

if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak", type = "source")
}

pak::pak(github_deps, dependencies = TRUE)
pak::local_install_dev_deps(dependencies = TRUE, upgrade = FALSE)
pak::pak("rcmdcheck")
