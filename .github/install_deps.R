# R script
github_deps <- c(
    "tmelliott/surveyspec@develop",
    "tmelliott/gWidgets2@patch-1",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@dev",
    "iNZightVIT/iNZightTS@1.5.8",
    "iNZightVIT/iNZightMR@dev",
    "iNZightVIT/iNZightPlots@dev",
    "iNZightVIT/iNZightRegression@dev",
    "iNZightVIT/iNZight@dev",
    "iNZightVIT/iNZightMaps@2.3"
)

OS <- Sys.getenv("OS_TYPE")
if (OS == "Windows" && !requireNamespace("utf8", quietly = TRUE)) {
    install.packages("utf8", repos = "https://cloud.r-project.org")
}

options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Linux" &&
    !requireNamespace("units", quietly = TRUE) &&
    getRversion() >= "4.3") {
    system("sudo apt-get install libudunits2-dev")
}

if (OS == "Windows" && getRversion() < numeric_version("4")) {
    install.packages("RODBC", type = "binary")
    install.packages("rgl", type = "binary")
}
if (OS == "Windows") {
    dir.create(tools::R_user_dir("iNZight", "config"), recursive = TRUE)
}

# install broom.helpers if not already installed
if (!requireNamespace("broom.helpers", quietly = TRUE)) {
    install.packages("broom.helpers", repos = "https://cloud.r-project.org")
}

remotes::install_github(
    github_deps,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_cran("rcmdcheck")
