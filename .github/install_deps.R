# R script
github_deps <- c(
    "tmelliott/gWidgets2@patch-1",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@1.11",
    "iNZightVIT/iNZightTS@1.5.8",
    "iNZightVIT/iNZightMR@2.2.5",
    "iNZightVIT/iNZightPlots@2.12",
    "iNZightVIT/iNZightRegression@1.3.1",
    "iNZightVIT/iNZight@4.1",
    "iNZightVIT/iNZightMaps@2.3"
)

OS <- Sys.getenv("OS_TYPE")
if (OS == "Windows" && !requireNamespace('utf8', quietly = TRUE)) {
    install.packages("utf8", repos = "https://cloud.r-project.org")
}
options(
    repos = c(
        if (OS == "Linux") RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Windows" && getRversion() < numeric_version("4")) {
    install.packages("RODBC", type = "binary")
    install.packages("rgl", type = "binary")
}
if (OS == "Windows")
    dir.create(tools::R_user_dir("iNZight", "config"), recursive = TRUE)

remotes::install_github(
    github_deps,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_cran("rcmdcheck")
