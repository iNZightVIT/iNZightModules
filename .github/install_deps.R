# R script
github_deps <- c(
    "tmelliott/gWidgets2@patch-1",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@dev",
    "iNZightVIT/iNZightTS@1.5.5",
    "iNZightVIT/iNZightMR@2.2.5",
    "iNZightVIT/iNZightPlots@2.12",
    "iNZightVIT/iNZightRegression@1.2.8",
    "iNZightVIT/iNZight@dev",
    "iNZightVIT/iNZightMaps@dev"
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
if (OS == "Windwos")
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
