##' iNZight Custom Module
##'
##' Provides a basic module for extending new ones
##'
##' @title iNZight Custom Module
##'
##' @author Tom Elliott
##'
##' @export CustomModule
##' @exportClass CustomModule
CustomModule <- setRefClass(
    "CustomModule",
    fields = list(
        GUI = "ANY",
        win = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)

            w 
        }
    )
)

getModules <- function(load) {
    dir <- file.path("~", "Documents", "iNZightVIT", "modules")
    mods <- list.files(dir, full.names = TRUE)
    if (load) sapply(mods, source)
    basename(mods)
}
