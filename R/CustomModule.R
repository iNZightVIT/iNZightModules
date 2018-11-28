CustomModule <- setRefClass(
    "CustomModule",
    fields = list(
        GUI = "ANY",
        moduledir = "character"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)

            ## set the moduledirectory
            # moduledir <<- file.path("~", "Documents", "iNZightVIT", "modules")

            ## check for and load any existing modules
            # mods <- listModules(moduledir)

        }
    )
)

getModules <- function(load) {
    dir <- file.path("~", "Documents", "iNZightVIT", "modules")
    mods <- list.files(dir, full.names = TRUE)
    if (load) sapply(mods, source)
    basename(mods)
}