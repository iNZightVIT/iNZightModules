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

# <<<<<<< HEAD
#             ## initialize the window for sub-classes to use
#             win <<- gwindow(width = 300, height = 400, parent = GUI$win)
# =======
#             w 
# >>>>>>> 5c580fad6a4a06992a34f040344aa17e738ae248
        }
    )
)

getModules <- function() {
    # dir <- file.path("~", "Documents", "iNZightVIT", "modules")
# <<<<<<< HEAD
#     mods <- list.files(dir, pattern = "*.R", full.names = TRUE)
#     # if (load) sapply(mods, source)
#     # basename(mods)
#     lapply(mods, getmodule)
# }

# getmodule <- function(f) {
#     ## load module into an environment to avoid clashes
#     e <- new.env()
#     eval(parse(text = paste(collapse = "\n", readLines(f))), e)

#     ## fetch the module's name
#     objs <- ls(e)
#     obj <- objs[which(sapply(objs, function(o) {
#         ob <- e[[o]]
#         pclass <- try(ob@generator$def@contains$refClass@by, silent = TRUE)
#         if (inherits(pclass, "try-error")) return(FALSE)
#         pclass == "CustomModule"
#     }))]
#     if (length(obj) != 1) {
#         warning("Couldn't find module class.")
#         return(NULL)
#     }
#     e$name <- obj
#     e$module <- e[[obj]]
#     e
# =======
#     mods <- list.files(dir, full.names = TRUE)
#     if (load) sapply(mods, source)
#     basename(mods)
# >>>>>>> 5c580fad6a4a06992a34f040344aa17e738ae248
}
