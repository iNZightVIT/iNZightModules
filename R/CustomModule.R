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
        modwin = "ANY",
        mainGrp = "ANY",
        homeButton = "ANY"
    ),
    methods = list(
        initialize = function(gui, 
            name = "Module", 
            embedded = TRUE
        ) {
            initFields(GUI = gui)

            # if (embedded) {}
            modwin <<- GUI$initializeModuleWindow(.self,
                title = name,
                scroll = TRUE
            )
            mainGrp <<- modwin$body

            homeButton <<- gbutton("Home",
                handler = function(h, ...) close()
            )

            GUI$plotToolbar$update(NULL)

            # add(modwin$footer, helpButton, expand = TRUE, fill = TRUE)
            add(modwin$footer, homeButton, expand = TRUE, fill = TRUE)


        },
        get_data = function() {
            GUI$getActiveData()
        },
        close = function() {
            ## run module-specific closure?

            ## delete the module window
            delete(GUI$leftMain, GUI$leftMain$children[[2]])
            ## display the default view (data, variable, etc.)
            GUI$plotToolbar$restore()
            GUI$menuBarWidget$defaultMenu()
            GUI$updatePlot()
            visible(GUI$gp1) <<- TRUE
            invisible(TRUE)
        }
    )
)

getModules <- function(dir) {
    mods <- list.files(dir, pattern = "*.R", full.names = TRUE)
    mod_list <- lapply(mods, getmodule)
    names(mod_list) <- sapply(mod_list, function(x) x$name)
    mod_list
}

getmodule <- function(f) {
    ## check if file is a Module
    t <- paste(collapse = "\n", readLines(f))
    if (!grepl("^[a-zA-Z]+[a-zA-Z0-9]*\\s*<-\\s*setRefClass", t)) return(NULL)

    ## load module into an environment to avoid clashes
    e <- new.env()
    eval(parse(text = t), e)

    ## fetch the module's name
    objs <- ls(e)
    obj <- objs[which(sapply(objs, function(o) {
        ob <- e[[o]]
        pclass <- try(ob@generator$def@contains$refClass@by, silent = TRUE)
        if (inherits(pclass, "try-error")) return(FALSE)
        pclass == "CustomModule"
    }))]
    if (length(obj) != 1) {
        warning("Couldn't find module class.")
        return(NULL)
    }
    e$name <- obj
    e$display_name <- e[[obj]]@className[1]
    e$module <- e[[obj]]
    e
}

InstallModules <- setRefClass(
    "InstallModules",
    fields = list(
        GUI = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)
        }
    )
)
