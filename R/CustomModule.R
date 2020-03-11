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
        install_dependencies = function(pkgs, optional) {
            if (!missing(pkgs)) {
                pkgs <- pkgs[!pkgs %in% rownames(utils::installed.packages())]
                if (length(pkgs) > 0) {
                    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                        xlab = "", ylab = "")
                    text(0, 0, "Installing dependencies, please wait ...")

                    utils::install.packages(pkgs, quiet = TRUE)
                }
            }

            if (!missing(optional)) {
                optional <- optional[!optional %in% rownames(utils::installed.packages())]
                if (length(optional) > 0) {
                    tryCatch(
                        utils::install.packages(optional, quiet = TRUE),
                        finally = {}
                    )
                }
            }
            plot(0, 0, type = "n", bty = "n", axt = "n", xaxt = "n", yaxt = "n",
                    xlab = "", ylab = "")
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
    mod_list <- mod_list[!sapply(mod_list, is.null)]
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
    e$path <- f
    e
}

##' iNZight Module Installation
##'
##' A window to allow installation/removal of iNZight addon modules
##'
##' @title iNZight Module Installation
##'
##' @author Daniel Barnett
##'
##' @export InstallModules
##' @exportClass InstallModules
InstallModules <- setRefClass(
    "InstallModules",
    fields = list(
        GUI = "ANY",
        installWin = "ANY",
        fname = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)

            installWin <<- gwindow("Install custom module",
                                      parent = GUI$win,
                                      width = 600,
                                      visible = TRUE
            )
            g <- gvbox(container = installWin, expand = TRUE, fill = TRUE)
            g$set_borderwidth(10)

            lbl.install <- glabel("Install Module...")
            font(lbl.install) <- list(weight = "bold")
            add(g, lbl.install)

            fileGp <- gframe("Select Module File to Install",
                             pos = 0,
                             horizontal = FALSE,
                             container = g
            )
            fileGp$set_borderwidth(10)
            fileTbl <- glayout(container = fileGp)
            ii <- 1

            lbl <- glabel("File Name :")
            font(lbl) <- list(weight = "bold")
            filename <- glabel("")
            browseBtn <- gbutton("Browse",
                                  handler = function(h, ...) {
                                      fname <<- gfile(
                                          text = "Choose a file",
                                          initial.dir = file.path("."),
                                          filter = list("iNZight module file" = list(patterns = c("*.R"))),
                                          quote = FALSE,
                                          container = fileGp
                                      )

                                      svalue(filename) <- fname
                                  }
            )
            fileTbl[ii, 1, anchor = c(1, 0)] <- lbl
            font(lbl) <- list(weight = "bold")
            fileTbl[ii, 2:4, expand = TRUE, anchor = c(1, 0)] <- filename
            fileTbl[ii, 5] <- browseBtn
            ii <- ii + 1

            installBtn <- gbutton("Install", handler = function(h, ...) {
                if (installmodule(fname, GUI$addonModuleDir)) {
                    if (gconfirm("Module installed successfully")) {
                        GUI$menuBarWidget$defaultMenu()
                        dispose(installWin)
                    }
                }
            })
            add(g, installBtn)

            lbl.remove <- glabel("Remove Module...")
            font(lbl.remove) <- list(weight = "bold")
            add(g, lbl.remove)

            mod.names <- lapply(getModules(GUI$addonModuleDir), function(mod) {
                mod$display_name
            })

            n.modules <- length(mod.names)

            installed.modules <- gtable(
                ifelse(n.modules == 0, "", unlist(mod.names))
            )

            module.table <- gvbox()

            module.placeholder <- glabel("No modules installed")
            remove.button <- gbutton("Remove Module")

            add(module.table, installed.modules, expand = TRUE, fill = TRUE)
            add(module.table, remove.button)

            add(g, module.table, expand = TRUE, fill = TRUE)
            add(g, module.placeholder, expand = TRUE, fill = TRUE)

            visible(module.placeholder) <- n.modules == 0
            visible(module.table) <- n.modules > 0

            remove.handler <- function(h, ...) {
                mod.to.remove <- Filter(function(mod) mod$display_name %in% svalue(installed.modules), getModules(GUI$addonModuleDir))
                if (removemodule(mod.to.remove[[1]])) {
                    GUI$menuBarWidget$defaultMenu()
                    gmessage("Addon successfully removed.", parent = GUI$win)
                    installed.modules[,] <- installed.modules[,][installed.modules[,] != mod.to.remove]

                    mod.names <- lapply(getModules(GUI$addonModuleDir), function(mod) {
                        mod$display_name
                    })

                    n.modules <- length(mod.names)

                    visible(module.placeholder) <- n.modules == 0
                    visible(module.table) <- n.modules > 0
                }
            }

            addHandlerDoubleclick(installed.modules, remove.handler)
            addHandlerClicked(remove.button, remove.handler)
        }
    )
)

installmodule <- function(file, dir) {
    if (checkfile(file)) {
        if (file.copy(file, dir)) {
            TRUE
        } else {
            gmessage("Could not install addon", icon = "error")
            FALSE
        }
    } else {
        gmessage("Selected file is not an iNZight addon module", icon = "error")
        FALSE
    }
}

checkfile <- function(file) {
    file.contents <- readLines(file)
    any(grepl("^[a-zA-Z]+[a-zA-Z0-9]*\\s*<-\\s*setRefClass", file.contents))
}

removemodule <- function(module) {
    if (gconfirm(sprintf("Are you sure you want to delete module '%s'?", module$display_name))) {
        file.remove(module$path)
    } else {
        FALSE
    }
}
