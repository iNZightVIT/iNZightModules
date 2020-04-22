
#' iNZight Module Installation
#'
#' A window to allow installation of modules from various locations.
#'
#' Add-on repo -> URL -> local file
#'
#' @title iNZight Module Installation
#'
#' @author Daniel Barnett
#'
#' @export InstallModules
#' @exportClass InstallModules
InstallModules <- setRefClass(
    "InstallModules",
    fields = list(
        manager = "ANY",
        installWin = "ANY",
        from = "ANY",
        g_repo = "ANY",
        g_url = "ANY",
        g_file = "ANY",
        fname = "ANY"
    ),
    methods = list(
        initialize = function(mngr) {
            initFields(manager = mngr)

            installWin <<- gwindow("Install custom module",
                parent = manager$GUI$win,
                width = 600,
                height = 400,
                visible = FALSE
            )
            g <- gvbox(container = installWin, expand = TRUE, fill = TRUE)
            g$set_borderwidth(10)

            # Choose where to install from:
            from_grp <- ggroup(container = g)
            lbl <- glabel("Install modules from: ")
            font(lbl) <- list(weight = "bold")
            add(from_grp, lbl)

            from <<- gcombobox(
                c("Addon repository", "URL", "Local file"),
                selected = 0,
                container = from_grp,
                expand = TRUE,
                handler = function(h, ...) {
                    j <- svalue(from, index = TRUE)
                    visible(g_repo) <<- j == 1
                    visible(g_url) <<- j == 2
                    visible(g_file) <<- j == 3
                }
            )

            # A box for the controls
            ctrl_g <- gvbox(container = g)
            addSpace(ctrl_g, 10)
            init_repo()
            init_url()
            init_file()

            add(ctrl_g, g_repo)
            add(ctrl_g, g_url)
            add(ctrl_g, g_file)

            # mod.names <- lapply(getModules(manager$m_dir), function(mod) {
            #     mod$display_name
            # })

            # n.modules <- length(mod.names)

            # installed.modules <- gtable(
            #     if (n.modules == 0) "" else unname(unlist(mod.names))
            # )

            # remGp <- gframe("Remove module", horizontal = FALSE)
            # remGp$set_borderwidth(10)
            # add(g, remGp, expand = TRUE, fill = TRUE)

            # module.table <- gvbox()

            # module.placeholder <- glabel("No modules installed")
            # remove.button <- gbutton("Remove Module")

            # add(module.table, installed.modules, expand = TRUE, fill = TRUE)
            # add(module.table, remove.button)

            # add(remGp, module.table, expand = TRUE, fill = TRUE)
            # add(remGp, module.placeholder, expand = TRUE, fill = TRUE)

            # visible(module.placeholder) <- n.modules == 0
            # visible(module.table) <- n.modules > 0

            # remove.handler <- function(h, ...) {
            #     mod.to.remove <- Filter(function(mod) mod$display_name %in% svalue(installed.modules), getModules(manager$m_dir))
            #     if (removemodule(mod.to.remove[[1]])) {
            #         # GUI$menuBarWidget$defaultMenu()
            #         manager$update_menu()
            #         gmessage("Addon successfully removed.", parent = installWin)

            #         mod.names <- lapply(getModules(manager$m_dir), function(mod) {
            #             mod$display_name
            #         })

            #         n.modules <- length(mod.names)

            #         installed.modules[] <- if (n.modules == 0) "" else unname(unlist(mod.names))

            #         visible(module.placeholder) <- n.modules == 0
            #         visible(module.table) <- n.modules > 0
            #     }
            # }

            # addHandlerDoubleclick(installed.modules, remove.handler)
            # addHandlerClicked(remove.button, remove.handler)

            # When the window is closed ...
            addHandlerUnrealize(installWin,
                handler = function(h, ...) {
                    close()
                    TRUE
                }
            )

            manager$hide_win()
            visible(installWin) <<- TRUE

            invisible(.self)
        },
        init_repo = function() {
            g_repo <<- gvbox()
            visible(g_repo) <<- FALSE
        },
        init_url = function() {
            g_url <<- gvbox()
            visible(g_url) <<- FALSE
        },
        init_file = function(g) {
            g_file <<- gvbox()
            visible(g_file) <<- FALSE

            lbl <- glabel("To install a local module file, Browse to the file location and choose Open, then click Install.")
            font(lbl) <- list(size = 9)
            add(g_file, lbl, anchor = c(-1, 0))


            g_choose <- ggroup(container = g_file)

            lbl <- glabel("File Name :")
            font(lbl) <- list(weight = "bold")
            add(g_choose, lbl)

            filename <- gedit("Select a file ...",
                container = g_choose,
                expand = TRUE
            )

            btn_choose <- gbutton("Browse ...",
                container = g_choose,
                handler = function(h, ...) {
                    fname <<- gfile(
                        text = "Choose a file",
                        initial.dir = file.path("."),
                        filter = list("R file" = list(patterns = c("*.R"))),
                        quote = FALSE,
                        container = g_choose
                    )
                    svalue(filename) <- fname
                }
            )

            addSpring(g_file)

            # Module information
            # tbl <- glayout(container = g_file)

            # lbl <- glabel("Name: ")
            # font(lbl) <- list(weight = "bold")
            # mod_name <- glabel("")

            # tbl[1, expand = TRUE] <- lbl


            # Buttons!!
            btn_grp <- ggroup(container = g_file)
            addSpring(btn_grp)

            # Cancel button:
            cancelBtn <- gbutton("Cancel",
                container = btn_grp,
                handler = function(h, ...) close()
            )
            size(cancelBtn) <- c(100, -1)

            # Install button
            installBtn <- gbutton("Install",
                container = btn_grp,
                handler = function(h, ...) {
                    if (installmodule(fname, manager$m_dir)) close()
                }
            )
            size(installBtn) <- c(100, -1)

        },
        close = function() {
            visible(installWin) <<- FALSE
            manager$show_win()
            manager$update_menu()
            dispose(installWin)
        }
    )
)

installmodule <- function(file, dir, overwrite = FALSE) {
    if (checkfile(file)) {
        if (file.exists(file.path(dir, basename(file)))) {
            if (gconfirm("Module already exists - do you wish to overwrite it?")) {
                overwrite <- TRUE
            }
        }

        if (file.copy(file, dir, overwrite = overwrite)) {
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
