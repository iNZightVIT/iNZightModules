InstallModules <- setRefClass(
    "InstallModules",
    fields = list(
        manager = "ANY",
        installWin = "ANY",
        from = "ANY",
        g_repo = "ANY",
        g_url = "ANY",
        g_file = "ANY",
        filename = "ANY",
        fname = "ANY",
        url = "ANY",
        add_tbl = "ANY",
        addons_available = "logical",
        inst_repo_btn = "ANY",
        inst_url_btn = "ANY",
        inst_file_btn = "ANY"
    ),
    methods = list(
        initialize = function(mngr) {
            initFields(manager = mngr)

            installWin <<- gwindow("Install custom module",
                parent = manager$GUI$win,
                width = size(manager$win)[1],
                height = size(manager$win)[2],
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
                    visible(g_repo) <<- FALSE
                    visible(g_url) <<- FALSE
                    visible(g_file) <<- FALSE
                    j <- svalue(from)
                    visible(g_repo) <<- j == "Addon repository"
                    visible(g_url) <<- j == "URL"
                    visible(g_file) <<- j == "Local file"
                }
            )

            # A box for the controls
            ctrl_g <- gvbox(container = g, expand = TRUE)
            addSpace(ctrl_g, 10)
            init_repo()
            init_url()
            init_file()

            add(ctrl_g, g_repo, expand = TRUE, fill = TRUE)
            add(ctrl_g, g_url)
            add(ctrl_g, g_file)

            if (addons_available) {
                svalue(from) <<- "Addon repository"
            } else {
                from$set_items(c("URL", "Local file"))
                svalue(from) <<- "Local file"
            }

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

            ## Fetch list
            #addon_file <- file.path(manager$repo_url, "addons.txt")
            #addon_list <- tempfile(fileext = ".txt")
            #download.file(addon_file, addon_list, quiet = TRUE)
            addons <- manager$repo_addons
            addons_available <<- nrow(addons) > 0
            if (!addons_available) {
                glabel("No addons available.", container = g_repo)
                return()
            }

            addons <- data.frame(Install = logical(nrow(addons)), addons)
            ## Display list
            add_tbl <<- gdf(addons,
                container = g_repo,
                expand = TRUE
            )
            add_tbl$remove_popup_menu()
            add_tbl$hide_row_names(TRUE)
            add_tbl$cmd_coerce_column(1, as.logical)
            addHandlerChanged(add_tbl,
                handler = function(h, ...) {
                    enabled(inst_repo_btn) <<-
                        sum(h$obj$get_frame()$Install) > 0
                }
            )

            ## Buttons (Cancel + Install)
            btn_grp <- ggroup(container = g_repo)
            addSpring(btn_grp)

            # Cancel button:
            cancelBtn <- gbutton("Cancel",
                container = btn_grp,
                handler = function(h, ...) close()
            )
            size(cancelBtn) <- c(100, -1)

            # Install button
            inst_repo_btn <<- gbutton("Install selected",
                container = btn_grp,
                handler = function(h, ...) {
                    w <- which(add_tbl$get_frame()$Install)
                    urls <- file.path(
                        manager$repo_url,
                        add_tbl$get_frame()$filename[w]
                    )
                    ok <- sapply(urls,
                        function(url) {
                            f <- file.path(tempdir(), basename(url))
                            on.exit(unlink(f))
                            download.file(url, f, quiet = TRUE)
                            installmodule(f, manager$m_dir)
                        }
                    )

                    if (all(ok)) close()
                    else gmessage("Unable to install", parent = installWin)
                }
            )
            size(inst_repo_btn) <<- c(150, -1)
            enabled(inst_repo_btn) <<- FALSE
        },
        init_url = function() {
            g_url <<- gvbox()
            visible(g_url) <<- FALSE

            g_choose <- ggroup(container = g_url)

            lbl <- glabel("URL :")
            font(lbl) <- list(weight = "bold")
            add(g_choose, lbl)

            url <<- gedit("", container = g_choose, expand = TRUE)

            addSpring(g_url)

            # Buttons!!
            btn_grp <- ggroup(container = g_url)
            addSpring(btn_grp)

            # Cancel button:
            cancelBtn <- gbutton("Cancel",
                container = btn_grp,
                handler = function(h, ...) close()
            )
            size(cancelBtn) <- c(100, -1)

            # Install button
            inst_url_btn <<- gbutton("Install",
                container = btn_grp,
                handler = function(h, ...) {
                    f <- file.path(tempdir(), basename(svalue(url)))
                    on.exit(unlink(f))
                    download.file(svalue(url), f, quiet = TRUE)
                    if (installmodule(f, manager$m_dir)) close()
                    else gmessage("Unable to install", parent = installWin)
                }
            )
            size(inst_url_btn) <<- c(100, -1)

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

            filename <<- gedit("Select a file ...",
                container = g_choose,
                expand = TRUE
            )

            btn_choose <- gbutton("Browse ...",
                container = g_choose,
                handler = function(h, ...) {
                    svalue(filename) <<- gfile(
                        text = "Choose a file",
                        initial.dir = file.path("."),
                        filter = list("R file" = list(patterns = c("*.R"))),
                        quote = FALSE,
                        container = g_choose
                    )
                }
            )
            addHandlerChanged(filename,
                handler = function(h, ...) {
                    fname <<- svalue(h$obj)
                }
            )
            addHandlerKeystroke(filename,
                handler = function(h, ...) {
                    fname <<- svalue(h$obj)
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
            inst_file_btn <<- gbutton("Install",
                container = btn_grp,
                handler = function(h, ...) {
                    if (installmodule(fname, manager$m_dir)) close()
                    else gmessage("Unable to install", parent = installWin)
                }
            )
            size(inst_file_btn) <<- c(100, -1)

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
        print(file)
        print(dir)
        if (!overwrite && file.exists(file.path(dir, basename(file)))) {
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
    msg <- sprintf("Are you sure you want to delete module '%s'?",
        module$display_name
    )
    if (gconfirm(msg)) {
        file.remove(module$path)
    } else {
        FALSE
    }
}
