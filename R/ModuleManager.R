ModuleManager <- setRefClass(
    "ModuleManager",
    fields = list(
        GUI = "ANY",
        win = "ANY",
        m_dir = "character",
        mods = "list",
        modules = "ANY",
        m_tbl = "ANY",
        add_btn = "ANY", upd_btn = "ANY", rmv_btn = "ANY",
        repo = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(
                GUI = gui,
                m_dir = gui$addonModuleDir
            )

            # Main window & container:
            win <<- gwindow(
                title = "Manage add-on modules",
                parent = GUI$win,
                width = 800
            )
            g <- gvbox(container = win)
            g$set_borderwidth(10)

            # Top label:
            lbl <- glabel("Installed modules:")
            font(lbl) <- list(weight = "bold")
            add(g, lbl, anchor = c(-1, 0))

            # Display modules:
            update_modules()
            m_tbl <<- gdf(modules,
                container = g,
                expand = TRUE
            )
            if (!is.null(modules$Select))
                m_tbl$cmd_coerce_column(1, as.logical)
            m_tbl$hide_row_names(TRUE)

            # Buttons
            btn_grp <- ggroup(container = g)
            add_btn <<- gbutton("Install modules ...",
                container = btn_grp,
                handler = function(h, ...) {
                    InstallModules$new(.self)
                }
            )

            upd_btn <<- gbutton("Update all",
                container = btn_grp
            )
            enabled(upd_btn) <<- length(mods) > 0

            rmv_btn <<- gbutton("Remove selected",
                container = btn_grp
            )
            enabled(rmv_btn) <<- length(mods) > 0

            addSpring(btn_grp)
            ok_btn <- gbutton("Close",
                container = btn_grp,
                handler = function(...) {
                    dispose(win)
                }
            )

            addSpace(g, 20)
            tbl <- glayout(container = g)

            lbl <- glabel("Addons are installed to: ")
            font(lbl) <- list(weight = "bold")
            tbl[1, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[1, 2:4, anchor = c(-1, 0), expand = TRUE] <- glabel(m_dir)

            lbl <- glabel("Addon repository: ")
            font(lbl) <- list(weight = "bold")
            tbl[2, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            repo <<- gcombobox(c("Stable (recommended)", "Development"))
            tbl[2, 2:4, expand = TRUE] <- repo

            visible(win) <<- TRUE
        },
        get_mods = function() {
            mods <<- lapply(
                getModules(m_dir),
                function(mod) {
                    as.data.frame(
                        list(
                            Select = FALSE,
                            Name = mod$meta$name %||% basename(mod$path),
                            Description = paste(
                                stringi::stri_wrap(
                                    mod$meta$desc %||% "",
                                    70
                                ),
                                collapse = "\n"
                            ),
                            Author = mod$meta$author %||% "",
                            Version = mod$meta$version %||% ""
                        ),
                        stringsAsFactors = FALSE
                    )
                }
            )
        },
        update_modules = function() {
            get_mods()
            if (length(mods)) {
                modules <<- do.call(rbind, mods)
                rownames(modules) <<- NULL
            } else {
                modules <<- data.frame(
                    Name = "Modules will appear here once installed"
                )
            }
        },
        update_tbl = function() {
            update_modules()
            m_tbl$set_frame(modules)
            if (!is.null(modules$Select))
                m_tbl$cmd_coerce_column(1, as.logical)
            m_tbl$hide_row_names(TRUE)
        },
        install_module = function() {
            # Open window

            # Select where to install from

            # Repository: display list of available addons

            # URL: box for URL

            # Local file: file chooser


            # Cancel and install buttons

        },
        show_win = function() {
            update_tbl()
            visible(win) <<- TRUE
        },
        hide_win = function() {
            visible(win) <<- FALSE
        },
        update_menu = function() {
            GUI$menuBarWidget$defaultMenu()
        }
    )
)
