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
            m_tbl$remove_popup_menu()
            if (!is.null(modules$Select))
                m_tbl$cmd_coerce_column(1, as.logical)
            m_tbl$hide_row_names(TRUE)

            addHandlerChanged(m_tbl,
                handler = function(h, ...) {
                    modules <<- h$obj$get_frame()
                    set_buttons()
                }
            )

            # Buttons
            btn_grp <- ggroup(container = g)
            btn_width <- 150
            add_btn <<- gbutton("Install modules ...",
                container = btn_grp,
                handler = function(h, ...) {
                    InstallModules$new(.self)
                }
            )
            size(add_btn) <<- c(btn_width, -1)

            upd_btn <<- gbutton("Update all",
                container = btn_grp
            )
            size(upd_btn) <<- c(btn_width, -1)
            enabled(upd_btn) <<- length(mods) > 0

            rmv_btn <<- gbutton("Remove selected",
                container = btn_grp
            )
            size(rmv_btn) <<- c(btn_width, -1)
            enabled(rmv_btn) <<- length(mods) > 0 && sum(modules$Select) > 0

            addSpring(btn_grp)
            ok_btn <- gbutton("Close",
                container = btn_grp,
                handler = function(...) {
                    dispose(win)
                }
            )
            size(ok_btn) <- c(btn_width, -1)

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
                svalue(upd_btn) <<- ifelse(sum(modules$Select) > 0,
                    "Update selected", "Update all")
                enabled(upd_btn) <<- TRUE
            } else {
                modules <<- data.frame(
                    Name = "Modules will appear here once installed"
                )
                enabled(upd_btn) <<- FALSE
            }
        },
        update_tbl = function() {
            update_modules()
            m_tbl$set_frame(modules)
            if (!is.null(modules$Select))
                m_tbl$cmd_coerce_column(1, as.logical)
            m_tbl$hide_row_names(TRUE)
            set_buttons()
        },
        set_buttons = function() {
            if (length(mods)) {
                if (sum(modules$Select) > 0) {
                    svalue(upd_btn) <<- "Update selected"
                    enabled(upd_btn) <<- TRUE
                    enabled(rmv_btn) <<- TRUE
                } else {
                    enabled(upd_btn) <<- TRUE
                    enabled(rmv_btn) <<- FALSE
                    svalue(upd_btn) <<- "Update all"
                }
            } else {
                enabled(upd_btn) <<- FALSE
                enabled(rmv_btn) <<- FALSE
                svalue(upd_btn) <<- "Update all"
            }
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
