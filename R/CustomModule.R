#' iNZight Custom Module
#'
#' Provides a basic module for extending new ones
#'
#' @title iNZight Custom Module
#'
#' @author Tom Elliott
#'
#' @export CustomModule
#' @exportClass CustomModule
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
        install_dependencies = function(pkgs, optional, github) {
            # add the iNZight repository:
            dkr <- "https://r.docker.stat.auckland.ac.nz"
            repo <- options()$repos
            if (!dkr %in% repo) repo <- c(dkr, repo)

            if (!missing(pkgs)) {
                pkgs <- pkgs[!pkgs %in% rownames(utils::installed.packages())]
                if (length(pkgs) > 0) {
                    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                        xlab = "", ylab = "")
                    text(0, 0, "Installing dependencies, please wait ...")

                    utils::install.packages(pkgs, quiet = TRUE, repos = repo, dependencies = TRUE)
                }
            }

            if (!missing(optional)) {
                optional <- optional[!optional %in% rownames(utils::installed.packages())]
                if (length(optional) > 0) {
                    tryCatch(
                        utils::install.packages(optional, quiet = TRUE, repos = repo, dependencies = TRUE),
                        finally = {}
                    )
                }
            }

            if (!missing(github)) {
                remotes::install_github(github, repos = repos)
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
    t <- readLines(f)
    mi <- grep("^#'", t)
    meta <- NULL
    if (length(mi)) {
        meta <- parse_meta(t[mi])
        t <- t[-mi]
    }
    t <- paste(collapse = "\n", t)
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
    e$meta <- meta
    e$module <- e[[obj]]
    e$path <- f
    e
}

parse_meta <- function(x) {
    # remove comment
    x <- gsub("^#' ", "", x)
    m <- regexpr("^@[a-zA-Z]+", x)
    names <- substr(x, m + 1, attr(m, "match.length"))
    values <- substr(x, m + attr(m, "match.length") + 1, nchar(x))
    names(values) <- names
    as.list(values)
}
