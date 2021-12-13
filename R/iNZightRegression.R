#' iNZight Model Fitting Module
#'
#' A GUI add-on for fitting regression models.
#'
#' @title iNZight Model Fitting Module
#'
#' @author Tom Elliott
#'
#' @import iNZightRegression
#' @importFrom survey svyglm
#'
#' @export iNZightRegMod
#' @exportClass iNZightRegMod
iNZightRegMod <- setRefClass(
    "iNZightRegMod",
    fields = list(
        GUI         = "ANY",
        outputWin = "ANY",
        plotWin = "ANY",
        mainGrp     = "ANY",
        smryOut     = "ANY",
        regPlots    = "ANY",
        responseBox = "ANY",
        responseTypeBox = "ANY",
        responseFamilyBox = "ANY",
        response = "character",
        responseType = "numeric",
        responseFamily = "numeric",
        responseTransform = "character",
        contVarBox  = "ANY",
        catVarBox = "ANY",
        variables   = "character",
        intercept = "ANY",
        explanatoryList = "ANY",
        confounding = "character",
        confounderList = "ANY",
        btnEditSig  = "ANY",
        modelName   = "ANY",
        modelList = "ANY",
        compBtn = "ANY",
        fit         = "ANY",
        summaryOutput = "character",
        fits        = "list",
        working     = "integer",
        showBoots   = "ANY",
        plottype    = "numeric",
        numVarList = "ANY",
        catVarList = "ANY",
        plotTypeList = "ANY",
        compMatrix = "ANY",
        svyname     = "character",
        codehistory = "ANY",
        isSurveyObject = "logical"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(
                GUI = GUI,
                working = 1L,
                plottype = 1,
                codehistory = NULL,
                svyname = ""
            )

            isSurveyObject <<- !is.null(getdesign())

            modwin <- GUI$initializeModuleWindow(.self,
                title = "Model Fitting",
                scroll = TRUE
            )
            mainGrp <<- modwin$body

            # character, datasheet, evaluate, history, preview, rlogo,
            showoutputBtn <- iNZight:::gimagebutton(
                stock.id = "gw-preview",
                tooltip = "View model output window",
                handler = function(h, ...) .self$showOutput()
            )
            addhistbtn <- iNZight:::gimagebutton(
                stock.id = "rlogo",
                tooltip = "Save code for current plot",
                handler = function(h, ...) updatePlot(savecode = TRUE)
            )
            showhistbtn <- iNZight:::gimagebutton(
                stock.id = "history",
                tooltip = "View code history",
                handler = function(h, ...) {
                    if (GUI$popOut) {
                        if (is.null(codehistory) || !is.null(svalue(codehistory))) {
                            codehistory <<- gtext("",
                                expand = TRUE, fill = TRUE,
                                font.attr = list(family = "monospace"),
                                container = ggroup(
                                    container = gwindow("Code History",
                                        width = 800, height = 600,
                                        parent = GUI$win
                                    )
                                )
                            )
                        }
                    } else {
                        if (is.null(codehistory) ||
                            !"Code History" %in% names(GUI$plotWidget$plotNb)) {
                            ## need to create it
                            codehistory <<- gtext("",
                                expand = TRUE, fill = TRUE,
                                font.attr = list(family = "monospace")
                            )
                            add(GUI$plotWidget$plotNb, codehistory,
                                label = "Code History",
                                close.button = TRUE
                            )
                        }
                    }

                    svalue(codehistory) <<- ""
                    sapply(GUI$rhistory$get(), insert,
                        obj = codehistory,
                        font.attr = list(family = "monospace")
                    )
                }
            )
            btns <- list(addhistbtn, showhistbtn)
            if (GUI$popOut) btns <- c(list(showoutputBtn), btns)
            GUI$plotToolbar$update(NULL,
                refresh = "updatePlot",
                extra = btns
            )

            ## and set the menubar
            GUI$menuBarWidget$setMenu(
                "Model Fitting" = list(
                    home =
                        gaction("Close",
                            icon = "close",
                            tooltip = "Close model fitting module",
                            handler = function(h, ...) .self$close()),
                    exit =
                        gaction("Exit iNZight",
                            icon = "symbol_diamond",
                            tooltip = "Exit iNZight completely",
                            handler = function(h, ...) GUI$close())
                ),
                "View" =
                    if (GUI$popOut) {
                        list(
                            output =
                                gaction("Model output",
                                    icon = "symbol_diamond",
                                    tooltip = "Show model output",
                                    handler = function(h, ...) .self$showOutput()
                                ),
                            plot =
                                gaction("Model plots",
                                    icon = "symbol_diamond",
                                    tooltip = "Show model plots",
                                    handler = function(h, ...) .self$showPlot()
                                )
                        )
                    } else list()
            )

            if (!is.null(GUI$moduledata) &&
                !is.null(GUI$moduledata$regression) &&
                !is.null(GUI$moduledata$regression$fits))

                fits <<- GUI$moduledata$regression$fits

            if (isSurveyObject) {
                lbl2 <- glabel(paste0(
                    "Using complex survey design ",
                    "(note: still under development)"
                ))
                font(lbl2) <- list(size = 9)
                add(mainGrp, lbl2, anchor = c(0, 0))
            }

            helpButton <- gbutton("Help",
                handler = function(h, ...) {
                    browseURL(paste0(
                        "https://www.stat.auckland.ac.nz/~wild/",
                        "iNZight/user_guides/add_ons/",
                        "?topic=model_fitting"
                    ))
                }
            )
            homeButton <- gbutton("Home",
                handler = function(h, ...) close()
            )

            add(modwin$footer, helpButton, expand = TRUE, fill = TRUE)
            add(modwin$footer, homeButton, expand = TRUE, fill = TRUE)



            ## -----------------------------------------------------------------
            ## Here lies the left panel
            responseGp <- gexpandgroup("Response Options",
                horizontal = FALSE,
                container = mainGrp
            )
            responseGp$set_borderwidth(10)
            responseTbl <- glayout(container = responseGp)
            ii <-  1

            lbl <- glabel("Variable: ")
            yVars <- names(getdata()[, sapply(getdata(),
                function(x) is.numeric(x) || length(levels(x)) == 2)])
            responseBox <<- gcombobox(yVars,
                selected = 0,
                handler = function(h, ...) {
                    working <<- working + 1L
                    response <<- svalue(h$obj)
                    ## detect framework
                    y <- getdata()[[response]]
                    if (is.numeric(y)) {
                        svalue(responseTypeBox, index = TRUE) <<- 1
                    } else if (length(levels(y)) == 2) {
                        svalue(responseTypeBox, index = TRUE) <<- 2
                    }
                    ## set explanatory variables
                    setAvailVars()
                    working <<- working - 1L
                    updateModel()
                }
            )
            responseTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            responseTbl[ii, 2:3, expand = TRUE] <- responseBox
            ii <- ii + 1

            flbl <- glabel("Framework: ")
            responseTypeBox <<- gcombobox(
                c(
                    "Least Squares",
                    "Binary Regression (logistic, ...)",
                    "Poisson Regression (counts)",
                    "Negative Binomial (counts)"
                ),
                selected = 0,
                handler = function(h, ...) {
                    working <<- working + 1L
                    responseType <<- svalue(h$obj, index = TRUE)
                    responseFamilyBox$set_items(
                        switch(responseType,
                            c("Gaussian"),
                            c("Logistic", "Probit"),
                            c("Log"),
                            c("Log")
                        )
                    )
                    if (responseType == 2) {
                        visible(responseFamilyBox) <<- TRUE
                        svalue(flbl) <- "Framework:\n\nLink function: "
                    } else {
                        visible(responseFamilyBox) <<- FALSE
                        svalue(flbl) <- "Framework: "
                    }
                    svalue(responseFamilyBox, index = TRUE) <<- 1
                    working <<- working - 1L
                    updateModel()
                }
            )
            responseFamilyBox <<- gcombobox(
                c("Gaussian"),
                selected = 1,
                handler = function(h, ...) {
                    responseFamily <<- svalue(h$obj, index = TRUE)
                    updateModel()
                }
            )
            visible(responseFamilyBox) <<- FALSE

            frameworkGp <- gvbox()
            add(frameworkGp, responseTypeBox)
            add(frameworkGp, responseFamilyBox)

            responseTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- flbl
            responseTbl[ii, 2:3, expand = TRUE] <- frameworkGp
            ii <- ii + 1


            ## Transform response (Y)
            lbl <- glabel("Transformation: ")
            responseTransformBox <- gcombobox(
                c("", "log", "exp", "square root", "inverse"),
                editable = TRUE,
                handler = function(h, ...) {
                    responseTransform <<- svalue(h$obj)
                    updateModel()
                }
            )
            responseTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            responseTbl[ii, 2:3, expand = TRUE] <- responseTransformBox
            ii <- ii + 1



            variableGp <- gexpandgroup(
                "Explanatory Variables (drag+drop/double-click)",
                horizontal = FALSE,
                container = mainGrp
            )
            variableGp$set_borderwidth(10)
            variableTbl <- glayout(homogeneous = TRUE, container = variableGp)

            contVarBox <<- gtable("")
            catVarBox <<- gtable("")
            setAvailVars()
            variableTbl[1:6, 1, expand = TRUE] <- contVarBox
            variableTbl[7:12, 1, expand = TRUE] <- catVarBox
            size(contVarBox) <<- c(-1, 150)
            size(catVarBox) <<- c(-1, 150)

            intercept <<- gcheckbox("Include intercept", checked = TRUE)
            variableTbl[2, 2, expand = TRUE, anchor = c(-1, 0)] <- intercept

            explanatoryList <<- gtable("")
            setExplVars()
            variableTbl[3:8, 2, expand = TRUE] <- explanatoryList

            confounderList <<- gtable("")
            setConfVars()
            variableTbl[9:12, 2, expand = TRUE] <- confounderList


            ## Right-panel controls (up/down/edit)
            pnlControls <- ggroup()
            addSpring(pnlControls)
            btnEdit <- gbutton(stock.id = "",
                handler = function(h, ...) {},
                container = pnlControls,
                tooltip = "Modify Variable (Transform, etc)"
            )
            btnEdit$set_icon("properties")
            addSpace(pnlControls, 5)
            btnUp <- iNZight:::gimagebutton(
                stock.id = "up",
                handler = function(h, ...) {
                    w <- svalue(explanatoryList, index = TRUE)
                    if (length(w) != 1 || w == 1) return(NULL)
                    ord <- seq_along(variables)
                    ord[w-1] <- w
                    ord[w] <- w-1
                    variables <<- variables[ord]
                    setExplVars()
                    svalue(explanatoryList, index = TRUE) <<- w-1
                },
                container = pnlControls,
                tooltip = "Move Variable Up"
            )
            addSpace(pnlControls, 5)
            btnDown <- iNZight:::gimagebutton(
                stock.id = "down",
                handler = function(h, ...) {
                    w <- svalue(explanatoryList, index = TRUE)
                    if (length(w) != 1 || w == length(variables)) return(NULL)
                    ord <- seq_along(variables)
                    ord[w+1] <- w
                    ord[w] <- w+1
                    variables <<- variables[ord]
                    setExplVars()
                    svalue(explanatoryList, index = TRUE) <<- w+1
                },
                container = pnlControls,
                tooltip = "Move Variable Down"
            )
            variableTbl[1, 2, expand = TRUE] <- pnlControls

            ## Right-click menus

            ## - for numeric variables:
            transforms <- list(
                "I(x^2)",
                "I(x^3)",
                "POWER",
                "POLY",
                "sqrt(x)",
                "log(x)",
                "exp(x)",
                "SEP",
                "as.factor(x)",
                "OTHER"
            )
            transformList <- function(var = "x", box, replace = FALSE) {
                lapply(transforms,
                    function(x) {
                        if (x == "SEP") return(gseparator())
                        if (var != "x") x <- gsub("x", var, x)
                        wx <- svalue(box, TRUE)

                        if (x == "POWER") {
                            return(gaction(sprintf("%s^z: other power ...", var),
                                handler = function(h, ...) {
                                    xname <- svalue(box, index = FALSE)
                                    zw <- gbasicdialog("Choose order of power",
                                        handler = function(h,...) {
                                            z <- as.numeric(svalue(zval))
                                            if (is.na(z))
                                                gmessage(
                                                    "Order must be a number.",
                                                    "Invalid Value",
                                                    "error",
                                                    parent = h$obj
                                                )
                                            else {
                                                addTransform(xname,
                                                    paste0("I(", var, "^", z, ")"),
                                                    replace = replace
                                                )
                                                if (replace)
                                                    svalue(box, TRUE) <- wx
                                            }
                                        },
                                        parent = GUI$win
                                    )
                                    zg <- ggroup(cont=zw)
                                    zt <- glayout(
                                        homogeneous = TRUE,
                                        container = zg
                                    )
                                    zt[1,1,anchor=c(1,0), expand = TRUE] <-
                                        glabel(paste0(xname, "^"))
                                    zt[1,2] <- (zval <- gedit(4, width = 4, container = zt))
                                    out <- visible(zw)
                                }
                            ))
                        }

                        if (x == "POLY") {
                            return(gaction("polynomial to degree ...",
                                handler = function(h, ...) {
                                    xname <- svalue(box, index = FALSE)
                                    zw <- gbasicdialog("Choose order of polynomial",
                                        handler = function(h,...) {
                                            z <- as.numeric(svalue(zval))
                                            if (is.na(z))
                                                gmessage(
                                                    "Order must be a number.",
                                                    "Invalid Value",
                                                    "error",
                                                    parent = h$obj
                                                )
                                            else {
                                                addTransform(xname,
                                                    paste0("Poly(", var, ", ", z, ")"),
                                                    replace = replace
                                                )
                                                if (replace)
                                                    svalue(box, TRUE) <- wx
                                            }
                                        },
                                        parent = GUI$win
                                    )
                                    zg <- ggroup(cont=zw)
                                    zt <- glayout(homogeneous = TRUE,
                                        container = zg
                                    )
                                    zt[1,1,anchor=c(1,0), expand = TRUE] <-
                                        glabel(paste0("poly(", xname, ","))
                                    zt[1,2] = (zval <- gspinbutton(1, 50, 1,
                                        value = 2,
                                        container = zt
                                    ))
                                    zt[1,3, anchor= c(-1,0), expand=TRUE] <-
                                        glabel(")")
                                    out <- visible(zw)
                                }
                            ))
                        }

                        if (x == "OTHER") {
                            return(gaction("Other ...",
                                handler = function(h, ...) {
                                    xname <- svalue(box, index = FALSE)
                                    zw <- gbasicdialog(
                                        "Specify other transformation",
                                        handler = function(h,...) {
                                            try(addTransform(
                                                sprintf("%s(%s)",
                                                    svalue(zfun),
                                                    svalue(zargs)
                                                ),
                                                replace = replace
                                            ))
                                            if (replace)
                                                svalue(box, TRUE) <- wx
                                            invisible(NULL)
                                        },
                                        parent = GUI$win
                                    )
                                    zg <- gvbox(cont=zw)
                                    zt <- glayout(
                                        homogeneous = FALSE,
                                        container = zg
                                    )
                                    zfun <- gedit("",
                                        width = 10,
                                        initial.msg = "function"
                                    )
                                    zargs <- gedit(xname, width = 15)

                                    ## This needs altering
                                    zt[1,1] <- glabel("Specify transformation :")
                                    zt[1,2] <- zfun
                                    zt[1,3] <- glabel("(")
                                    zt[1,4] <- zargs
                                    zt[1,5] <- glabel(")")
                                    glabel(
                                        "Include comma-separated arguments if necessary",
                                        container = zg
                                    )
                                   out <- visible(zw)
                                }
                            ))
                        }

                        lbl <- x
                        if (grepl("I(", lbl, fixed = TRUE)) {
                            lbl <- gsub(":.+", "",
                                gsub(")", "",
                                    gsub("I(", "", lbl, fixed = TRUE),
                                    fixed = TRUE
                                )
                            )
                        }

                        gaction(lbl,
                            handler = function(h, ...) {
                                xname <- svalue(box, index = FALSE)
                                addTransform(xname, x, replace = replace)
                                if (replace) svalue(box, TRUE) <- wx
                            }
                        )
                    }
                )
            }

            factorTransformList <- function(var = "x", box, replace = FALSE) {
                wx <- svalue(box, TRUE)
                list(
                    "Reset baseline ..." =
                        if (var == "x") {
                            gaction("Reset baseline ...",
                                handler = function(h, ...) {
                                    xname <- svalue(box, index = FALSE)
                                    zw <- gbasicdialog("Create Interaction",
                                        handler = function(h, ...) {
                                            ref <- svalue(lev, index = FALSE)
                                            try(addTransform(xname, sprintf("relevel(x, \"%s\")", ref), replace = replace))
                                            if (replace) svalue(box, TRUE) <- wx
                                            invisible(NULL)
                                        }
                                    )
                                    zg <- gvbox(container = zw)
                                    size(zw) <- c(280, 400)
                                    glabel(
                                        sprintf(
                                            "Choose level of %s to set as baseline",
                                            xname
                                        ),
                                        container = zg
                                    )
                                    vs <- levels(getdata()[[xname]])
                                    lev <- gtable(vs, container = zg)
                                    out <- visible(zw)
                                }
                            )
                        } else {
                            lapply(levels(getdata()[[var]]),
                                function(lvl) {
                                    gaction(lvl,
                                        handler = function(h, ...) {
                                            try(addTransform(var,
                                                sprintf("relevel(x, \"%s\")", lvl),
                                                replace = replace
                                            ))
                                            if (replace)
                                                svalue(box, TRUE) <- wx
                                            invisible(NULL)
                                        }
                                    )
                                }
                            )
                        }
                )
            }

            interactList <- function(box) {
                list(
                    gaction("Interact with ...",
                        handler = function(h, ...) {
                            xname <- svalue(box, index = FALSE)
                            zw <- gbasicdialog("Create Interaction",
                                handler = function(h, ...) {
                                    vs <- c(xname, svalue(intvar, index = FALSE))

                                    if (any(vs %in% variables))
                                        variables <<-
                                            variables[-which(variables %in% vs)]

                                    if (svalue(zval) == 0) {
                                        vv <- paste(vs, collapse = " * ")
                                    } else {
                                        vtxt <- sprintf("(%s)^%s",
                                            paste(vs, collapse = " + "),
                                            svalue(zval) + 1
                                        )
                                        f <- as.formula(paste0("~", vtxt))
                                        vv <- attr(terms.formula(f), "term.labels")
                                    }

                                    ## clean up clutter ...
                                    if (any(vv %in% variables))
                                        variables <<-
                                            variables[-which(variables %in% vv)]

                                    variables <<- c(variables, vv)
                                    setExplVars()
                                }
                            )

                            zg <- gvbox(container = zw)
                            size(zw) <- c(280, 400)
                            glabel(
                                sprintf(
                                    "Choose variables to interact with %s\n(CTRL to select many)",
                                    xname
                                ),
                                container = zg
                            )
                            vs <- c(
                                contVarBox$get_items(),
                                catVarBox$get_items()
                            )
                            intvar <- gtable(vs[vs != xname],
                                multiple = TRUE,
                                container = zg
                            )
                            zg2 <- ggroup(container = zg)
                            glabel("How many interaction terms?\n(0 = all) :",
                                container = zg2
                            )
                            zval <- gspinbutton(0, length(svalue(intvar)), 1,
                                container = zg2
                            )
                            enabled(zval) <- length(svalue(intvar)) > 0
                            ## when interactions chosen, change upper limit of zval
                            addHandlerSelectionChanged(intvar,
                                handler = function(h, ...) {
                                    n <- length(svalue(h$obj))
                                    zval$set_items(0:n)
                                    enabled(zval) <- n > 0
                                    invisible(NULL)
                                }
                            )
                            out <- visible(zw)
                        }
                    )
                )
            }
            addRightclickPopupMenu(contVarBox,
                c(
                    transformList(box = contVarBox),
                    list(gseparator()),
                    interactList(contVarBox)
                )
            )
            addRightclickPopupMenu(catVarBox,
                c(
                    factorTransformList(box = catVarBox),
                    list(gseparator()),
                    interactList(catVarBox)
                )
            )

            addHandlerChanged(intercept,
                handler = function(h, ...) {
                    updateModel()
                }
            )

            enabled(btnUp) <- enabled(btnEdit) <- enabled(btnDown) <- FALSE
            btnEditSig <<- NULL
            addHandlerSelectionChanged(explanatoryList,
                function(h, ...) {
                    enabled(btnEdit) <- length(svalue(h$obj)) == 1
                    enabled(btnUp) <- length(svalue(h$obj)) == 1 &&
                        svalue(h$obj, TRUE) > 1
                    enabled(btnDown) <- length(svalue(h$obj)) == 1 &&
                        svalue(h$obj, TRUE) < length(variables)

                    ## Now set the edit list - remove existing first!
                    if (!is.null(btnEditSig)) {
                        gSignalHandlerDisconnect(btnEdit$widget, btnEditSig)
                        btnEditSig <<- NULL
                    }
                    if (length(svalue(h$obj)) != 1) return()
                    vn <- svalue(h$obj, FALSE)
                    if (!vn %in% names(getdata())) {
                        opts <- list()

                        if (grepl(".+\\*.+", vn)) {
                            ## It's an interaction!
                            opts <- c(
                                opts,
                                list(gaction("Expand interaction terms",
                                    handler = function(h, ...) {
                                        f <- as.formula(paste0("~", vn))
                                        variables <<-
                                            c(
                                                variables[-which(variables == vn)],
                                                attr(terms.formula(f), "term.labels")
                                            )
                                        setExplVars()
                                    }
                                ))
                            )
                        } else {
                            ## removing transformation
                            opts <- c(opts,
                                list(
                                    gaction("Remove transformation",
                                        handler = function(h, ...) {
                                            ## figure out what the variable is ...
                                            v <- svalue(explanatoryList, TRUE)
                                            var <- gsub(".+\\(|,.+|\\).*|\\^.+", "",
                                                variables[v]
                                            )
                                            if (var %in% names(getdata())) {
                                                variables[v] <<- var
                                                setExplVars()
                                                svalue(explanatoryList, TRUE) <<- v
                                            }
                                        }
                                    )
                                )
                            )
                        }
                        btnEditSig <<- addPopupMenu(btnEdit, opts)
                    } else if (vn %in% numericVars()) {
                        btnEditSig <<- addPopupMenu(btnEdit,
                            c(
                                transformList(vn, box = explanatoryList, replace = TRUE),
                                list(gseparator()),
                                interactList(explanatoryList))
                            )
                    } else {
                        btnEditSig <<- addPopupMenu(btnEdit,
                            c(
                                factorTransformList(vn, box = explanatoryList, replace = TRUE),
                                list(gseparator()),
                                interactList(explanatoryList)
                            )
                        )
                    }
                }
            )

            ## Drag-and-drop behaviour
            addDropSource(contVarBox,
                type = "object",
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    attr(varname, "from") <- "avail"
                    varname
                }
            )
            addDropSource(catVarBox,
                type = "object",
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    attr(varname, "from") <- "avail"
                    varname
                }
            )
            addDropSource(explanatoryList,
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    variables <<- variables[variables != varname]
                    working <<- working + 1L
                    setExplVars()
                    working <<- working - 1L
                    varname
                }
            )
            addDropSource(confounderList,
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    confounding <<- confounding[confounding != varname]
                    working <<- working + 1L
                    setConfVars()
                    working <<- working - 1L
                    varname
                }
            )

            addDropTarget(explanatoryList,
                handler =  function(h, ...) {
                    varname <- h$dropdata
                    if (varname %in% c(variables, confounding)) return()
                    variables <<- c(variables, varname)
                    setExplVars()
                }
            )
            addDropTarget(confounderList,
                handler = function(h, ...) {
                    varname <- h$dropdata
                    if (varname %in% c(variables, confounding)) return()
                    confounding <<- c(confounding, varname)
                    setConfVars()
                }
            )

            ## double click behaviour
            addHandlerDoubleclick(contVarBox,
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    if (length(varname) != 1) return()
                    if (varname %in% c(variables, confounding)) return()
                    variables <<- c(variables, varname)
                    setExplVars()
                }
            )
            addHandlerDoubleclick(catVarBox,
                handler = function(h, ...) {
                    varname <- svalue(h$obj)
                    if (length(varname) != 1) return()
                    if (varname %in% c(variables, confounding)) return()
                    if (any(grepl(sprintf("relevel\\(%s", varname),
                        c(variables, confounding)))) return()
                    if (varname %in% attr(fit$terms, "term.labels")) return()
                    variables <<- c(variables, varname)
                    setExplVars()
                }
            )
            addHandlerDoubleclick(explanatoryList,
                handler = function(h, ...) {
                    if (length(svalue(h$obj)) != 1) return()
                    variables <<- variables[variables != svalue(h$obj)]
                    setExplVars()
                }
            )
            addHandlerDoubleclick(confounderList,
                handler = function(h, ...) {
                    if (length(svalue(h$obj)) != 1) return()
                    confounding <<- confounding[confounding != svalue(h$obj)]
                    setConfVars()
                }
            )



            ## -----------------------------------------------------------------
            ## Model options

            modelGp <- gexpandgroup("Manage Saved Models",
                horizontal = FALSE,
                container = mainGrp
            )
            visible(modelGp) <- FALSE
            modelGp$set_borderwidth(10)
            modelTbl <- glayout(homogeneous = TRUE, container = modelGp)
            ii <- 1

            modelList <<- gcombobox(c("(new)", names(fits)), ## if (length(fits) > 0) names(fits) else "(new)",
                handler = function(h, ...) {
                    ## reset response, framework, variables ...
                    if (svalue(h$obj, index = TRUE) == 1) {
                        newBtn$invoke_change_handler()
                        return()
                    }
                    enabled(renameBtn) <- TRUE
                    enabled(newBtn) <- TRUE
                    obj <- fits[[svalue(h$obj, index = TRUE) - 1]]
                    working <<- working + 1L
                    svalue(responseBox) <<- obj$response
                    svalue(responseTypeBox, index = TRUE) <<-
                        obj$responseType
                    svalue(responseFamilyBox, index = TRUE) <<-
                        obj$responseFamily
                    svalue(responseTransformBox, index = FALSE) <-
                        obj$responseTransform
                    variables <<- obj$variables
                    setExplVars()
                    confounding <<- obj$confounding
                    setConfVars()
                    modelName <<- svalue(h$obj)
                    blockHandlers(saveBtn)
                    svalue(saveBtn) <- "Update"
                    unblockHandlers(saveBtn)
                    working <<- working - 1L
                    fit <<- obj$fit
                    updateModel(new = FALSE)
                }
            )

            newBtn <- gbutton("New",
                handler = function(h, ...) {
                    modelName <<- paste("Model", length(fits) + 1)
                    blockHandlers(modelList)
                    svalue(modelList, index = TRUE) <<- 1
                    unblockHandlers(modelList)
                    blockHandlers(saveBtn)
                    svalue(saveBtn) <- "Save Model"
                    unblockHandlers(saveBtn)
                    enabled(renameBtn) <- FALSE
                    enabled(newBtn) <- FALSE
                }
            )

            modelName <<- sprintf("Model %i", length(fits) + 1)
            renameBtn <- gbutton("Rename",
                handler = function(h, ...) {
                    modelName <<- ginput("Type the new name for the model",
                        title = "Rename Model",
                        text = modelName, icon = "question",
                        parent = GUI$win
                    )
                    updateModel(save = TRUE)
                }
            )
            saveBtn <- gbutton("Save Model",
                handler = function(h, ...) {
                    updateModel(save = TRUE)
                    blockHandler(h$obj)
                    svalue(h$obj) <- "Update"
                    unblockHandler(h$obj)
                    enabled(renameBtn) <- TRUE
                    enabled(newBtn) <- TRUE
                }
            )

            enabled(renameBtn) <- FALSE
            enabled(newBtn) <- FALSE

            modelTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <-
                glabel("Select Model :")
            modelTbl[ii, 2:3, expand = TRUE, fill = TRUE] <- modelList
            ii <- ii + 1
            modelTbl[ii, 1, expand = TRUE, fill = TRUE] <- newBtn
            modelTbl[ii, 2, expand = TRUE, fill = TRUE] <- saveBtn
            modelTbl[ii, 3, expand = TRUE, fill = TRUE] <- renameBtn
            ii <- ii + 1

            # display model comparison information (AIC, BIC)
            compBtn <<- gbutton("Compare models by AIC, BIC",
                handler = function(h, ...) {
                    fts <- fits
                    names(fts) <- gsub(" ", "_", names(fts))
                    e <- new.env()
                    z <- lapply(names(fts),
                        function(fit) assign(fit, fts[[fit]]$fit, envir = e)
                    )
                    rm("z")

                    ## if survey, must add design object to e too
                    cleanup <- FALSE
                    if (isSurveyObject) {
                        cleanup <- TRUE
                        if (!"dataSet" %in% ls(.GlobalEnv))
                            assign("dataSet", getdata(), envir = .GlobalEnv)
                        else cleanup <- FALSE
                        if (!"svy.design" %in% ls(.GlobalEnv))
                            assign("svy.design", getdesign(), envir = .GlobalEnv)
                        else cleanup <- FALSE
                    }

                    expr <- sprintf("iNZightRegression::compare_models(%s)",
                        paste(names(fts), collapse = ", ")
                    )
                    x <- suppressWarnings(eval(parse(text = expr), envir = e))
                    o <- capture.output(print(x))
                    o <- gsub("_", " ", o)
                    addOutput(o)
                    rule()
                    summaryOutput <<- svalue(smryOut)

                    # clean up
                    if (cleanup) {
                        rm("dataSet", envir = .GlobalEnv)
                        rm("svy.design", envir = .GlobalEnv)
                    }
                }
            )
            modelTbl[ii, 2:3, expand = TRUE, fill = TRUE] <- compBtn
            ii <- ii + 1

            ## -----------------------------------------------------------------
            ## Plot options

            plotGp <- gexpandgroup("Model/Residual Plots, Factor Comparisons",
                horizontal = FALSE,
                container = mainGrp
            )
            visible(plotGp) <- FALSE
            plotGp$set_borderwidth(10)
            plotTbl <- glayout(
                homogeneous = TRUE,
                container = plotGp,
                expand = TRUE
            )
            ii <- 1

            showBoots <<- gcheckbox("Show bootstraps",
                checked = nrow(getdata()) >= 30 && nrow(getdata()) < 4000,
                handler = function(h, ...) updatePlot()
            )
            plotTbl[ii, 3:6, anchor = c(-1, 0)] <- showBoots
            ii <- ii + 1

            lbl <- glabel("Residual plots :")
            plotTypeList <<- gcombobox(
                c(
                    "Residual", "Scale-Location", "Leverage", "Cook's Distance",
                    "Normal Q-Q", "Histogram", "Summary Matrix", "Partial Residual",
                    "Factor Comparisons", "Marginal Model Plots"
                ),
                handler = function(h, ...) {
                    plottype <<- svalue(h$obj, index = TRUE)
                    updatePlot()
                }
            )
            plotTbl[ii, 1:2, anchor = c(1, 0)] <- lbl
            plotTbl[ii, 3:6, expand = TRUE] <- plotTypeList
            ii <- ii + 1

            numVarList <<- gcombobox("",
                handler = function(h, ...) updatePlot()
            )
            visible(numVarList) <<- FALSE
            plotTbl[ii, 3:6, expand = TRUE] <- numVarList


            catVarList <<- gcombobox("",
                handler = function(h, ...) updatePlot()
            )
            visible(catVarList) <<- FALSE
            plotTbl[ii, 3:6, expand = TRUE] <- catVarList
            ii <- ii + 1

            compMatrix <<- gbutton("Comparison Matrix",
                handler = function(h, ...) {
                    out <- capture.output(
                        #iNZightMR::moecalc(fit, svalue(catVarList))
                        print(
                            iNZightRegression::factorComp(fit,
                                svalue(catVarList)
                            )
                        )
                    )
                    addOutput(out)
                    rule()
                    summaryOutput <<- svalue(smryOut)
                }
            )
            visible(compMatrix) <<- FALSE
            plotTbl[ii, 3:6, expand = TRUE] <- compMatrix


            ## -----------------------------------------------------------------
            ## Model comparisons

            # compGp <- gexpandgroup("Model Comparison",
            #     horizontal = FALSE,
            #     container = mainGrp
            # )
            # visible(compGp) <- FALSE
            # compGp$set_borderwidth(10)
            # compTbl <- glayout(
            #     homogeneous = TRUE,
            #     container = compGp,
            #     expand = TRUE
            # )
            # ii <- 1

            # comparison criteria
            # compTypes <- gcombobox(
            #     c("AIC", "BIC")
            #     # handler = function(h, ...) {
            #     #     model_comp <<- svalue(h$obj)
            #     # }
            # )
            # compBtn <<- gbutton("Compare",
            #     handler = function(h, ...) {
            #         fts <- lapply(fits, function(x) x$fit)
            #         # construct expression using names of `fts`
            #         # expr <- sprintf("with(fts, %s(%s))",
            #         #     "BIC", #svalue(compTypes),
            #         #     paste0(
            #         #         "`",
            #         #         paste(names(fts), collapse = "`, `"),
            #         #         "`"
            #         #     )
            #         # )

            #

            #         # x <- tryCatch(
            #         #     suppressWarnings(
            #         #         eval(parse(text = expr))
            #         #     ),
            #         #     error = "Unable to compare these models"
            #         # )
            #         svy.design <- mod$getdesign()
            #         x <- do.call(
            #             # eval(parse(text = svalue(compTypes))),
            #             "AIC",
            #             fts
            #         )
            #         rownames(x) <- NULL
            #         x <- data.frame(Model = names(fits), x)
            #         addOutput(capture.output(print(x, row.names = FALSE)))
            #         if (!all(diff(sapply(fts, function(f) length(f$residuals))) == 0))
            #             addOutput("", "Note: models are not all fitted to the same number of observations")
            #         rule()
            #         summaryOutput <<- svalue(smryOut)
            #     }
            # )
            # compTbl[ii, 1:2, anchor = c(1, 0)] <- glabel("Criteria:")
            # compTbl[ii, 3:6, expand = TRUE] <- compTypes
            # ii <- ii + 1
            # compTbl[ii, 3:6, expand = TRUE] <- compBtn
            # ii <- ii + 1


            ## Now create new tab for SUMMARY output:
            smryOut <<- gtext()
            if (GUI$popOut) addInstructions(smryOut)
            outputWin <<- NULL
            .self$showOutput()

            regPlots <<- ggraphics(expand = TRUE)
            if (GUI$popOut) {
                ## nothing to do?
            } else {
                add(GUI$plotWidget$plotNb, regPlots,
                    label = "Model Plots",
                    close.button = FALSE
                )
                ## Display instructions in a closable tab
                inst <- gtext()
                add(GUI$plotWidget$plotNb, inst,
                    label = "Instructions",
                    close.button = TRUE
                )
                addInstructions(inst)
            }
            plot(NA,
                xlim = 0:1,
                ylim = 0:1,
                xlab = "",
                ylab = "",
                xaxt = "n",
                yaxt = "n",
                bty = "n"
            )
            text(0.5, 0.5, "Select a response variable to begin", cex = 1.2)


            ## So now, can swith between text and plot tabs ...
            showTab("instructions")

            working <<- 0L

            watchData()
            ## add to code output
            if (!is.null(getdesign())) {
                svyname <<- sprintf("%s",
                    GUI$getActiveDoc()$getModel()$dataDesignName
                )
            }

            invisible(10)
        }, # end initialize()
        showOutput = function() {
            if (GUI$popOut) {
                if (is.null(outputWin)) {
                    outputWin <<- gwindow("iNZight Model Fitting Output",
                        width = 800,
                        height = 400,
                        parent = GUI$win
                    )
                    addHandlerDestroy(outputWin,
                        handler = function(h, ...) {
                            outputWin <<- NULL
                            # the old one will be destroyed,
                            # so recreate an empty one so other code
                            # doesn't wall over
                            smryOut <<- gtext()
                            TRUE
                        }
                    )
                    tg <- ggroup(cont = outputWin)
                    add(tg, smryOut, expand = TRUE, fill = TRUE)

                    # repopulate smryOut value
                    updateModel()
                }
            } else {
                pb.i <- svalue(GUI$plotWidget$plotNb)
                add(GUI$plotWidget$plotNb, smryOut,
                    label = "Model Output",
                    close.button = FALSE
                )
                svalue(GUI$plotWidget$plotNb) <<- pb.i
                GUI$plotWidget$closePlot()
            }
        },
        showPlot = function() {

        },
        getdata = function() {
            des <- getdesign()
            if (!is.null(des)) return(des$variables)
            GUI$getActiveData()
        },
        getdesign = function() {
            if (is.null(GUI$getActiveDoc()$dataModel$getDesign())) return(NULL)
            des <- GUI$getActiveDoc()$dataModel$createSurveyObject()

            ## set the name
            dname <- attr(GUI$getActiveData(), "name", exact = TRUE)
            if (is.null(dname) || dname == "")
                dname <- sprintf("data%s",
                    ifelse(GUI$activeDoc == 1, "", GUI$activeDoc)
                )
            dname <- iNZightTools:::create_varname(dname)
            des$call$data <- eval(parse(text = sprintf("quote(%s)", dname)))
            des
        },
        setAvailVars = function() {
            if (is.null(response) || length(response) == 0) {
                vars <- "Select response"
            } else {
                vars <- names(getdata()[,-which(names(getdata()) == response), drop = FALSE])
                if (length(variables) && response %in% variables) {
                    variables <<- variables[variables != response]
                    setExplVars()
                }
                if (length(confounding) && response %in% confounding) {
                    confounding <<- confounding[confounding != response]
                    setConfVars()
                }

                numvars <- sapply(getdata()[vars], is.numeric, USE.NAMES = FALSE)
                v1 <- vars[numvars]
                v2 <- vars[!numvars]
                contVarBox$set_items(
                    structure(
                        data.frame(v1,
                            stringsAsFactors = FALSE
                        ),
                        names = "Numeric Variables"
                    )
                )
                catVarBox$set_items(
                    structure(
                        data.frame(v2,
                            stringsAsFactors = FALSE
                        ),
                        names = "Categorical Variables"
                    )
                )
            }
        },
        setExplVars = function () {
            variables <<- unique(variables)
            explanatoryList$set_items(
                structure(
                    data.frame(variables, stringsAsFactors = FALSE),
                    names = "Variables of Interest"
                )
            )
            updateModel()
        },
        setConfVars = function() {
            confounderList$set_items(
                structure(
                    data.frame(confounding, stringsAsFactors = FALSE),
                    names = "Confounding Variables"
                )
            )
            updateModel()
        },
        addTransform = function(var, fun, replace = FALSE, remove) {
            if (!missing(fun)) {
                fn <- gsub("x", "%s", fun)
                nv <- sprintf(fn, var)
            } else nv <- var
            ## remove any variables (e.g., when adding interaction)
            if (!missing(remove) && any(remove %in% variables))
                variables <<- variables[-which(variables %in% remove)]
            if (replace) {
                variables[which(variables == var)] <<- nv
            } else if (! nv %in% variables ) {
                variables <<- c(variables, nv)
            }
            setExplVars()
        },
        showTab = function(x = c("plot", "summary", "instructions")) {
            if (GUI$popOut) return(invisible(NULL))
            x <- match.arg(x)
            svalue(GUI$plotWidget$plotNb) <<-
                which(names(GUI$plotWidget$plotNb) ==
                    switch(x,
                        "plot" = "Model Plots",
                        "summary" = "Model Output",
                        "instructions" = "Instructions"
                    )
                )
            invisible(NULL)
        },
        addOutput = function(..., font.attr = list(family = "monospace")) {
            ## showTab("summary")
            sapply(list(...), insert, obj = smryOut, font.attr = font.attr)
        },
        rule = function(char = "-") {
            addOutput("", paste0(rep(char, 80), collapse = ""), "")
        },
        modelVars = function() {
            if (is.null(fit)) return(character())
            names(fit$model)[-1]
        },
        numericVars = function(index = FALSE) {
            names(which(attr(fit$terms, "dataClasses")[-1] == "numeric"))
            ## ind <- which(sapply(getdata()[modelVars()], is.numeric))
            ## if (!index) return(modelVars()[ind])
            ## ind
        },
        factorVars = function(index = FALSE) {
            names(which(attr(fit$terms, "dataClasses")[-1] == "factor"))
            ## ind <- which(!sapply(getdata()[modelVars()], is.numeric))
            ## if (!index) return(modelVars()[ind])
            ## ind
        },
        updateModel = function(new = TRUE, save = FALSE) {
            if (working > 0) return()

            if (length(response) == 0) return()

            xexpr <- paste(
                c(
                    if (length(variables) > 0) variables else "1",
                    confounding
                ),
                collapse = " + "
            )
            if (!svalue(intercept)) xexpr <- paste(xexpr, "- 1")

            dataset <- if (isSurveyObject) getdesign() else getdata()
            resp <- response
            if (length(responseTransform) == 1 && responseTransform != "") {
                trans <- responseTransform
                if (trans == "inverse") {
                    resp <- paste0("1 / ", response)
                } else {
                    if (trans == "square root") trans <- "sqrt"
                    resp <- sprintf("%s(%s)", trans, response)
                }
            }
            mcall <- NULL
            if (new) {
                svy.design <- getdesign()
                mcall <- iNZightTools::fitModel(resp, xexpr,
                    data = "dataset",
                    family = switch(responseType,
                        "gaussian",
                        "binomial",
                        "poisson",
                        "negbin"
                    ),
                    link = switch(responseType,
                        NULL,
                        switch(responseFamily,
                            "logit",
                            "probit"
                        ),
                        NULL
                    ),
                    design = ifelse(isSurveyObject, "survey", "simple")
                )
                fit <<- try(eval(parse(text = mcall)), TRUE)
            }

            svalue(smryOut) <<- ""
            addOutput(summaryOutput)
            rule()

            addOutput(paste0(
                "# Summary of ", modelName, ": ", resp, " ~ ",
                if (length(variables)) paste(variables, collapse = " + ") else "1"
            ))
            if (inherits(fit, "try-error")) {
                addOutput("Unable to fit model.")
                rule()
                return()
            } else {
                wd <- options()$width
                options(width = 200)
                fito <- try(
                    capture.output(
                        iNZightRegression::iNZightSummary(
                            fit,
                            exclude = if (length(confounding) > 0) confounding else NULL
                        )
                    ),
                    silent = TRUE
                )
                if (inherits(fito, "try-error"))
                    fito <- try(
                        capture.output(summary(fit)),
                        silent = TRUE
                    )
                if (inherits(fito, "try-error"))
                    fito <- "Unable to obtain summary information for model."
                addOutput(fito)
                options(width = wd)
                ## plot it

                if (save) {
                    obj <- list(
                        fit = fit,
                        response = response,
                        responseType = responseType,
                        responseFamily = responseFamily,
                        responseTransform = responseTransform,
                        variables = variables,
                        confounding = confounding
                    )
                    if (svalue(modelList, index = TRUE) == 1) {
                        ## Creating a new model
                        fits <<- c(fits,
                            structure(list(obj), .Names = modelName)
                        )
                    } else {
                        ## Updating an existing model
                        fits[[svalue(modelList, index = TRUE) - 1]] <<- obj
                        names(fits)[svalue(modelList, index = TRUE) - 1] <<-
                            modelName
                    }
                    blockHandlers(modelList)
                    modelList$set_items(c("(new)", names(fits)))
                    svalue(modelList) <<- modelName
                    unblockHandlers(modelList)
                    summaryOutput <<- svalue(smryOut)

                    if (!is.null(mcall)) {
                        fname <- sprintf("fit%s",
                            ifelse(svalue(modelList, TRUE) == 2,
                                "",
                                svalue(modelList, TRUE) - 1
                            )
                        )
                        dname <- sprintf("data%s",
                            ifelse(GUI$activeDoc == 1, "", GUI$activeDoc)
                        )
                        GUI$rhistory$add(
                            c(
                                sprintf("%s <- %s",
                                    fname,
                                    gsub("svy.design", svyname,
                                        gsub("dataset", dname, mcall)
                                    )
                                ),
                                sprintf("iNZightSummary(%s%s)", fname,
                                    ifelse(length(confounding) == 0,
                                        "",
                                        sprintf(", exclude = c(\"%s\")",
                                            paste(confounding,
                                                collapse = "\",\""
                                            )
                                        )
                                    )
                                )
                            ),
                            keep = TRUE
                        )
                        if (!is.null(codehistory)) {
                            svalue(codehistory) <<- ""
                            sapply(GUI$rhistory$get(), insert,
                                obj = codehistory,
                                font.attr = list(family = "monospace")
                            )
                        }
                    }
                } # end if save
            }

            rule()


            updatePlot()
        }, # updateModel()
        updatePlot = function(savecode = FALSE) {
            dev.hold()
            on.exit(dev.flush())
            visible(compMatrix) <<-
                visible(catVarList) <<-
                visible(numVarList) <<- FALSE

            e <- new.env()
            assign("dataset", getdata(), e)

            fitn <- sprintf("fit%s",
                ifelse(svalue(modelList, TRUE) == 2,
                    "",
                    svalue(modelList, TRUE) - 1
                )
            )
            ## Is the model the one that's saved? Or is it a temporary one ...
            mcall <- ""
            if (savecode && (length(fits) == 0 ||
                !identical(fit, fits[[svalue(modelList, TRUE) - 1]]$fit))) {

                mcall <- gsub("formula = ", "",
                    paste(capture.output(fit$call), collapse = "\n")
                )
                dname <- sprintf("data%s",
                    ifelse(GUI$activeDoc == 1, "", GUI$activeDoc)
                )
                fname <- "tmpfit"
                mcall <- sprintf("%s <- %s", fname,
                    gsub("dataset", dname, mcall)
                )
                fitn <- fname
            }

            fmla <- character()

            if (plottype %in% 1:7) {

                if (isSurveyObject) {
                    if (svalue(showBoots) && plottype %in% 5:6) {
                        if (plottype == 5) {
                            iNZightRegression::iNZightQQplot(fit, env = e)
                            fmla <- sprintf("iNZightQQplot(%s)", fitn)
                        } else {
                            iNZightRegression::histogramArray(fit, env = e)
                            fmla <- sprintf("histogramArray(%s)", fitn)
                        }
                    } else {
                        ## I want to do bootstrapping, therefore I need to pass in the environment
                        iNZightRegression::plotlm6(fit,
                            which = plottype,
                            showBootstraps = svalue(showBoots),
                            env = e
                        )
                        fmla <- sprintf(
                            "plotlm6(%s, which = %s, showBootstraps = %s)",
                            fitn,
                            plottype,
                            ifelse(svalue(showBoots), "TRUE", "FALSE")
                        )
                    }
                } else {
                    plottypes <- c("residual", "scale", "leverage", "cooks", "normal", "hist")
                    ptype <- if (plottype == 7) plottypes else plottypes[plottype]
                    
                    iNZightRegression::inzplot(fit, 
                        which = ptype, 
                        show.bootstraps = svalue(showBoots),
                        env = e
                    )
                    fmla <- sprintf("iNZightRegression::inzplot(%s, which = '%s', show.bootstraps = %s)",
                        fitn,
                        ptype, 
                        ifelse(svalue(showBoots), "TRUE", "FALSE")
                    )
                }

                
            } else if (plottype == 8) {
                numvars <- numericVars()
                if (length(numvars) == 0) {
                    numvars <- ""
                    plot(NA, xlim = 0:1, ylim = 0:1, bty = "n", type = "n",
                        xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
                    text(0.5, 0.5,
                        paste(sep = "\n",
                            "Partial residual plots require",
                            "at least one numeric expanatory variable",
                            cex = 2
                        )
                    )
                }
                ## Set options for the dropdown
                blockHandlers(numVarList)
                pvar <- svalue(numVarList, index = FALSE)
                numVarList$set_items(numvars)
                svalue(numVarList, index = TRUE) <<-
                    if (length(pvar) == 1 && pvar %in% numvars)
                        which(numvars == pvar) else 1
                unblockHandlers(numVarList)

                visible(numVarList) <<- length(numvars) > 1
                if (svalue(numVarList) != "") {
                    iNZightRegression::partialResPlot(fit,
                        svalue(numVarList, index = FALSE),
                        showBootstraps = svalue(showBoots),
                        env = e
                    )
                    fmla <- sprintf(
                        "partialResPlot(%s, varname = %s, showBootraps = %s)",
                        fitn,
                        svalue(numVarList, index = FALSE),
                        ifelse(svalue(showBoots), "TRUE", "FALSE")
                    )
                }
            } else if (plottype == 9) {
                catvars <- factorVars()
                if (length(catvars) == 0) {
                    catvars <- ""
                    plot(NA, xlim = 0:1, ylim = 0:1, bty = "n", type = "n",
                        xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
                    text(0.5, 0.5,
                        paste(sep = "n",
                            "Comparison plots require",
                            "at least one categorical variable",
                            cex = 2
                        )
                    )
                }
                blockHandlers(catVarList)
                cvar <- svalue(catVarList, index = FALSE)
                catVarList$set_items(catvars)
                svalue(catVarList, index = TRUE) <<-
                    if (length(cvar) == 1 && cvar %in% catvars)
                        which(catvars == cvar) else 1
                unblockHandlers(catVarList)

                visible(compMatrix) <<- length(catvars) > 1 || catvars != ""
                visible(catVarList) <<- length(catvars) > 1
                if (svalue(catVarList) != "") {
                    plot(
                        suppressWarnings(iNZightMR::moecalc(fit,
                            svalue(catVarList, index = FALSE)
                        ))
                    )
                    fmla <- sprintf(
                        "plot(iNZightMR::moecalc(%s, factorname = %s))",
                        fitn,
                        svalue(catVarList, index = FALSE)
                    )
                }
            } else if (plottype == 10) {
                ## TODO: fix the below assignment as it wont be allowed by CRAN
                assign("dataset", getdata(), envir = .GlobalEnv)
                inzplot(fit, which = "marginal")
                rm(dataset, envir = .GlobalEnv)
                fmla <- sprintf("inzplot(%s, which = \"marginal\")", fitn)
            } else {
                plot(NA, xlim = 0:1, ylim = 0:1, bty = "n", type = "n",
                    xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
                text(0.5, 0.5, "No Model", cex = 2)
            }

            if (savecode && length(fmla) == 1) {
                if (mcall != "") fmla <- c(mcall, fmla)
                GUI$rhistory$add(fmla, keep = TRUE)
                if (!is.null(codehistory)) {
                    svalue(codehistory) <<- ""
                    sapply(GUI$rhistory$get(), insert,
                        obj = codehistory,
                        font.attr = list(family = "monospace")
                    )
                }
            }
            invisible(NULL)
        }, # updatePlot()
        addInstructions = function(where) {
            insert(where,
                "\nWelcome to the iNZight Model Fitting Module!\n",
                font.attr = list(weight = "bold", size = 12)
            )

            ## Getting around
            sapply(
                list(readLines(
                    system.file(
                        file.path("instructions",
                            sprintf("model_fitting_%s.txt",
                                ifelse(GUI$popOut, "popout", "integrated")
                            )
                        ),
                        package = "iNZightModules"
                    )
                )),
                insert,
                obj = where
            )

            sapply(
                list(readLines(
                    system.file(file.path("instructions",
                        "model_fitting.txt"),
                        package = "iNZightModules")
                )),
                insert,
                obj = where
            )

            # Now scroll to top
            where$widget$scrollToIter(
                where$buffer$getStartIter()$iter,
                within.margin = 0
            )
        }, # addInstructions()
        watchData = function() {
            ## append some history stuff too ...
            GUI$rhistory$add(
                c(
                    "",
                    "## --- Model Fitting --- ##",
                    "## NOTE: to add code, SAVE the model (under Managed Saved Models)",
                    "##       and click the R icon in the toolbar to save plot code.", "",
                    "library(iNZightRegression)"
                ),
                keep = TRUE,
                tidy = FALSE
            )

            ## watch changes to dataset, then ...
            GUI$getActiveDoc()$addDataObserver(
                function() try({
                    ## 1. update response variable box (-> which should trigger updating everything else)
                    yVars <- names(getdata()[, sapply(getdata(),
                        function(x) is.numeric(x) || length(levels(x)) == 2)]
                    )
                    blockHandlers(responseBox)
                    responseBox$set_items(yVars)
                    unblockHandlers(responseBox)
                    if (response %in% yVars) {
                        svalue(responseBox) <<- response
                    } else {
                        response <<- NULL
                        svalue(responseBox) <<- NULL
                    }

                    ## 2. update code history (but don't show it!)
                    if (!is.null(codehistory)) {
                        svalue(codehistory) <<- ""
                        sapply(GUI$rhistory$get(), insert,
                            obj = codehistory,
                            font.attr = list(family = "monospace")
                        )
                    }
                }, silent = TRUE)
            )
        }, # watchData()
        close = function() {
            ## save fits
            GUI$moduledata$regression <<- list(fits = fits)

            ## clean up tabs ...
            while ("Model Plots" %in% names(GUI$plotWidget$plotNb)) {
                showTab("plot")
                GUI$plotWidget$closePlot()
                GUI$plotWidget$addPlot()
            }
            while ("Model Output" %in% names(GUI$plotWidget$plotNb)) {
                showTab("summary")
                GUI$plotWidget$closePlot()
            }
            while ("Instructions" %in% names(GUI$plotWidget$plotNb)) {
                showTab("instructions")
                GUI$plotWidget$closePlot()
            }

            GUI$rhistory$add(c("", "## End Model Fitting", "SEP"),
                             tidy = FALSE)

            ## delete the module window
            GUI$close_module()
            ## display the default view (data, variable, etc.)
            GUI$plotToolbar$restore()
            GUI$menuBarWidget$defaultMenu()
            GUI$updatePlot()

            invisible(TRUE)
        }
    )
)
