context("Model Fitting Module")
skip("skipping")

require(iNZight)
ui <- iNZGUI$new()
ui$initializeGui()
initialPrefs <- ui$preferences

ui$preferences$popout <- FALSE
ui$savePreferences()
ui$close()

ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), silent = TRUE))

test_that("Module opens (and closes)", {
    mod <- iNZightRegMod$new(ui)
    expect_equal(svalue(ui$moduleWindow$header$children[[1]]), "Model Fitting")
    expect_equal(names(ui$plotWidget$plotNb),
        c("Model Output", "Model Plots", "Instructions")
    )

    expect_true(mod$close())
    expect_equal(names(ui$plotWidget$plotNb), "plot")
})

mod <- iNZightRegMod$new(ui)
test_that("Welcome text loads", {
    expect_match(
        svalue(ui$plotWidget$plotNb$children[[3]]),
        "Welcome to the iNZight Model Fitting Module!"
    )
})

test_that("Valid model options are displayed", {
    # continuous
    svalue(mod$responseBox) <- "height"
    expect_equal(mod$responseType, 1)

    mod$variables <- c("age", "year")
    mod$setExplVars()

    # binary
    svalue(mod$responseBox) <- "gender"
    expect_equal(mod$responseType, 2)
    expect_equal(svalue(mod$responseFamilyBox), "Logistic")
})

# require(iNZight)
# ui <- iNZGUI$new()
# ui$initializeGui(census.at.school.500)
# mod <- iNZightRegMod$new(ui)
test_that("Models can be saved, restored, and compared", {
    svalue(mod$responseBox) <- "height"
    mod$variables <- c("gender")
    mod$setExplVars()
    mod$updateModel(save = TRUE)
    expect_equal(svalue(mod$modelList), "Model 1")
    mod$variables <- c("gender", "armspan")
    mod$setExplVars()
    svalue(mod$modelList, TRUE) <- 1
    mod$updateModel(save = TRUE)
    expect_equal(svalue(mod$modelList), "Model 2")

    expect_silent(mod$compBtn$invoke_change_handler())
})


# require(iNZight)
# ui$close(); load_all()
# ui <- iNZGUI$new()
# ui$initializeGui(census.at.school.500)
# mod <- iNZightRegMod$new(ui)
test_that("Factor comparisons display on button press", {
    svalue(mod$responseBox) <- "height"
    mod$variables <- c("gender")
    mod$setExplVars()
    mod$updateModel(save = TRUE)

    svalue(mod$plotTypeList) <- "Factor Comparisons"
    expect_silent(mod$compMatrix$invoke_change_handler())
})


test_that("Model can be fit without intercept", {
    svalue(mod$responseBox) <- "height"
    mod$variables <- c("armspan")
    mod$setExplVars()
    expect_silent(svalue(mod$intercept) <- FALSE)
    expect_equal(
        capture.output(mod$fit$call),
        "lm(formula = height ~ armspan - 1, data = dataset)"
    )
})

mod$close()
ui$close()


test_that("Two columns works fine", {
    df <- data.frame(x = rnorm(100), y = rnorm(100))
    ui$initializeGui(df)
    on.exit(try(ui$close()))
    mod <- iNZightRegMod$new(ui)
    on.exit(try(mod$close()), add = TRUE, after = FALSE)

    expect_silent(mod$responseBox$set_value("x"))
    mod$setExplVars()
    expect_equal(mod$contVarBox$get_items(), "y")
})

try(mod$close())



## popout mode
# ui$preferences$popout <- TRUE
# ui$savePreferences()
# ui$close()
# ui$initializeGui(census.at.school.500)
# on.exit(try(ui$close(), silent = TRUE))

# mod <- iNZightRegMod$new(ui)
# test_that("Reopening output window in popout mode", {
#     expect_is(mod$outputWin, "GWindow")
#     gWidgets2::dispose(mod$outputWin)
#     expect_null(mod$outputWin)
#     mod$showOutput()
#     expect_is(mod$outputWin, "GWindow")
# })

# ui$preferences <- initialPrefs
# ui$savePreferences()
# ui$close()
