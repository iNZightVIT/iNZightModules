context("Model Fitting Module")

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
})


mod$close()

## popout mode
ui$preferences$popout <- TRUE
ui$savePreferences()
ui$close()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), silent = TRUE))

mod <- iNZightRegMod$new(ui)
test_that("Reopening output window in popout mode", {
    expect_is(mod$outputWin, "GWindow")
    gWidgets2::dispose(mod$outputWin)
    expect_null(mod$outputWin)
    mod$showOutput()
    expect_is(mod$outputWin, "GWindow")
})

ui$preferences <- initialPrefs
ui$savePreferences()
ui$close()

