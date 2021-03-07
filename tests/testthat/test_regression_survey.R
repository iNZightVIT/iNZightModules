context("Survey modelling")
skip()

chis <- iNZightTools::smart_read("chis.csv")

data(api, package = 'survey')

require(iNZight)
ui <- iNZGUI$new()
ui$initializeGui(apiclus1)
on.exit(try(ui$close(), silent = TRUE))

ui$iNZDocuments[[ui$activeDoc]]$getModel()$setDesign(
    list(
        ids = "dnum",
        weights = "pw",
        fpc = "fpc",
        type = "survey"
    ),
    gui = ui
)


test_that("Module opens (and closes)", {
    mod <- iNZightRegMod$new(ui)
    expect_equal(svalue(ui$moduleWindow$header$children[[1]]), "Model Fitting")
    expect_equal(names(ui$plotWidget$plotNb),
        c("Model Output", "Model Plots", "Instructions")
    )

    expect_match(
        svalue(mod$mainGrp$children[[1]]),
        "Using complex survey design"
    )

    expect_true(mod$close())
    expect_equal(names(ui$plotWidget$plotNb), "plot")
})

test_that("Basic survey models work", {
    mod <- iNZightRegMod$new(ui)
    expect_silent(svalue(mod$responseBox) <- "api00")
    expect_true(mod$close())
})

test_that("Model comparison works", {
    mod <- iNZightRegMod$new(ui)
    svalue(mod$responseBox) <- "api00"
    mod$variables <- c("api99")
    mod$setExplVars()
    mod$updateModel(save = TRUE)
    expect_equal(svalue(mod$modelList), "Model 1")
    mod$variables <- c("api99", "enroll")
    mod$setExplVars()
    svalue(mod$modelList, TRUE) <- 1
    mod$updateModel(save = TRUE)
    expect_equal(svalue(mod$modelList), "Model 2")

    expect_silent(mod$compBtn$invoke_change_handler())
    expect_true(mod$close())
})

ui$iNZDocuments[[ui$activeDoc]]$getModel()$setDesign(
    list(
        ids = "dnum",
        weights = "pw",
        fpc = "fpc",
        calibrate = list(stype = c(E = 4421, H = 755, M = 1018)),
        type = "survey"
    ),
    gui = ui
)

test_that("Post-stratified models work", {
    mod <- iNZightRegMod$new(ui)
    expect_silent(svalue(mod$responseBox) <- "api00")
    expect_true(mod$close())
})


# chis <- iNZightTools::smart_read("tests/testthat/chis.csv")
ui$close()
ui <- iNZGUI$new()
ui$initializeGui(chis)

ui$iNZDocuments[[ui$activeDoc]]$getModel()$setDesign(
    list(
        weights = "rakedw0",
        repweights = paste0("rakedw", 1:80),
        reptype = "other",
        scale = 1,
        rscales = 1,
        type = "replicate"
    ),
    gui = ui
)

test_that("Replicate design models work", {
    mod <- iNZightRegMod$new(ui)
    expect_silent(svalue(mod$responseBox) <- "bmi_p")
    expect_true(mod$close())
})
