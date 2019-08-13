context("Survey modelling")

chis <- iNZightTools::smart_read("chis.csv")

require(iNZight)
data(api, package = 'survey')


ui <- iNZGUI$new()
ui$initializeGui(apiclus1)
on.exit(try(ui$close(), silent = TRUE))

ui$iNZDocuments[[ui$activeDoc]]$getModel()$setDesign(
    clus1 = "dnum", wt = "pw", fpc = "fpc",
    type = "survey",
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
})


ui$iNZDocuments[[ui$activeDoc]]$getModel()$setDesign(
    clus1 = "dnum", wt = "pw", fpc = "fpc",
    poststrat = list(stype = 
        data.frame(
            stype = c("E", "H", "M"),
            Freq = c(4421, 755, 1018)
        )
    ),
    type = "survey",
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
    wt = "rakedw0", repweights = paste0("rakedw", 1:80),
    reptype = "other", scale = 1, rscales = 1,
    type = "replicate",
    gui = ui
)

test_that("Replicate design models work", {
    mod <- iNZightRegMod$new(ui)
    expect_silent(svalue(mod$responseBox) <- "bmi_p")
})
