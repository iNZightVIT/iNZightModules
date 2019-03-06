context("Model Fitting Module")

require(iNZight)

ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))

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
# test_that("")
