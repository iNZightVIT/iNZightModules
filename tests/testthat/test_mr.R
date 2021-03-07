context("Multiple response module")
skip()

require(iNZight)
require(iNZightMR)
# try(ui$close(), T)
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.5000)
on.exit(try(ui$close(), silent = TRUE))

mr <- NULL
test_that("Module loads and displays basic graph", {
    mr <<- iNZightMultiRes$new(ui)
    Sys.sleep(1)
    techv <- grep("^tech", mr$gtab$get_items())
    expect_silent(mr$gtab$set_selected(techv))
    Sys.sleep(5)
})

test_that("Buttons enable/disable appropriately", {
    expect_true(enabled(mr$sumButton))
    expect_true(enabled(mr$comButton))

    svalue(mr$mid$children[[1]]) <- "gender"
    expect_true(enabled(mr$sumButton))
    expect_false(enabled(mr$comButton))

    svalue(mr$mid$children[[2]]) <- "handed"
    expect_false(enabled(mr$sumButton))
    expect_false(enabled(mr$comButton))
})
