context("3D Plotting Module")

require(iNZight)
# try(ui$close(), T)
ui <- iNZGUI$new()
ui$initializeGui(iris)
on.exit(try(ui$close(), silent = TRUE))

skip_if_not_installed("car")

test_that("3D module loads", {
    # expect_silent({
        ign <- gwindow("...", visible = FALSE)
        tag(ign, "dataSet") <- ui$getActiveData()
        e <- list(obj = ign)
        e$win <- ui$win
        p <- plot3D(e)
    # })
    svalue(p$children[[1]]$children[[1]]$children[[1]]$children[[2]]) <- "Sepal.Length"
    svalue(p$children[[1]]$children[[1]]$children[[1]]$children[[5]]) <- "Sepal.Width"
    svalue(p$children[[1]]$children[[1]]$children[[1]]$children[[8]]) <- "Petal.Length"
    expect_silent(
        p$children[[1]]$children[[2]]$children[[2]]$children[[2]]$children[[1]]$invoke_change_handler()
    )
    rgl::rgl.close()
    dispose(p)
})
