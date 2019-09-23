DemoModule3 <- setRefClass(
    "DemoModule3",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui, 
                name = "Demo Module 3", # the name to appear in the window
                embedded = TRUE       # if TRUE, module will be embedded in iNZight, otherwise a separate window
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            label <- glabel("This is yet another demo module",
                container = mainGrp)

            cat("Running a newer module\n")
        },
        close = function() {
            cat("Closing module 3\n")

            callSuper()
        }
    )
)
