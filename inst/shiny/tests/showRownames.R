app <- ShinyDriver$new("../")
app$snapshotInit("showRownames")

app$setInputs(Rownames = TRUE)
app$snapshot()
