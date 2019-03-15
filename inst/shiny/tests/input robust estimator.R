app <- ShinyDriver$new("../")
app$snapshotInit("input robust estimator")

app$setInputs(robust = TRUE)
app$snapshot()
