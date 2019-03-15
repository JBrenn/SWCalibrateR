app <- ShinyDriver$new("../")
app$snapshotInit("dignosticsRLM")

app$setInputs(robust = TRUE)
app$snapshot()
