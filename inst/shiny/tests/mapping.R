app <- ShinyDriver$new("../")
app$snapshotInit("mapping")

# Input 'map_groups' was set, but doesn't have an input binding.
app$snapshot()
