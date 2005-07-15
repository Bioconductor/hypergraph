require("RUnit", quietly=TRUE) || stop("RUnit not found")
require("SharedItems") || stop("SharedItems not found")

testFilePattern <- ".*_test\.R$"
testFileDirs <- "."

suite <- defineTestSuite(name="SharedItems Unit Tests",
                         dirs=testFileDirs,
                         testFileRegexp=testFilePattern)
result <- runTestSuite(suite)
printTextProtocol(result, showDetails=FALSE)
