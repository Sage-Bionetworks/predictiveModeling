# This test is only run during R CMD check
# It should invoke all unit tests but no integration tests
require("predictiveModeling") || stop("unable to load predictiveModeling package")
predictiveModeling:::.test()
