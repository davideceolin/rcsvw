library(testthat)
library(rcsvw)
require(RCurl)
require(rrdf)

context("test001")
#test_check("rcsvw")
test_that("test001", {
  s1 = csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output="store")
  s2 = fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test001.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
  
})