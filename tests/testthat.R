library(testthat)
library(rcsvw)
library("RCurl")
library(rrdf)

context("test001")
#test_check("rcsvw")
test_that("test001", {
  s1 = expect_equal(csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output="store"))
  s2 = load.rdf("http://www.w3.org/2013/csvw/tests/test001.ttl",format="TURTLE")              
  expect_equal(s1,s2)
  
})