library(testthat)
library(rcsvw)
library(RCurl)
library(rrdf)

context("test001")
#test_check("rcsvw")
test_that("test001", {
  library(rcsvw)
  expect_equal(1,1)
  #s1 = csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output = "store")
  #s2 = fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test001.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  #expect_equal(s1,s2)
  
})

library(lubridate)
test_that("floor_date works for different units", {
  base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
})