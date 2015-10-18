library(testthat)
library("rcsvw")
test_that("test001rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test001.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test001json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test001.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test001.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test005rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test005.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test005.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test005json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test005.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test005.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})


test_that("test006rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test006.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test006.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test006json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test006.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test006.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})


test_that("test007rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test007.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test007.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test007json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test007.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test007.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})


test_that("test008rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test008.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test008.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test008json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test008.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test008.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})


test_that("test009rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test009.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test009.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test009json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test009.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test009.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test010rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test010.csv",output = "store")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test010.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
  expect_equal(s1,s2)
})

test_that("test010json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test010.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test010.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test011json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test011/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test011rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test011/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test012json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test012/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test012rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test012/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})
