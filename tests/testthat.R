library(testthat)
library("rcsvw")
library("rrdf")

 store <<- new.rdf(FALSE)
 add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
 add.triple(store,"http://www.w3.org/ns/csvw#describes","http://www.w3.org/ns/csvw#describes","http://www.w3.org/ns/csvw#describes")
 save.rdf(store,"test1111.ttl",format="TURTLE")

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

test_that("test013json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv","http://www.w3.org/2013/csvw/tests/test013-user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test013.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test013rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv","http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test013.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test014json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"'))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test014/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test014rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"')
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test014/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test015json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test015/user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test015/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test015rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv", metadata="http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test015/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test016json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"'))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test016/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test016rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv", link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"')
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test016/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test017json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test017/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test017rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test017/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test018json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test018/user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test018/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test018rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test018/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})

test_that("test023json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test023-user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test023.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

test_that("test023rdf", {
  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test023.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
  expect_equal(s1,s2)
})
 
test_that("test027json", {
   s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test027-user-metadata.json",minimal=T))
   s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test027.json",.opts=curlOptions(followlocation=TRUE)))             
   expect_equal(s1,s2)
})

test_that("test027rdf", {
 s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test027-user-metadata.json",minimal=T)
 s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test027.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
 expect_equal(s1,s2)
})
 
test_that("test028json", {
 s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/countries.csv"))
 s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test028.json",.opts=curlOptions(followlocation=TRUE)))             
 expect_equal(s1,s2)
})
 
 test_that("test028rdf", {
   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/countries.csv")
   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test028.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
   expect_equal(s1,s2)
 }) 

 test_that("test029json", {
   s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/countries.csv",minimal=T))
   s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test029.json",.opts=curlOptions(followlocation=TRUE)))             
   expect_equal(s1,s2)
 })
 
 test_that("test029rdf", {
   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/countries.csv",minimal=T)
   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test029.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
   expect_equal(s1,s2)
 }) 
