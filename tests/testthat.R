library(testthat)
library("rcsvw")
library("rrdf")

init_test<-function(){
  store <<- new.rdf(FALSE)
  me<-create.resource(store,"http://trustingwebdata.org/foaf#me")
  rcsvw<-create.resource(store,"https://github.com/davideceolin/rcsvw")
  add.prefix(store,prefix="doap",namespace="http://usefulinc.com/ns/doap#")
  add.prefix(store,prefix="earl",namespace = "http://www.w3.org/ns/earl#")
  add.prefix(store,prefix="dc",namespace="http://purl.org/dc/terms/")
  add.prefix(store,prefix="foaf",namespace="http://xmlns.com/foaf/0.1/")
  rdf_type <<- create.property(store,"http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  add.triple(store,rcsvw,rdf_type,create.resource(store,"doap:Project"))
  add.triple(store,rcsvw,rdf_type,create.resource(store,"earl:TestSubject"))
  add.triple(store,rcsvw,rdf_type,create.resource(store,"earl:Software"))
  add.triple(store,rcsvw,create.property(store,"doap:name"),"rcsvw")
  add.triple(store,rcsvw,create.property(store,"doap:homepage"),"rcsvw")
  add.triple(store,rcsvw,create.property(store,"doap:license"),"http://creativecommons.org/licenses/publicdomain/")
  add.triple(store,rcsvw,create.property(store,"doap:description"),"rcsvw processes tabular data with metadata creating RDF or JSON output.")
  add.triple(store,rcsvw,create.property(store,"doap:programming-language"),"R")
  add.triple(store,rcsvw,create.property(store,"doap:developer"),me)
  add.triple(store,rcsvw,create.property(store,"dc:title"),"rcsvw")
  add.triple(store,rcsvw,create.property(store,"dc:date"),as.character(Sys.Date()))
  me<-create.resource(store,"http://trustingwebdata.org/foaf#me")
  add.triple(store,rcsvw,create.property(store,"dc:creator"),me)
  add.triple(store,me,rdf_type,create.resource(store,"earl:Assertor"))
  add.triple(store,me,rdf_type,create.resource(store,"foaf:Person"))
  add.triple(store,me,create.property(store,"foaf:name"),"\"Davide Ceolin\"")
  add.triple(store,me,create.property(store,"foaf:title"),"Implementor")
  add.triple(store,me,create.property(store,"foaf:homepage"),create.resource(store,"http://trustingwebdata.org/davide.html"))
  save.rdf(store,"/Users/dceolin/Desktop/test.ttl",format="TURTLE")
} 

record_test<-function(test,file){
  store = load.rdf(file,"TURTLE")
  t<-format(Sys.time(), "%Y-%m-%dT%X^^xsd:date")
  rdf_type<-create.property(store,"http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  TestResult<-create.resource(store,"http://www.w3.org/ns/earl#TestResult")
  ass<-create.blankNode(store)
   add.triple(store,ass,rdf_type,create.resource(store,"http://www.w3.org/ns/earl#Assertion"))
   add.triple(store,ass,create.property(store,"http://www.w3.org/ns/earl#assertedBy"),create.resource(store,"http://trustingwebdata.org/foaf#me"))
   add.triple(store,ass,create.property(store,"http://www.w3.org/ns/earl#subject"),create.resource(store,"https://github.com/davideceolin/rcsvw"))
   add.triple(store,ass,create.property(store,"http://www.w3.org/ns/earl#test"),create.resource(store,paste("http://www.w3.org/2013/csvw/tests/manifest-rdf#",test,sep="")))
   res<-create.blankNode(store)  
   add.triple(store,ass,create.property(store,"http://www.w3.org/ns/earl#result"),res)
   add.triple(store,ass,create.property(store,"http://www.w3.org/ns/earl#mode"),create.resource(store,"http://www.w3.org/ns/earl#automatic"))
   add.triple(store,res,rdf_type,TestResult)
   add.triple(store,res,create.property(store,"http://www.w3.org/ns/earl#outcome"),create.resource(store,"http://www.w3.org/ns/earl#passed"))
   add.triple(store,res,create.property(store,"http://purl.org/dc/terms/date"),t)
   save.rdf(store,"/Users/dceolin/Desktop/test.ttl",format="TURTLE")
}
#  
# test_that("test001rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test001.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
#   init_test()
# })
# 
test_that("test001json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test001.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test001.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
  #record_test("test001","/Users/dceolin/Desktop/test.ttl")
  #record_test("validation#test001","/Users/dceolin/Desktop/test.ttl")
})
# 
# test_that("test005rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test005.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test005.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })
# 
test_that("test005json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test005.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test005.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
  #record_test("test005","/Users/dceolin/Desktop/test.ttl")
  #record_test("validation#test005","/Users/dceolin/Desktop/test.ttl")
})
# 
# 
# test_that("test006rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test006.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test006.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })
# 
test_that("test006json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test006.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test006.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
#   record_test("test006","/Users/dceolin/Desktop/test.ttl")
#   record_test("validation#test006","/Users/dceolin/Desktop/test.ttl")
})


# test_that("test007rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test007.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test007.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })
# 
test_that("test007json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test007.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test007.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
#   record_test("test007","/Users/dceolin/Desktop/test.ttl")
})


# test_that("test008rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test008.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test008.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })
# 
test_that("test008json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test008.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test008.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
#   record_test("test008","/Users/dceolin/Desktop/test.ttl")
})


# test_that("test009rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test009.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test009.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })
# 
test_that("test009json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test009.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test009.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
#   record_test("test009","/Users/dceolin/Desktop/test.ttl")
})

# test_that("test010rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test010.csv",output = "store")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test010.ttl",.opts=curlOptions(followlocation=TRUE)) ,format="TURTLE")             
#   expect_equal(s1,s2)
# })

test_that("test010json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test010.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test010.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
#   record_test("test010","/Users/dceolin/Desktop/test.ttl")
})

test_that("test011json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test011/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test011rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test011/tree-ops.csv")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test011/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test011","/Users/dceolin/Desktop/test.ttl")
# })

test_that("test012json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test012/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test012rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test012/tree-ops.csv")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test012/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test012","/Users/dceolin/Desktop/test.ttl")
# })

test_that("test013json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv","http://www.w3.org/2013/csvw/tests/test013-user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test013.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test013rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv","http://www.w3.org/2013/csvw/tests/test013-user-metadata.json")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test013.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test013","/Users/dceolin/Desktop/test.ttl")
# })

test_that("test014json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"'))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test014/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test014rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test014/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"')
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test014/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test014","/Users/dceolin/Desktop/test.ttl")
# })
# 
test_that("test015json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test015/user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test015/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test015rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test015/tree-ops.csv", metadata="http://www.w3.org/2013/csvw/tests/test015/user-metadata.json")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test015/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test015","/Users/dceolin/Desktop/test.ttl")
# })
# 
test_that("test016json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv",link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"'))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test016/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test016rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test016/tree-ops.csv", link_header = '<linked-metadata.json>; rel="describedby"; type="application/csvm+json"')
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test016/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test016","/Users/dceolin/Desktop/test.ttl")
# })
# 
test_that("test017json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test017/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})

# test_that("test017rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test017/tree-ops.csv")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test017/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test017","/Users/dceolin/Desktop/test.ttl")
# })
# 
test_that("test018json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test018/user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test018/result.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})
# 
# test_that("test018rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/test018/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test018/user-metadata.json")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test018/result.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test018","/Users/dceolin/Desktop/test.ttl")
# })
# 
test_that("test023json", {
  s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test023-user-metadata.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test023.json",.opts=curlOptions(followlocation=TRUE)))           
  expect_equal(s1,s2)
})
# 
# test_that("test023rdf", {
#   s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test023-user-metadata.json")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test023.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test023","/Users/dceolin/Desktop/test.ttl")
# })
#  
test_that("test027json", {
   s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test027-user-metadata.json",minimal=T))
   s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test027.json",.opts=curlOptions(followlocation=TRUE)))             
   expect_equal(s1,s2)
})
# 
# test_that("test027rdf", {
#  s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/tree-ops.csv",metadata="http://www.w3.org/2013/csvw/tests/test027-user-metadata.json",minimal=T)
#  s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test027.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#  expect_equal(s1,s2)
#  record_test("test027","/Users/dceolin/Desktop/test.ttl")
# })
#  
test_that("test028json", {
 s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/countries.csv"))
 s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test028.json",.opts=curlOptions(followlocation=TRUE)))             
 expect_equal(s1,s2)
})
#  
#  test_that("test028rdf", {
#    s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/countries.csv")
#    s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test028.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#    expect_equal(s1,s2)
#    record_test("test028","/Users/dceolin/Desktop/test.ttl")
#  }) 
# 
 test_that("test029json", {
   s1 <- fromJSON(csv2json("http://www.w3.org/2013/csvw/tests/countries.csv",minimal=T))
   s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test029.json",.opts=curlOptions(followlocation=TRUE)))             
   expect_equal(s1,s2)
 })
#  
#  test_that("test029rdf", {
#    s1 <- csv2rdf("http://www.w3.org/2013/csvw/tests/countries.csv",minimal=T)
#    s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test029.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#    expect_equal(s1,s2)
#    record_test("test029","/Users/dceolin/Desktop/test.ttl")
#  }) 
# 
test_that("test030json", {
  s1 <- fromJSON(csv2json(metadata="http://www.w3.org/2013/csvw/tests/countries.json"))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test030.json",.opts=curlOptions(followlocation=TRUE)))             
  expect_equal(s1,s2)
  })
# 
# test_that("test030rdf", {
#   s1 <- csv2rdf(metadata="http://www.w3.org/2013/csvw/tests/countries.json")
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test030.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test030","/Users/dceolin/Desktop/test.ttl")
# }) 
# 
test_that("test031json", {
  s1 <- fromJSON(csv2json(metadata="http://www.w3.org/2013/csvw/tests/countries.json",minimal=T))
  s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test031.json",.opts=curlOptions(followlocation=TRUE)))             
  expect_equal(s1,s2)
})
# 
# test_that("test031rdf", {
#   s1 <- csv2rdf(metadata="http://www.w3.org/2013/csvw/tests/countries.json",minimal=T)
#   s2 <- fromString.rdf(getURL("http://www.w3.org/2013/csvw/tests/test030.ttl",.opts=curlOptions(followlocation=TRUE)),format="TURTLE")           
#   expect_equal(s1,s2)
#   record_test("test031","/Users/dceolin/Desktop/test.ttl")
# }) 

# test_that("test032json", {
#   s1 <- fromJSON(csv2json(metadata="http://www.w3.org/2013/csvw/tests/test032/csv-metadata.json"))
#   s2 <- fromJSON(getURL("http://www.w3.org/2013/csvw/tests/test032/result.json",.opts=curlOptions(followlocation=TRUE)))             
#   expect_equal(s1,s2)
# })
