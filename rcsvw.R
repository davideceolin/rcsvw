require("rrdf")
require("RCurl")
require("rjson")

x<-getURL("http://www.w3.org/2013/csvw/tests/test001.csv")
y<-read.csv(text=x)

csv2json<-function(url){
  result <- read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE)
  result <- lapply(rownames(result),row2json,url,result)
  res <- list(tables=list(list(url=url,row=result)))
  toJSON(res)
}

row2json<-function(index,url,data){
  row = data[index,][,!is.na(data[index,])]
  list(url=paste(url,"#row=",strtoi(index)+1,sep=""),rownum=strtoi(index),describes=c(row))
}

csv2rdf<-function(url){
  data <- read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE,stringsAsFactors = FALSE)
  url=tail(unlist(strsplit(url,"/")),n=1)
  store=new.rdf()
  add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
  add.prefix(store,prefix="rdf",namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  add.prefix(store,prefix="xsd",namespace="http://www.w3.org/2001/XMLSchema#")
  add.prefix(store,prefix="",namespace=paste(url,"#",sep=""))
  tg = create.blankNode(store)
  tgclass = create.resource(store,"http://www.w3.org/ns/csvw#TableGroup")
  rdftype = create.property(store,"http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  csvwdesc = create.resource(store,"http://www.w3.org/ns/csvw#describes")
  add.triple(store,tg,rdftype,tgclass)
  tbclass = create.resource(store,"http://www.w3.org/ns/csvw#Table")
  tb1 = create.blankNode(store)
  add.triple(store,tb1,rdftype,tbclass)
  add.triple(store,tg,create.property(store,"http://www.w3.org/ns/csvw#table"),tb1)
  rtype = create.resource(store,"http://www.w3.org/ns/csvw#Row")
  pdesc<-create.property(store,"http://www.w3.org/ns/csvw#describes")
  prownum<-create.property(store,"http://www.w3.org/ns/csvw#rownum")
  purl<-create.property(store,"http://www.w3.org/ns/csvw#url")
  lapply(rownames(data),row2csv,url,data,store,rdftype,rtype,pdesc,prownum,purl,tb1)
  add.triple(store,tb1,purl,url)
  save.rdf(store,"prova.xml",format="N3")
}

row2csv<-function(index,url,data,store,rdftype,tr,pdesc,prownum,purl,tb){
  row = data[index,][,!is.na(data[index,])]
  rowrdf <- create.blankNode(store)
  add.triple(store,rowrdf,rdftype,tr)
  desc <- create.blankNode(store)
  add.triple(store,rowrdf,pdesc,desc)
  print(index)
  lapply(seq(1,ncol(data)),function(i){p<-create.property(store,paste(url,"#",colnames(data)[i],sep=""));add.triple(store,desc,p,as.character(data[index,i]))})
  add.triple(store,rowrdf,prownum,index)
  add.triple(store,rowrdf,purl,paste(url,"#row=",strtoi(index)+1,sep=""))
  add.triple(store,tb,create.property(store,"http://www.w3.org/ns/csvw#row"),rowrdf)
}

csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv")
