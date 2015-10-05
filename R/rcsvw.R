library("rrdf")
library("RCurl")
library("rjson")

store <- new.rdf()
add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
add.prefix(store,prefix="rdf",namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#")
add.prefix(store,prefix="xsd",namespace="http://www.w3.org/2001/XMLSchema#")
csvw_describes <- create.property(store,"http://www.w3.org/ns/csvw#describes")
csvw_Table <- create.resource(store,"http://www.w3.org/ns/csvw#Table")
csvw_tablegroup <- create.resource(store,"http://www.w3.org/ns/csvw#TableGroup")
rdf_type <- create.property(store,"http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
csvw_Row <- create.resource(store,"http://www.w3.org/ns/csvw#Row")
csvw_rownum <-create.property(store,"http://www.w3.org/ns/csvw#rownum")
csvw_url<-create.property(store,"http://www.w3.org/ns/csvw#url")
csvw_table <- create.property(store,"http://www.w3.org/ns/csvw#table")
csvw_row <- create.property(store,"http://www.w3.org/ns/csvw#row")

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

csv2rdf<-function(url,output="text"){
  add.prefix(store,prefix="",namespace=paste(url,"#",sep=""))
  data <- read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE,stringsAsFactors = FALSE)
  url <- tail(unlist(strsplit(url,"/")),n=1)
  tg1 <- create.blankNode(store)
  add.triple(store,tg1,rdf_type,csvw_tablegroup)
  tb1 <- create.blankNode(store)
  add.triple(store,tb1,rdf_type,csvw_Table)
  add.triple(store,tg1,csvw_table,tb1)
  lapply(rownames(data),row2rdf,url,data,tb1)
  add.triple(store,tb1,csvw_url,url)
  if(output=="text"){
    dump.rdf(store)
  }else if(output=="file"){
    fn<-strsplit(url,".",fixed=TRUE)[[1]][1]
    fn<-paste(fn,".ttl",sep="")
    save.rdf(store,x,format="TURTLE")
    print ("file saved")
  }else if(output=="store"){
    store
  }
  
}

row2rdf<-function(index,url,data,tb){
  row = data[index,][,!is.na(data[index,])]
  rowrdf <- create.blankNode(store)
  add.triple(store,rowrdf,rdf_type,csvw_Row)
  desc <- create.blankNode(store)
  add.triple(store,rowrdf,csvw_describes,desc)
  lapply(seq(1,ncol(data)),rowdescribes,data,index,desc)
  add.triple(store,rowrdf,csvw_rownum,index)
  add.triple(store,rowrdf,csvw_url,paste(url,"#row=",strtoi(index)+1,sep=""))
  add.triple(store,tb,csvw_row,rowrdf)
}

rowdescribes <- function(i,data,index,desc){
  p<-create.property(store,paste(url,"#",colnames(data)[i],sep=""));
  add.triple(store,desc,p,as.character(data[index,i]))
}

csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output = "file")
