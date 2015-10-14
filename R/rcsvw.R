library("rrdf")
library("RCurl")
library("rjson")

setClass(
  Class = "Tabular",
  representation = representation(
    url = "character",
    tables = "list",
    meta = "list")
)


Tabular<-function(url=NA){
  table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE,stringsAsFactors = FALSE)
  if(url.exists(paste(url,"-metadata.json",sep=""))){
    metadata<-fromJSON(getURL(paste(url,"-metadata.json",sep=""),.opts=curlOptions(followlocation=TRUE)))
    colnames(table)<-unlist(lapply(metadata$tableSchema$columns,FUN=function(x){x$name}))
    m<-gregexpr("\\{[^:]+\\}",metadata$tableSchema$aboutUrl)
    id<-gsub("\\{|\\}",'',regmatches(metadata$tableSchema$aboutUrl, m))
    table<-lapply(rownames(table),row2json,url,table)
    #row[["describes"]][,"@id"]<-sapply(as.vector(row$describes[[id]]),function(x) paste(url,gsub(paste("\\{",id,"\\}",sep=""),x,s),sep=""))
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    table<-lapply(rownames(table),row2json,url,table)
    meta<-list()
  }
  new("Tabular",url=url,tables=table,meta=meta)
}

clean <-function(y){
  if(!is.null(names(y)) && names(y)=="@id")
    y$"@id"
  else if(!is.null(names(y)) && "@value" %in% names(y)){
    y$"@value"
  }else if(class(y) == "list")
    lapply(y,clean)
  else
    y
}

init<-function(){
  store <<- new.rdf(FALSE)
  add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
  add.prefix(store,prefix="rdf",namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  add.prefix(store,prefix="xsd",namespace="http://www.w3.org/2001/XMLSchema#")
  csvw_describes <<- create.property(store,"http://www.w3.org/ns/csvw#describes")
  csvw_Table <<- create.resource(store,"http://www.w3.org/ns/csvw#Table")
  csvw_tablegroup <<- create.resource(store,"http://www.w3.org/ns/csvw#TableGroup")
  rdf_type <<- create.property(store,"http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  csvw_Row <<- create.resource(store,"http://www.w3.org/ns/csvw#Row")
  csvw_rownum <<-create.property(store,"http://www.w3.org/ns/csvw#rownum")
  csvw_url<<-create.property(store,"http://www.w3.org/ns/csvw#url")
  csvw_table <<- create.property(store,"http://www.w3.org/ns/csvw#table")
  csvw_row <<- create.property(store,"http://www.w3.org/ns/csvw#row")
}
csv2json<-function(url){
  tb<-Tabular(url)
  if(length(tb@meta)>0){
    toJSON(list(tables=list(list(url=tb@url,row=tb@tables,tb@meta))))
  }else{
    toJSON(list(tables=list(list(url=tb@url,row=tb@tables))))
  }
}

row2json<-function(index,url,data){
  row = lapply(data[index,][,!is.na(data[index,])],as.character)
  list(url=paste(url,"#row=",strtoi(index)+1,sep=""),rownum=strtoi(index),describes=list(as.list(data.frame(row,check.names=F))))
}

csv2rdf<-function(url,output="text"){
  init()
  data <- read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE)
  url <- tail(unlist(strsplit(url,"/")),n=1)
  add.prefix(store,prefix="",namespace=paste(url,"#",sep=""))
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
    save.rdf(store,fn,format="TURTLE")
    print (paste(fn,"saved"))
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
  lapply(seq(1,ncol(data)),rowdescribesrdf,data,index,desc,url)
  add.triple(store,rowrdf,csvw_rownum,index)
  add.triple(store,rowrdf,csvw_url,paste(url,"#row=",strtoi(index)+1,sep=""))
  add.triple(store,tb,csvw_row,rowrdf)
}

rowdescribesrdf <- function(i,data,index,desc,url){
  if(!is.null(data[index,i]) & !is.na(data[index,i])>0){
    p<-create.property(store,paste(url,"#",colnames(data)[i],sep=""));
    add.triple(store,desc,p,as.character(data[index,i]))
  }
}
