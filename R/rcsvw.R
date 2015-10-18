library("rrdf")
library("RCurl")
library("rjson")
library("httr")

setClass(
  Class = "Tabular",
  representation = representation(
    url = "character",
    tables = "list",
    meta = "list")
)

Tabular<-function(url=NA){
  table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F)
  metadata_file<-NULL
  http_header<-GET(url)
  if(url.exists(paste(url,"-metadata.json",sep=""))){
    metadata_file<-paste(url,"-metadata.json",sep="")
  }else if(url.exists(gsub(tail(unlist(strsplit(url,"/")),n=1),"csv-metadata.json",url))){
    metadata_file<-gsub(tail(unlist(strsplit(url,"/")),n=1),"csv-metadata.json",url)
  }else if("Link" %in% names(http_header)){
    metadata_file<-http_header$Link
  }
  if(!is.null(metadata_file)){
    metadata<-fromJSON(getURL(metadata_file,.opts=curlOptions(followlocation=TRUE)))
    colnames(table)<-unlist(lapply(metadata$tableSchema$columns,FUN=function(x){x$name}))
    n<-colnames(table)
    table<-as.data.frame(lapply(colnames(table),format_column,table,metadata))
    colnames(table)<-n
    if("aboutUrl" %in% names(metadata$tableSchema)){
      ids<-sapply(as.vector(table[[id]]),function(x) paste(gsub(paste("\\{",id,"\\}",sep=""),x,metadata$tableSchema$aboutUrl),sep=""))
      m<-gregexpr("\\{[^:]+\\}",metadata$tableSchema$aboutUrl)
      id<-gsub("\\{|\\}",'',regmatches(metadata$tableSchema$aboutUrl, m))
      table<- cbind("@id"=ids,table)
    }
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    meta<-list()
  }
  new("Tabular",url=url,tables=list(table),meta=meta)
}

clean <-function(y){
  if(length(y)>0){
  if(!is.null(names(y)) && names(y)=="@id")
    y$"@id"
  else if(!is.null(names(y)) && "@value" %in% names(y)){
    y$"@value"
  }else if(class(y) == "list")
    lapply(y,clean)
  else
    y}else
      y
}

format_column<-function(col_name,table,metadata){
  index<-sapply(metadata$tableSchema$columns,function(y){y$name==col_name})
  datatype<-metadata$tableSchema$columns[index][[1]]$datatype
  if(!is.atomic(datatype) && datatype$base=="date"){
    R_format<-gsub("yyyy","Y",datatype$format,perl=TRUE)
    R_format<-gsub("([[:alpha:]])+","%\\1",R_format,perl=TRUE)
    d<-lapply(table[,col_name],function(y) as.Date(y,format=R_format))
    R_format<-gsub("M","m",R_format,perl=TRUE)
    unlist(lapply(table[,col_name],function(y) as.character(as.Date(y,format=R_format))))
  }else
    table[,col_name]
}

init<-function(){
  store <<- new.rdf(FALSE)
  add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
  add.prefix(store,prefix="rdf",namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  add.prefix(store,prefix="xsd",namespace="http://www.w3.org/2001/XMLSchema#")
  add.prefix(store,prefix="dc",namespace="http://purl.org/dc/terms/")
  add.prefix(store,prefix="dcat",namespace="http://www.w3.org/ns/dcat#")
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
  if("@id" %in% colnames(tb@tables[[1]])){tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x) paste(url,x,sep=""))}
  tb1<-lapply(rownames(tb@tables[[1]]),row2json,tb@url,tb@tables[[1]])
  toJSON(list(tables=list(c(list(url=tb@url),tb@meta,list(row=tb1)))))
}

row2json<-function(index,url,data){
  row = lapply(data[index,][,!is.na(data[index,])],as.character)
  list(url=paste(url,"#row=",strtoi(index)+1,sep=""),rownum=strtoi(index),describes=list(as.list(data.frame(row,check.names=F))))
}

csv2rdf<-function(url,output="store"){
  init()
  tb<-Tabular(url)
  tb@url <- tail(unlist(strsplit(url,"/")),n=1)
  if("@id" %in% colnames(tb@tables[[1]])){tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x) paste(url,x,sep=""))}
  #tb@url <- paste(tb@url,"#",sep="")
  add.prefix(store,prefix="",namespace=paste(tb@url,"#",sep=""))
  tg1 <- create.blankNode(store)
  add.triple(store,tg1,rdf_type,csvw_tablegroup)
  tb1 <- create.blankNode(store)
  add.triple(store,tg1,csvw_table,tb1)
  add.triple(store,tb1,rdf_type,csvw_Table)
  lapply(rownames(tb@tables[[1]]),row2rdf,tb@url,tb@tables[[1]],tb1)
  add.triple(store,tb1,csvw_url,tb@url)
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
  if("@id" %in% colnames(row)){
    desc <- create.resource(store,as.character(row[1]))
  }else{
    desc <- create.blankNode(store)
  }
  rowrdf <- create.blankNode(store)
  add.triple(store,rowrdf,rdf_type,csvw_Row)
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