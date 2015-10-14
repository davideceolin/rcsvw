library("rrdf")
library("RCurl")
library("rjson")

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
  result <- read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE,stringsAsFactors = TRUE)
  result <- lapply(rownames(result),row2json,url,result)
  res <- list(tables=list(list(url=url,row=result)))
  toJSON(res)
}

row2json<-function(index,url,data){
  row = lapply(data[index,][,!is.na(data[index,])],as.character)
  print(row)
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

getMetadata<-function(url){
  metadata<-fromJSON(getURL(paste(url,"-metadata.json")))
}

setClass (" tabularModel ",
          representation ( tabular ="data.frame")# ,
          #prototype ( size = integer (2) ,
          #            cellres =c (1 ,1) ,
          #            bbox = numeric (4))
          )

tabularModel<-function(url=NA_character_){
  new("tabularModel",url=url)
}

setClass(
  Class = "Tabular",
  representation = representation(
    url = "character",
    row = "data.frame",
    meta = "list"
  )
)

Tabular<-function(url=NA,row=NA,meta=NA){
  row<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=TRUE)),check.names=FALSE)
  if(url.exists(paste(url,"-metadata.json",sep=""))){
    metadata<-fromJSON(getURL(paste(url,"-metadata.json",sep=""),.opts=curlOptions(followlocation=TRUE)))
    colnames(row)<-unlist(lapply(metadata$tableSchema$columns,FUN=function(x){x$name}))
    m<-gregexpr("\\{[^:]+\\}",metadata$tableSchema$aboutUrl)
    id<-gsub("\\{|\\}",'',regmatches(metadata$tableSchema$aboutUrl, m))
    row[,"url"]<-sapply(as.vector(row[[id]]),function(x) paste(url,gsub(paste("\\{",id,"\\}",sep=""),x,s),sep=""))
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    url <- tail(unlist(strsplit(url,"/")),n=1)
    row[,"url"]<-lapply(seq(1,nrow(row)),function(x)paste(url,"#row=",strtoi(x)+1,sep=""))
  }
  new("Tabular",url=url,row=row,meta=meta)
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


#csv2rdf("http://www.w3.org/2013/csvw/tests/test001.csv",output = "file")
#getrownum = function(){return(get("rownum",thisEnv))},

#setrownum = function(value)
#{
#  return(assign("rownum",value,thisEnv))
#},
#getaboutUrl = function()
#{
#  return(get("aboutUrl",thisEnv))
#},
#setaboutUrl = function(value)
#{
#  return(assign("aboutUrl",value,thisEnv))
#}
