library("rrdf")
library("RCurl")
library("rjson")
library("httr")
library("stringi")

setClass(
  Class = "Tabular",
  representation = representation(
    url = "character",
    tables = "list",
    meta = "list",
    header = "logical",
    foreignKeys = "list")
)

Tabular<-function(url=NA,metadata_param=NULL,link_header=NULL){
  metadata_file<-NULL
  foreignKeys<-list()
  rownum<-1
  if(!is.null(url)){
    http_header<-GET(url)
  }
  if(!is.null(metadata_param)){
    metadata_file<-metadata_param
  }else if(!is.null(url) && "Link" %in% names(http_header)){
    metadata_file<-http_header$Link
  }else if(!is.null(link_header)){
    loc<-if(gsub(' ','',unlist(strsplit(link_header,';'))[2])=='rel=\"describedby\"') gsub('[<>]','',unlist(strsplit(link_header,';'))[1])
    metadata_file<-gsub(tail(unlist(strsplit(url,"/")),n=1),loc,url)
  }else if(url.exists(paste(url,"-metadata.json",sep=""))){
    metadata_file<-paste(url,"-metadata.json",sep="")
  }else if(url.exists(gsub(tail(unlist(strsplit(url,"/")),n=1),"csv-metadata.json",url))){
    metadata_file<-gsub(tail(unlist(strsplit(url,"/")),n=1),"csv-metadata.json",url)
  }
  if(!is.null(metadata_file)){
    metadata<-fromJSON(getURL(metadata_file,.opts=curlOptions(followlocation=TRUE)))
    if(!is.null(metadata$tables)){
      metadata<-metadata$tables[sapply(metadata$tables,function(x)(x$url==tail(unlist(strsplit(url,"/")),n=1)))][[1]]
    }
    header<-T
    if(!is.null(metadata$dialect$rownum)){
      header<-metadata$dialect$rownum>=1
      row.nums<-seq(rownum,length.out=nrow(table))
    }else if(!is.null(metadata$dialect$header)){
      header<-(metadata$dialect$header=="true")
    }
    table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F,header=header)
    ext_ref<-metadata$tableSchema$columns[sapply(metadata$tableSchema$columns,function(x)!is.null(x$valueUrl))]
    lapply(ext_ref,function(x) table[,x$name]<<-
             sapply(table[,x$name], function(y){gsub("[{}]",'',stri_replace_last_fixed(x$valueUrl,x$name,y,vectorize_all = F)[1])}))
    if(!is.null(metadata$dialect$rownum) && metadata$dialect$rownum!=0){
      rownames(table)<-seq(metadata$dialect$rownum,length.out=nrow(table))
    }
    if("aboutUrl" %in% names(metadata$tableSchema)){
      m<-gregexpr("\\{[^:]+\\}",metadata$tableSchema$aboutUrl)
      id<-gsub("\\{|\\}",'',regmatches(metadata$tableSchema$aboutUrl, m))
      clean_id<-gsub("[#_]",'',id)
      ids<-sapply(as.vector(table[[clean_id]]),
                  function(x){paste(gsub(paste("\\{",id,"\\}",sep=""),paste(gsub(clean_id,'',id),x,sep=""),metadata$tableSchema$aboutUrl),sep="")}
                  )
      table<- cbind("@id"=ids,table)
    }
    if(!is.null(metadata$tableSchema$columns)){
      n<-unlist(lapply(metadata$tableSchema$columns,FUN=function(x){
        if(!is.null(x$propertyUrl)){
          s<-stri_replace_last_fixed(x$propertyUrl,names(x),x,vectorize_all = F)[1]
          check_ns(s)
        }else if(!is.null(metadata$tableSchema$propertyUrl)){
          s<-stri_replace_last_fixed(metadata$tableSchema$propertyUrl,names(x),x,vectorize_all = F)[1]
          check_ns(s)
        }else{
         x$name
        }}))
      n<-gsub("[{}_]",'',n)
      id_tag<-F
      if(colnames(table)[1]=="@id"){
        id<-table[,1]
        table<-table[,-1]
        id_tag<-T
      }
      table<-as.data.frame(lapply(seq(1,ncol(table)),format_column,table,metadata))
      colnames(table)<-n
      if(id_tag){
        table<-cbind("@id"=id,table)
      }
    }else{
      colnames(table)<-sapply(seq(1,ncol(table)),function(x)paste("_col.",x,sep=""))
    }
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    header<-T
    table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F)
    meta<-list()
  }
  new("Tabular",url=url,tables=list(table),meta=meta,header=header,foreignKeys=foreignKeys)
}

check_ns<-function(x){
  ns<-fromJSON(getURL("http://www.w3.org/2013/json-ld-context/rdfa11"))
  n<-which(lapply(ns$'@context',function(y) {length(grep(y,x,fixed = T))>0})==1)
  if(length(n)>0) paste(names(n),":",sub(ns$'@context'[n],"",x),sep="") else x
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

format_column<-function(index,table,metadata){
  index<-sapply(metadata$tableSchema$columns,function(y){y$name==colnames(table)[index]})
  if(length(metadata$tableSchema$columns[index])>0){
  datatype<-metadata$tableSchema$columns[index][[1]]$datatype
  if(!is.atomic(datatype) && datatype$base=="date"){
    R_format<-gsub("yyyy","Y",datatype$format,perl=TRUE)
    R_format<-gsub("([[:alpha:]])+","%\\1",R_format,perl=TRUE)
    d<-lapply(table[,col_name],function(y) as.Date(y,format=R_format))
    R_format<-gsub("M","m",R_format,perl=TRUE)
    unlist(lapply(table[,colnames(table)[index]],function(y) as.character(as.Date(y,format=R_format))))
  }else if(length(datatype)>0 && datatype %in% c("string","gYear")){
    unlist(lapply(table[,colnames(table)[index]],as.character))
  }else{
    table[,colnames(table)[index]]
  }
  }
}

init<-function(minimal=F){
  store <<- new.rdf(FALSE)
  if(!minimal){
    add.prefix(store,prefix="csvw",namespace="http://www.w3.org/ns/csvw#")
    add.prefix(store,prefix="rdf",namespace="http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    add.prefix(store,prefix="dc",namespace="http://purl.org/dc/terms/")
    add.prefix(store,prefix="dcat",namespace="http://www.w3.org/ns/dcat#")
  }
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

csv2json<-function(url=NULL,metadata=NULL,link_header=NULL,minimal=F){
  if(!is.null(url)){
    tb<-Tabular(url,metadata,link_header)
    if("@id" %in% colnames(tb@tables[[1]])){
      tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x){ 
        if(grep("http://",x)){
          x
        }else{
          paste(url,x,sep="")
        }
      }
      )}
    tb1<-lapply(rownames(tb@tables[[1]]),row2json,tb@url,tb@tables[[1]],tb@header)
    if(minimal){
      toJSON(sapply(tb1,function(x)x$describes))
    }else{
      toJSON(list(tables=list(c(list(url=tb@url),tb@meta,list(row=tb1))))) 
    }
  }else{
    metadata_f<-fromJSON(getURL(metadata,.opts=curlOptions(followlocation=TRUE)))
    l<-lapply(metadata_f$tables,
           function(x){csv2json(gsub(tail(unlist(strsplit(metadata,"/")),n=1),x$url,metadata),metadata)})
    toJSON(list(tables=lapply(seq(1,length(l)),function(x)fromJSON(l[[x]])$tables[[1]])))
  }
}

row2json<-function(index,url,data,header){
  #row = lapply(data[index,][,!is.na(data[index,])],as.character)
  row = data[index,][,!is.na(data[index,])]
  list(url=paste(url,"#row=",strtoi(index)+header,sep=""),
       rownum=strtoi(index),
       describes=list(as.list(data.frame(row,check.names=F))))
}

csv2rdf<-function(url,metadata=NULL,link_header=NULL,minimal=F,output="store"){
  init()
  tb<-Tabular(url,metadata,link_header)
  tb@url <- tail(unlist(strsplit(url,"/")),n=1)
  if(!minimal){
    if("@id" %in% colnames(tb@tables[[1]])){tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x) paste(url,x,sep=""))}
    add.prefix(store,prefix="",namespace=paste(tb@url,"#",sep=""))
    tg1 <- create.blankNode(store)
    add.triple(store,tg1,rdf_type,csvw_tablegroup)
    tb1 <- create.blankNode(store)
    add.triple(store,tg1,csvw_table,tb1)
    add.triple(store,tb1,rdf_type,csvw_Table)
    add.triple(store,tb1,csvw_url,tb@url)
  }
  lapply(rownames(tb@tables[[1]]),row2rdf,tb@url,tb@tables[[1]],tb1,tb@header,minimal)
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

row2rdf<-function(index,url,data,tb,header,minimal=F){
  row = data[index,][,!is.na(data[index,])]
  if("@id" %in% colnames(row)){
    desc <- create.resource(store,as.character(row[1]))
  }else{
    desc <- create.blankNode(store)
  }
  if(!minimal){
  rowrdf <- create.blankNode(store)
  add.triple(store,rowrdf,rdf_type,csvw_Row)
  add.triple(store,rowrdf,csvw_describes,desc)
  add.triple(store,rowrdf,csvw_rownum,index)
  add.triple(store,rowrdf,csvw_url,paste(url,"#row=",strtoi(index)+header,sep=""))
  add.triple(store,tb,csvw_row,rowrdf)
  }
  lapply(seq(1,ncol(data)),rowdescribesrdf,data,index,desc,url,minimal)
}

rowdescribesrdf <- function(i,data,index,desc,url,minimal){
  if(!is.null(data[index,i]) & !is.na(data[index,i])>0){
    if(minimal){
      desc<-create.blankNode(store)
    }
    p<-create.property(store,paste(url,"#",colnames(data)[i],sep=""))
    add.triple(store,desc,p,as.character(data[index,i]))
  }
}