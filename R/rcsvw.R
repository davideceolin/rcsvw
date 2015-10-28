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
  dialect<-list()
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
      dialect<-metadata$dialect
      metadata<-metadata$tables[sapply(metadata$tables,function(x)(x$url==tail(unlist(strsplit(url,"/")),n=1)))][[1]]
    }
    header<-T
    if(!is.null(dialect$headerRowCount)){
      header<-dialect$headerRowCount>=1
      #row.nums<-seq(rownum,length.out=nrow(table))
    }else if(!is.null(dialect$header)){
      header<-(dialect$header=="true")
    }
    table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F,header=header)
    if(!is.null(dialect$rownum) && dialect$rownum!=0){
      rownames(table)<-seq(dialect$rownum,length.out=nrow(table))
    }
    if(!is.null(metadata$tableSchema$columns)){
      #check that all columns from csv file are moved
      #add _row, _name, _column, _sourcerow, _sourcecolumn
      # add context
      # compute name from title
      propertyUrl<-metadata$tableSchema$propertyUrl
      aboutUrl<-metadata$tableSchema$aboutUrl
      tab_list<-NULL
      lapply(metadata$tableSchema$columns,function(x){
        aboutUrl_col<<-lapply(seq(1,nrow(table)),function(y){
          stri_replace_all_fixed(ifelse(!is.null(x$aboutUrl),x$aboutUrl,aboutUrl),paste0("{", "_row","}"),y,vectorize_all = F)})
        if(is.null(tab_list)){
          tab_list<<-list(data.frame("@id"=aboutUrl_col))
        }else if (length(tab_list)==0 || length(tab_list[sapply(tab_list,function(x){trycatch(x["@id",1]==aboutUrl_col[1],except=NULL)})])==0){
          tab_list<<-append(tab_list,data.frame("@id"=aboutUrl_col))
        }
        tryCatch(
        tab_index<-tab_list[sapply(tab_list,function(x){x["@id",1]==aboutUrl_col[1]})],
        except=tab_index<-NULL)
        header_col<-check_ns(stri_replace_all_fixed(ifelse(is.null(x$propertyUrl),propertyUrl,x$propertyUrl),
                                           paste0("{",apply(expand.grid(c("#",""), c("_name",names(x))), 1, paste,collapse=""),"}",sep=""),
                                           paste0(c("#",""),c(ifelse(!is.null(x$name),x$name,URLencode(x$title)),x)),vectorize_all = F))
        if(!is.null(x$valueUrl)){
          data_col<-rep(check_ns(stri_replace_all_fixed(ifelse(is.null(x$propertyUrl),propertyUrl,x$propertyUrl),
                                              paste0("{",apply(expand.grid(c("#",""), c("_name",names(x))), 1, paste,collapse=""),"}",sep=""),
                                              paste0(c("#",""),c(x$name,x)),vectorize_all = F)),nrow(table))
        }else if(length(x$virtual)>0 && x$virtual){
          data_col<-rep(nrow(table),NULL)
        }else{
          print(x)
          data_col<-lapply(ifelse(is.element(x$name,names(table)),table[,x$name],table[,x$title]),format_column)
        }
        if(is.null(tab_index)){
          append(tab_list,structure(data.frame(data_col),names=header_col))
        }else{
          tab_list[[tab_index]]<<-cbind(tab_list[[tab_index]],structure(data.frame(data_col),names=header_col)) 
        }
      })
    }else{
      colnames(table)<-sapply(seq(1,ncol(table)),function(x)paste("_col.",x,sep=""))
    }
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    header<-T
    table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F)
    lapply(rownames(table),function(x) table[x,]<<-as.character(table[x,]))
    meta<-list()
  }
  new("Tabular",url=url,tables=list(table),meta=meta,header=header,foreignKeys=foreignKeys)
}

check_ns<-function(x){
  ns<-fromJSON(getURL("http://www.w3.org/2013/json-ld-context/rdfa11"))
  n<-which(lapply(ns$'@context',function(y) {grepl(y,x,fixed = T)})==1)
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
  index<-sapply(metadata$tableSchema$columns,function(y){y$name==colnames(table)[index] || y$title==colnames(table)[index]})
  if(length(metadata$tableSchema$columns[index])>0){
    datatype<-metadata$tableSchema$columns[index][[1]]$datatype
    if(!is.atomic(datatype) && datatype$base=="date"){
      R_format<-gsub("yyyy","Y",datatype$format,perl=TRUE)
      R_format<-gsub("([[:alpha:]])+","%\\1",R_format,perl=TRUE)
      d<-lapply(table[,index],function(y) as.Date(y,format=R_format))
      R_format<-gsub("M","m",R_format,perl=TRUE)
      unlist(lapply(table[,colnames(table)[index]],function(y) as.character(as.Date(y,format=R_format))))
    }else if(length(datatype)>0 && datatype %in% c("string","gYear")){
      unlist(lapply(table[,colnames(table)[index]],as.character))
    }else if(length(datatype)>0 && datatype == "number"){
      table[,colnames(table)[index]]
    }else if(length(datatype)>0 && datatype == "integer"){
      round(table[,colnames(table)[index]],0)
    }else{
      as.character(table[,colnames(table)[index]])
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
        if(grepl("http://",x)){
          x
        }else{
          paste(url,x,sep="")
        }
      })}
    tb1<-lapply(rownames(tb@tables[[1]]),row2json,tb@url,tb@tables[[1]],tb@header)
    if(minimal){
      toJSON(sapply(tb1,function(x)x$describes))
    }else{
      toJSON(list(tables=list(c(list(url=tb@url),tb@meta,list(row=tb1))))) 
    }
  }else{
    metadata_f<-fromJSON(getURL(metadata,.opts=curlOptions(followlocation=TRUE)))
    l<-lapply(metadata_f$tables,
              function(x){csv2json(gsub(tail(unlist(strsplit(metadata,"/")),n=1),x$url,metadata),metadata,minimal=minimal)})
    if(length(l)==0){
      l<-csv2json(gsub(tail(unlist(strsplit(metadata,"/")),n=1),metadata_f$url,metadata),metadata,minimal=minimal)
    }
    if(minimal){
      toJSON(sapply(l,fromJSON))
    }else{
      toJSON(list(tables=lapply(seq(1,length(l)),function(x)fromJSON(l[[x]])$tables[[1]])))
    }
  }
}

row2json<-function(index,url,data,header){
  row = data[index,][,!is.na(data[index,])]
  list(url=paste(url,"#row=",strtoi(index)+header,sep=""),
       rownum=strtoi(index),
       describes=list(as.list(data.frame(row,check.names=F))))
}

csv2rdf<-function(url=NULL,metadata=NULL,link_header=NULL,minimal=F,output="store",tg=NULL,init=T){
  if(!is.null(url)){
    if(init) init()
    tb<-Tabular(url,metadata,link_header)
    tb@url <- tail(unlist(strsplit(url,"/")),n=1)
    if(!minimal){
      if("@id" %in% colnames(tb@tables[[1]])){tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x) paste(url,x,sep=""))}
      add.prefix(store,prefix="",namespace=paste(tb@url,"#",sep=""))
      if(is.null(tg)) {
        tg <- create.blankNode(store)
        add.triple(store,tg,rdf_type,csvw_tablegroup)
      }
      tb1 <- create.blankNode(store)
      add.triple(store,tg,csvw_table,tb1)
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
  }else{
    if(!minimal){
      init(F)
      tg <- create.blankNode(store)
      add.triple(store,tg,rdf_type,csvw_tablegroup)
    }else{
      tg<-NULL
      init(T)
    }
    metadata_f<-fromJSON(getURL(metadata,.opts=curlOptions(followlocation=TRUE)))
    lapply(metadata_f$tables,
           function(x){csv2rdf(url=gsub(tail(unlist(strsplit(metadata,"/")),n=1),x$url,metadata),metadata=metadata,tg=tg,minimal=minimal,init=F)})
    #toJSON(list(tables=lapply(seq(1,length(l)),function(x)fromJSON(l[[x]])$tables[[1]])))
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