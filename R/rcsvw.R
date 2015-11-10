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
    if(!is.null(metadata$"@context")){
      context<-paste0(metadata$"@context"[[1]],'/',metadata$url,"#")
    }else{
      context<-tail(unlist(strsplit(url,"/")),n=1)
    }
    context<-url
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
      propertyUrl<-metadata$tableSchema$propertyUrl
      aboutUrl<-metadata$tableSchema$aboutUrl
      columns<-metadata$tableSchema$columns
      lapply(columns,function(x){
        if(is.null(x$virtual) || !x$virtual){
          i<-which(colnames(table)==x$name)
          i<-ifelse(length(i)>0,i,which(colnames(table) %in% x$titles))
          if(length(i)>0) table[,i]<<-format_column(table[,i],x$datatype)
          }
        })
      
      # compute name from title
      propertyUrl<-metadata$tableSchema$propertyUrl
      aboutUrl<-metadata$tableSchema$aboutUrl
      columns<-metadata$tableSchema$columns
      tab_list<-NULL
      if(!is.null(metadata$tableSchema$primaryKey) || (!is.null(aboutUrl) && length(aboutUrl))){
        aboutUrls<-aboutUrl
        k<-list(colnames(table))
        names(k)<-ifelse(length(aboutUrl)>0,aboutUrl,"")
      }else{
        aboutUrls<-unique(lapply(columns,function(x)x$aboutUrl))
        if(!is.null(aboutUrls[[1]]) && length(aboutUrls)>0){
          k<-sapply(aboutUrls,function(u)
                      sapply(columns[sapply(columns, function(x) x$aboutUrl==u)],function(x) x$name))
          names(k)<-aboutUrls
        }else{
          k<-list(colnames(table))
          names(k)<-""
        }
      }
      if(length(metadata$dialect$trim)>0 && metadata$dialect$trim){
        colnames(table)<-gsub(" ","_",tolower(colnames(table)))
      }
      lapply(metadata$tableSchema$columns,function(x,y){
        if(length(x$virtual)>0 && x$virtual){
          col<-sapply(seq(1,nrow(table)),function(y){
            tmp<-stri_replace_all_fixed(x$valueUrl,
                  paste0("{",apply(expand.grid(c("#",""), c("_row",colnames(table))), 1, paste,collapse=""),"}",sep=""),
                  paste0(apply(expand.grid(c("#",""), c(y,table[y,])), 1, paste,collapse="")),vectorize_all = F)
            ifelse((grepl("http://",tmp[1]) || grepl(":",tmp[1])),tmp,gsub("##","#",paste0(context,tmp)))
            })
          col<-data.frame(col,stringsAsFactors = F,check.names = F)
          colnames(col)<-ifelse((length(y)>0 && y),gsub(" ","_",tolower(x)),x$name)
          table<<-cbind(table,col)
          #print(table)
        }
        else if(!is.null(x$valueUrl)){
          table[,x$name]<<-sapply(seq(1,nrow(table)),function(y){
            tmp<-stri_replace_all_fixed(x$valueUrl,
            paste0("{",apply(expand.grid(c("#",""), c("_row",colnames(table))), 1, paste,collapse=""),"}",sep=""),
            paste0(apply(expand.grid(c("#",""), c(y,table[y,])), 1, paste,collapse="")),vectorize_all = F)
            ifelse((grepl("http://",tmp[1])||grepl(":",tmp[1])),tmp,gsub("##","#",paste0(context,tmp)))
            })
        }
      },metadata$dialect$trim)
      tab_list<-list()
      lapply(names(k),function(x){
        aboutUrl_col<<-sapply(seq(1,nrow(table)),function(y){
          tmp<-stri_replace_all_fixed(x,
              paste0("{",apply(expand.grid(c("#",""), c("_row",colnames(table))), 1, paste,collapse=""),"}",sep=""),
              paste0(apply(expand.grid(c("#",""), c(y,table[y,])), 1, paste,collapse="")),vectorize_all = F)
          ifelse((grepl("http://",tmp[1])||grepl(":",tmp[1])),tmp,gsub("##","#",paste0(context,tmp)))
          })
        table_temp<-NULL
        table_temp<-data.frame(table[,unlist(k[names(k)==x])],stringsAsFactors = F,check.names = F)
        #print(table_temp)
        colnames(table_temp)<-sapply(colnames(table_temp),function(z){
          #print(x)
          #print(z)
          meta_col<-columns[sapply(columns,function(y){(y$name==z || z %in% y$titles)})][[1]]
          if("propertyUrl" %in% names(meta_col)){
            propUrl<-meta_col$propertyUrl
          }else{
            propUrl<-propertyUrl
          }
#           print("z::")
#           print(z)
#           print("titles::")
#           print(y$titles)
#           print("propUrl::")
#           print(propUrl)
          if(!is.null(propUrl) && length(propUrl)>0){
            tmp<-stri_replace_all_fixed(propUrl,
                paste0("{",apply(expand.grid(c("#",""), c("_col","_name")), 1, paste,collapse=""),"}",sep=""),
                paste0(apply(expand.grid(c("#",""), c(which(colnames(table)==meta_col$name),z)), 1, paste,collapse="")),vectorize_all = F)
            check_ns(ifelse((grepl("http://",tmp[1])||grepl(":",tmp[1])),tmp,gsub("##","#",paste0(context,tmp))))
          }else if(!is.null(meta_col$name) || !is.null(meta_col$titles)){
            ifelse(!is.null(meta_col$name),meta_col$name,URLencode(meta_col$titles))
          }else{
            x
          }
        })
        if(!(x=="")){
          table_temp<-cbind(aboutUrl_col,table_temp,stringsAsFactors=F)
          colnames(table_temp)[1]<-"@id"
        }
        tab_list[[length(tab_list)+1]]<<-table_temp
      })
    }else{
      colnames(table)<-sapply(seq(1,ncol(table)),function(x)paste("_col.",x,sep=""))
      tab_list<-list(table)
    }
    meta<-clean(metadata[names(metadata)[grepl(":", names(metadata))]])
  }else{
    header<-T
    table<-read.csv(text=getURL(url,.opts=curlOptions(followlocation=T)),check.names=F,stringsAsFactors = F)
    lapply(rownames(table),function(x) table[x,]<<-as.character(table[x,]))
    tab_list<-list(table)
    meta<-list()
  }
  new("Tabular",url=url,tables=tab_list,meta=meta,header=header,foreignKeys=foreignKeys)
}

check_ns<-function(x){
  ns<-fromJSON(getURL("http://www.w3.org/2013/json-ld-context/rdfa11"))
  n<-which(lapply(ns$'@context',function(y) {grepl(y,x,fixed = T)})==1)
  if(length(n)>0) paste(names(n),":",sub(ns$'@context'[n],"",x),sep="") else x
}

clean <-function(x){
  if(length(x)>0){
    if(!is.null(names(x)) && names(x)=="@id") x$"@id"
    else if(!is.null(names(x)) && "@value" %in% names(x)) x$"@value"
    else if(class(x) == "list") lapply(x,clean)
    else x
    }else x
}

format_column<-function(column, datatype){
  if(!is.null(datatype) && length(datatype)>0){
    if(!is.atomic(datatype) && (datatype$base=="date" || datatype$base=="datetime")){
      R_format<-gsub("yyyy","Y",datatype$format,perl=TRUE)
      if(datatype$base=="datetime"){
        R_format<-strsplit(R_format,"T")[[1]]
        R_format[1]<-gsub("([[:alpha:]])+","%\\1",R_format[1],perl=TRUE)
        R_format[2]<-gsub("([[:alpha:]])+","%\\1",R_format[2],perl=TRUE)
        R_format[1]<-gsub("%M","%m",R_format[1],perl=TRUE)
        R_format[2]<-gsub("%m","%M",R_format[2],perl=TRUE)
        R_format<-paste(R_format[1],"T",R_format[2])
        unlist(lapply(column,function(y) format(strptime(y,format=R_format),format="%Y-%m-%dT%H:%M:%S")))
      }else{
        R_format<-gsub("M","m",R_format,perl=TRUE)
        R_format<-gsub("([[:alpha:]])+","%\\1",R_format,perl=TRUE)
        unlist(lapply(column,function(y) as.character(as.Date(y,format=R_format))))  
      }
    }else if(length(datatype)>0 && datatype %in% c("string","gYear")){
      unlist(lapply(column,as.character))
    }else if(length(datatype)>0 && datatype == "number"){
      column
    }else if(length(datatype)>0 && datatype == "integer"){
      round(column,0)
    }else{
      as.character(column)
    }
  }else{
    as.character(column)
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
      tb@tables[[1]][,"@id"]<-sapply(as.vector(tb@tables[[1]][,"@id"]),function(x)ifelse(grepl("http://",x),x,paste(url,x,sep="")))
    }
    tb1<-lapply(rownames(tb@tables[[1]]),row2json,tb@url,tb@tables[[1]],tb@header,tb@tables)
    ifelse(minimal,toJSON(sapply(tb1,function(x)x$describes)),toJSON(list(tables=list(c(list(url=tb@url),tb@meta,list(row=tb1))))))
  }else{
    metadata_f<-fromJSON(getURL(metadata,.opts=curlOptions(followlocation=TRUE)))
    l<-lapply(metadata_f$tables,
              function(x){csv2json(gsub(tail(unlist(strsplit(metadata,"/")),n=1),x$url,metadata),metadata,minimal=minimal)})
    if(length(l)==0) l<-csv2json(gsub(tail(unlist(strsplit(metadata,"/")),n=1),metadata_f$url,metadata),metadata,minimal=minimal)
    ifelse(minimal,toJSON(sapply(l,fromJSON)),toJSON(list(tables=lapply(seq(1,length(l)),function(x)fromJSON(l[[x]])$tables[[1]]))))
  }
}

row2json<-function(index,url,data,header,tables){
  row <- data[index,][,!is.na(data[index,])]
  if("rdf:type" %in% colnames(row)) colnames(row)[colnames(row)=="rdf:type"]<-"@type"
  l<-list(as.list(data.frame(row,check.names=F,stringsAsFactors = F)))
  lapply(names(l[[1]])[-1],function(y) {
    t<-tables[sapply(tables,function(x){"@id" %in% colnames(x) && l[[1]][y] %in% x[,"@id"]})]
    if(!is.null(t) && length(t)>0){
      if("rdf:type" %in% colnames(t[[1]])) colnames(t[[1]])[colnames(t[[1]])=="rdf:type"]<-"@type"
      l_tmp<-t[[1]][t[[1]]$"@id"==l[[1]][y],]
      l[[1]][[y]]<<-l_tmp
    }
  })
  list(url=paste(url,"#row=",strtoi(index)+header,sep=""),
       rownum=strtoi(index),
       describes=l)
}

csv2rdf<-function(url=NULL,metadata=NULL,link_header=NULL,minimal=F,output="store",tg=NULL,init=T){
  if(!is.null(url)){
    if(init) init()
    tb<-Tabular(url,metadata,link_header)
    if(!minimal){
      lapply(tb@tables,function(x){
      if("@id" %in% colnames(x)){
        x[,"@id"]<-sapply(as.vector(x[,"@id"]),function(y) ifelse(grepl("http://",y),y,paste(url,y,sep="")))
      }
      })
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
    lapply(rownames(tb@tables[[1]]),row2rdf,tb@url,tb@tables,tb1,tb@header,minimal)
    print(store)
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
    if("tables" %in% names(metadata_f)){
    lapply(metadata_f$tables,
           function(x){
             csv2rdf(url=gsub(tail(unlist(strsplit(metadata,"/")),n=1),x$url,metadata),metadata=metadata,tg=tg,minimal=minimal,init=F)
             })
    }else{
      csv2rdf(url=gsub(tail(unlist(strsplit(metadata,"/")),n=1),metadata_f$url,metadata),metadata=metadata,tg=tg,minimal=minimal,init=F)
    }
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

row2rdf<-function(index,url,tables,tb,header,minimal=F){
  print("ok")
  lapply(tables,function(data){
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
    })
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