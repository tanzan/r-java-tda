library(stringr)
library(dplyr)

read.dump <-function(file, jvm_version="oracle-8"){
  raw_threads <- readLines(file)
  
  header_regexp <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  
  if(jvm_version != "oracle-8" && jvm_version != "oracle-7"){
    stop("wrong jvm version")  
  }     
  
  java_thread_regexp <- "^\"(.+)\" .+ prio=(\\d{1,2}) os_prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+) \\[(.+)\\]$"
  if (jvm_version == "oracle-7"){
    java_thread_regexp <- "^\"(.+)\" .+ prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+) \\[(.+)\\]$"  
  }
  
  sys_thread_regexp <- "^\"(.+)\" os_prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+)$"
  
  java_thread <- NULL
  java_stack <- F
  snapshot_str <- NULL
  
  threads <- data.frame(type=character(),snapshot_time=character(),name=character(),prio=character(),os_prio=character(),id=character(), native_id=character(), 
                        state=character(), addr=character(), java_state=character(), dump=character(), stringsAsFactors = F)  
  
  for(t in raw_threads) {
    if (grepl(header_regexp,t,perl = T)){
      snapshot_str <- t        
    } else if(grepl(java_thread_regexp,t,perl = T)){
      java_thread <- t 
      java_stack <- F
    } else if (grepl(sys_thread_regexp,t,perl = T)) {
      g <- str_match(t,sys_thread_regexp)
      threads[nrow(threads)+1,] = str_trim(c("system",snapshot_str,g[2],NA,g[3:length(g)],NA,NA,t))  
      java_stack <- F
    } else if (!is.null(java_thread)){
      g <- str_match(java_thread,java_thread_regexp)
      if (jvm_version == "oracle-8"){
        threads[nrow(threads)+1,] = str_trim(c("java",snapshot_str,g[2:length(g)],str_match(t,"\\s.java.lang.Thread.State: (.+)")[2],paste(java_thread,t,sep="\n")))  
      } else {
        threads[nrow(threads)+1,] = str_trim(c("java",snapshot_str,g[2:3],NA,g[4:length(g)],str_match(t,"\\s.java.lang.Thread.State: (.+)")[2],paste(java_thread,t,sep="\n")))
      }      
      java_thread <- NULL
      java_stack <- T
    } else if(java_stack && length(t) != 0){
      if (length(threads[nrow(threads),"dump"]) == 0) {
        threads[nrow(threads),"dump"] <- paste(threads[nrow(threads),"dump"],t)  
      } else {
        threads[nrow(threads),"dump"] <- paste(threads[nrow(threads),"dump"],t,sep = "\n")  
      }
    }
    
  }
  mutate(threads,snapshot_time=as.POSIXct(snapshot_time),prio=as.integer(prio),os_prio=as.integer(os_prio),native_id_hex=native_id,native_id=as.integer(native_id)) %>%
  select(type,snapshot_time,name,id,native_id,native_id_hex,prio,os_prio,state,java_state,addr,dump) %>%
  arrange(snapshot_time,type,name)
}

dump.threads <- function(dump){
  select(dump,-dump)
}

dump.summary <-function(dump){
  dump %>% group_by(type,state,java_state) %>% summarise(count=n()) %>% as.data.frame()
}

dump.summary.state.plot <- function(dump) {
  d <- dump %>% group_by(state) %>% summarise(count=n()) %>% as.data.frame()  
  with(d,barplot(count,names.arg = state))
}

print.dump <- function(dump){
  cat(dump$dump,sep="\n")
}

dump.snapshots <- function(dump){
  unique(dump$snapshot_time)  
}