library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)

read.dump <-function(file, jvm_version="hotspot-8") {
  
  if(jvm_version != "hotspot-8" && jvm_version != "hotspot-7"){
    stop("wrong jvm version: expected hotspot-8 or hotspot-7")  
  }     
  
  read.entry <- function() {
    e <- character()
    i <- 1
    repeat {
      l <- readLines(con = con, n = 1, warn = F) 
      if (length(l) == 0 || l == "") { 
        break
      }
      e[i] = l[1]
      i <- i + 1
    }
    e
  }
  
  con <- file(description = file, open = "r")
  on.exit(close(con))
  
  header_regexp <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  
  java_thread_regexp <- "^\"(.+)\" .+ prio=(\\d{1,2}) os_prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+) \\[(.+)\\]$"
  sys_thread_regexp <- "^\"(.+)\" os_prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+)$"
  
  if (jvm_version == "oracle-7"){
    java_thread_regexp <- "^\"(.+)\" .+ prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+) \\[(.+)\\]$"
  }
  
  sys_thread_regexp <- "^\"(.+)\" os_prio=(\\d{1,2}) tid=(0x[\\da-f]+) nid=(0x[\\da-f]+) (.+)$"
  
  thread_list <- list()
  index <- 1
  
  while (length(e <- read.entry()) > 0) {
    g <- str_match(e[1], java_thread_regexp)
    if(!is.na(g[1])) {
      if (jvm_version == "hotspot-8") {
         thread_list[[index]] <- str_trim(c("java" , snapshot_str, g[2:length(g)], 
                                                str_match(e[2],"\\s.java.lang.Thread.State: (.+)")[2], 
                                                paste(e, collapse = "\n")))  
      } else {
        thread_list[[index]] <- str_trim(c("java", snapshot_str, g[2:3], NA, g[4:length(g)], 
                                               str_match(e[2],"\\s.java.lang.Thread.State: (.+)")[2], 
                                               paste(e, collapse = "\n")))
      } 
      index <- index + 1
      next
    }
    g <- str_match(e[1], sys_thread_regexp)
    if (!is.na(g[1])) {
      thread_list[[index]] <- str_trim(c("system", snapshot_str, g[2], NA, g[3:length(g)], 
                                             NA, NA, paste(e,sep="\n"))) 
      index <- index + 1
    } else if (grepl(header_regexp, e[1], perl = T)) {
        snapshot_str <- e[1] 
    }
  }
  
  n <- length(thread_list)
  threads <- data.table(type = character(n), snapshot = character(n), name = character(n), prio = character(n),
                        os_prio = character(n), id = character(n), native_id = character(n), state = character(n), 
                        addr = character(n), java_state = character(n), dump = character(n), stringsAsFactors = F)
  
  m <- ncol(threads) - 1
  for(i in 1:n) {
    for(j in 1:m) {
      set(threads, i, j, thread_list[[i]][j])    
    }  
  }
  
  mutate(threads, snapshot = as.POSIXct(snapshot), prio = as.integer(prio), os_prio = as.integer(os_prio),
         native_id_hex = native_id, native_id = as.integer(native_id)) %>%
  select(type, snapshot, name, id, native_id, native_id_hex, prio, os_prio, state, java_state, addr, dump) %>%
  arrange(snapshot, type, name) %>% as.data.frame()
}

dump.threads <- function(dump){
  select(dump,-dump)
}

dump.summary.state <-function(dump){
  dump %>% group_by(type, state, java_state) %>% summarise(count = n()) %>% as.data.frame()
}

dump.summary.state.plot <- function(dump) {
  d <- dump %>% group_by(state) %>% summarise(count = n()) %>% as.data.frame()  
  ggplot(data = d, aes(x = state, y = count)) + 
    ggtitle("Thread Count By State") +
    geom_bar(stat = "identity", fill = "deepskyblue3")
}

print.dump <- function(dump, file=""){
  cat(dump$dump, file=file, sep="\n")
}

dump.snapshots <- function(dump){
  unique(dump$snapshot)  
}