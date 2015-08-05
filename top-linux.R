library(dplyr)
library(stringr)

read.top <- function(file) {
  
  con <- file(description = file, open = "r")
  
  on.exit(close(con))
  
  sum_col_classes <- c("character","numeric","numeric","numeric",
                       "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","integer")
  
  sum_col_names <- c("snapshot","load_avg1","load_avg5","load_avg15",
                     "cpu_us","cpu_sy","cpu_ni","cpu_id","cpu_wa","cpu_hi","cpu_si","cpu_st","nthreads")
  
  summary <- read.table(text="", colClasses = sum_col_classes, col.names = sum_col_names)
  
  col_classes <- c("integer","character","character","character","character","character","character",
                   "character","numeric","numeric","character","character","character")
  
  col_names <- c("pid","user","pr","ni","virt","res","shr","s","cpu","mem","time","command","params")
  
  threads <- read.table(text = "", colClasses = col_classes, col.names = col_names, stringsAsFactors = F)
  
  read.summary <- function(skip = 0) {
    
    if (skip > 0) readLines(con = con, n = skip)
    l <- readLines(con = con, n = 1)
    
    if(length(l) == 0) return(vector())
    
    m <- str_match(l[1], "top - (\\d{2}:\\d{2}:\\d{2}) up\\s+(?:\\d+ days, \\d+:\\d+,|\\d+:\\d+,)\\s+[^,]+,\\s+load average: ([^,]+), ([^,]+), (.+)")
    snapshot <- m[2] 
    load_avg <- as.numeric(m[3:5])
    
    t <- scan(file=con, nlines = 1, what=character(), sep = ",", quiet = T)
    nthreads <- as.integer(str_match(t[1], "Threads: (\\d+) total")[2])
    
    c <- scan(file=con, nlines = 1, what=character(), sep = ",", quiet = T)
    cpu <- as.numeric(gsub("[^\\.\\d]+(\\d+\\.\\d+)[^\\.\\d]+","\\1", c ,perl = T)) 
    
    list(snapshot, load_avg[1],load_avg[2], load_avg[3], cpu[1], cpu[2], cpu[3], cpu[4], 
         cpu[5], cpu[6], cpu[7], cpu[8], nthreads)
  }
  
  read.threads <- function(nthreads,snapshot) {
    t <- as.data.frame(scan(file=con, nlines = nthreads, what=sapply(col_classes,do.call,list(0)), skip = 4, 
                            strip.white = T,fill = T, na.strings = "",quiet = T), stringsAsFactors=F) 
    names(t) <- col_names
    mutate(t, snapshot = snapshot)
  }
  
  s <- read.summary()
  while(length(s) > 0) {
    summary[nrow(summary) + 1,] = s
    threads <- rbind(threads,read.threads(s[[length(s)]],s[[1]]))
    s <- read.summary(skip = 1)
  }
  summary %>% merge(select(threads,-params))
}

top.snapshots <- function(top) {
  select(top,snapshot) %>% unique()
}