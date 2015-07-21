# r-java-tda
R Utils for Java Thread Dump Analisys

To this moment tested for Oracle java 8 on linux.

## R Package Requirements

`install.packages("dplyr")`

## Usage

Script is able to analyse standalone dumps as well as contigious dumps.

Contigious dump format is similar to generated by this bash script example:

```for i in `seq 1 6`; do jstack <pid> >> dump.txt; sleep 10; done```


### Load Script
`source("java-tda.R")`

### Load Dump

`dump <- read.dump("dump.txt")`

### Threads Summary

`dump.summary(dump)`

### States Plot

`dump.summary.plot(dump)`

### Threads Table

`dump.threads(dump)`

### Dump Snapshots

`dump.snapshots(dump)`

## Advanced Usage Examples

R gives a full power to look at the every aspect of dump data. Let us look at the following examples.

### Print Dump Entries For the Specific Name

`filter(dump,grepl("SharedPool",name)) %>% print.dump()`

### Obtain Threads by Specific State

`filter(dump,state=="runnable") %>% dump.threads()`

### Etc ...


