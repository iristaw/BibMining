#由于合著行为，一条记录的地址字段会有多个作者，相应多个国家，故要将地址字段按“；”分割，但由于此字段也包含作者，作者信息中也有“；”，所以要先替换“[]”中的“；”为“#”
#require package stringr
wos.string.replace <- function(line, oldchar, newchar) {
  reg <- "\\[(\\s|\\S)*?\\]"
  pos <- str_locate_all(line,reg)
  lines <- str_sub(line, start = pos[[1]][, "start"], end = pos[[1]][, "end"])
  
  for (i in seq_along(lines)) {
    
    pos.new <- str_locate_all(lines[i], oldchar)
    
    pos.new <- pos.new[[1]] + pos[[1]][i, "start"] - 1
    
    for (i in seq_along(pos.new[, "start"])) {
      str_sub(line, start = pos.new[i, "start"], end = pos.new[i, "end"]) <- newchar
    }
    ##
  }
  return(line) ##
}

#现按 ";"进行分割
wos.item.addresses <- function(line) {
  #不为空或者包括;
  splitchar<-";"
  if (length(line)!=0) {
    if(str_detect(line,splitchar)>0){
      terms <- strsplit(line,splitchar)
      ##
      terms <- terms[[1]]
    } else {
      terms <- line  ##
    }
  } 
  return(terms)
}

#提取国家
wos.item.countries <- function(terms) {
  locatechar <- ","
  if (length(terms)!=0){
    countries<-c() ##
    for(term in terms) {
    allcomma <-str_locate_all(term,locatechar)
    rownum <- nrow(allcomma[[1]])  ##
    pos<-allcomma[[1]][rownum, ] ##
    pos <- as.numeric(pos)
    extractions<-str_sub(term,pos+1)
    extractions<-str_trim(extractions[1])
    countries<-c(countries,extractions) ##
    }
  }
  return(countries)
}
#modify USA
wos.items.USA <- function(results) {
  
  countries <- results[ ,"country"]
  
  pos <- str_detect( countries, "USA") 
  pos2 <- str_detect( countries, "[0-9]") 
  
  countries[pos | pos2] <- "USA"
  
  return(countries)
}

#统一调用以上函数（每条记录的国家对应每条记录的年份）

wos.items.countries <- function(items,start= NULL,end= NULL) {
  
  uts <- vector()
  years <- vector()
  tcs <- vector()
  extractions <- vector()##
  
  for (i in seq_along(items[, "C1"])) {
    line = items[i, "C1"]
    line2 = items[i, "RP"]
    line2=substr(line2,1,nchar(line2)-1)
    if (length(line)!=0) {
    line <- paste(line,line2,sep="; ")
    }
    else
    line <- paste(line,line2,sep="")
    #print(i)
    if (length(line)>0) {
      
      year <- as.integer(items[i, "PY"])
       
      ut <- as.character(items[i, "UT"]) ##
      
      tc <- as.integer(items[i,"TC"])
      
      line <- wos.string.replace(line,";","#")
      
      addresses <- wos.item.addresses(line)
      
      extraction <- wos.item.countries(addresses)
      
      extractions <- c(extractions, extraction) ##
      
      # gsub("\\.","",extractions)
      
      years <- c(years, rep(year, times = length(addresses)))
      
      uts <- c(uts, rep(ut, times = length(addresses))) ##
      
      tcs <- c(tcs, rep(tc, times = length(addresses)))
      
    }
  }
  countries = data.frame(extractions, years, uts, tcs, stringsAsFactors = FALSE) ##
  colnames(countries) <- c("country", "year","ut","cites")
  countries[,1] <- wos.items.USA(countries)
  countries <- na.omit(countries)
  countries[,1] <- str_to_upper(countries[,1])
  countries <- countries[!duplicated(countries),]
  countries$country[which(countries$country=="AL"|countries$country=="AK"
                          |countries$country=="AS"|countries$country=="AZ"
                          |countries$country=="AR"|countries$country=="CA"
                          |countries$country=="CO"|countries$country=="CT"
                          |countries$country=="DE"|countries$country=="DC"
                          |countries$country=="FM"|countries$country=="FL"
                          |countries$country=="GA"|countries$country=="GU"
                          |countries$country=="HI"|countries$country=="ID"
                          |countries$country=="IL"|countries$country=="IN"
                          |countries$country=="IA"|countries$country=="KS"
                          |countries$country=="KY"|countries$country=="LA"
                          |countries$country=="ME"|countries$country=="MH"
                          |countries$country=="MD"|countries$country=="MA"
                          |countries$country=="MI"|countries$country=="MN"
                          |countries$country=="MS"|countries$country=="MO"
                          |countries$country=="MT"|countries$country=="NE"
                          |countries$country=="NV"|countries$country=="NH"
                          |countries$country=="NJ"|countries$country=="NM"
                          |countries$country=="NY"|countries$country=="NC"
                          |countries$country=="ND"|countries$country=="MP"
                          |countries$country=="OH"|countries$country=="OK"
                          |countries$country=="OR"|countries$country=="PW"
                          |countries$country=="PA"|countries$country=="PR"
                          |countries$country=="RI"|countries$country=="SC"
                          |countries$country=="SD"|countries$country=="TN"
                          |countries$country=="TX"|countries$country=="UT"
                          |countries$country=="VT"|countries$country=="VI"
                          |countries$country=="VA"|countries$country=="WA"
                          |countries$country=="WV"|countries$country=="WI"
                          |countries$country=="WY")] <-"USA"
  if ((!is.null(start))&(!is.null(end))) {
  countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  return(countries)
  
}

#colaborate countries
library(dplyr)
wos.items.cocountries <- function(items,start= NULL,end= NULL) {
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  cocountries <- countries %>% group_by(ut) %>% mutate(count=n()) %>% filter(count>1) %>% select(-count)
  return(cocountries)
}
#3 coloumns data frame


wos.items.togethers <- function(results) {
  
  uniqueuts <- unique(results[ ,"ut"])
  
  togethers1 <- vector()
  
  togethers2 <- vector()
  
  for(i in seq_along(uniqueuts)) {
    #unique_uts
    
    ut <- uniqueuts[i]
    
    pos <- which(results[,"ut"] == ut)
    
    countries <- results[pos, "country"]
    
    uniquecountries <- unique(countries)
    
    if(length(uniquecountries) > 1) {
      
      nums <- length(uniquecountries)
      
      for(j in 1:(nums - 1)) {
       
        for(k in (j + 1):nums) {
        
        togethers1 <- c(togethers1, uniquecountries[j])
        togethers2 <- c(togethers2, uniquecountries[k])
        
        }
      }
    }
  }
  togethers <- data.frame(togethers1, togethers2, Times = 1, stringsAsFactors = FALSE) 
  colnames(togethers) <- c("country1", "country2","Times")
  return(togethers)
}

#clean country1 = country2, country2 = country1, they are the same
wos.items.final <- function(togethers) {
  agg <- aggregate(Times ~ country1   +   country2, data = togethers, sum)
  
  nums <- nrow(agg)
  
  for(i in 1:(nums - 1)) {
    
    for(j in (i + 1):nums) {
      
     if(agg[i,"country1"] == agg[j,"country2"] & agg[i,"country2"] == agg[j,"country1"] & agg[i,"Times"] != 0 & agg[j,"Times"] != 0) {
       agg[i,"Times"] <- agg[i,"Times"] + agg[j,"Times"]
       agg[j,"Times"] <- 0
     }
      
    }
  }
  pos <- agg[ ,"Times"] != 0
  agg <- agg[pos, ]
  row.names(agg) <- 1:nrow(agg)
  return(agg)
}

wos.items.3cols <- function(items,start= NULL,end= NULL){
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  togethers <- wos.items.togethers(countries)
  agg <- wos.items.final(togethers)
  return(agg)
}
#creat 3columns for edges for network graph
wos.items.3cols_2 <- function(agg1){
  
  country1 <- agg1[,2]
  country2 <- agg1[,1]
  Times <- agg1[,3]
  agg2 <- data.frame(country1,country2,Times)
  agg <- rbind(agg1,agg2)
  return(agg)
}

#change work together data frame  into work together matrix 
wos.items.matrix <- function(items,start= NULL,end= NULL) {
  countries <- wos.items.countries(items)
  if ((!is.null(start))&(!is.null(end))) {
    countries <- subset (countries, (countries$year>=start)&(countries$year<=end))}
  togethers <- wos.items.togethers(countries)
  agg <- wos.items.final(togethers)
  allcountries <- unique(c(agg[ ,"country1"], agg[ ,"country2"]))
  mat <- matrix(0, nrow = length(allcountries), ncol = length(allcountries))
  row.names(mat) <- colnames(mat) <- allcountries
  nums <- nrow(agg)
  for(i in 1:nums) {
  
   pos1 <-  which(allcountries == agg[i, "country1"] )
   pos2 <-  which(allcountries == agg[i, "country2"] )
   mat[pos1,pos2] <-  mat[pos2,pos1] <- agg[i, "Times"]
      
}
 return(mat)
}
