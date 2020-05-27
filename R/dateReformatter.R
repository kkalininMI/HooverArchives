#' @title dateReformatter function
#' @description This function helps to standartize and correct misspellings of date entries.
#' @param datV character vector of dates.
#' @export
#' @importFrom stringdist stringdist
#' @import dplyr
#' @return Returns the vector of standardized and corrected dates.
#' @examples
#' library(HooverArchives)
#'
#' datesV<- c("1914:Aug 2 - 20; 1918:NOv 18",
#'            "1941:August-September 5, 1943 :Aug-1944:July")
#'
#' reformated.data <- dateReformatter(datesV)
#' reformated.data


dateReformatter<-function(datV){

  short.month<-c("Jan", "Feb", "Mar", "Apr",
                 "May", "Jun", "Jul","Aug",
                 "Sep", "Oct", "Nov", "Dec")


  convert_month <- function(mon, short.month){

    full.month<-c("January|January|Januar|Janua|Janu",
                  "February|Februar|Februa|Febru|Febr",
                  "March|Marc",
                  "April|Apri",
                  "May",
                  "June",
                  "July",
                  "August|Augus|Augu",
                  "September|Septembe|Septemb|Septem|Septe|Sept",
                  "October|Octobe|Octob|Octob|Octo",
                  "November|Novembe|Novemb|Novem|Nove",
                  "December|Decembe|Decemb|Decem|Dece")

    for(i in 1:12){mon<-gsub(full.month[i], short.month[i], mon)}

    for(i in 1:12){mon<-gsub(paste(short.month[i],"\\.", sep=""), paste(short.month[i]," ", sep=""), mon)}

    return(mon)}

  dateref<-function(refd, ...){

    regex <-
      "(\\w{3}\\s+(?!\\d{1,2}))|(\\w{3}(?!\\d{1,2})$)|(:\\w{3}\\s+(?!\\d{1,2}))|(:\\w{3}\\s?(?=\\-))"

    if(grepl(regex, refd, perl=TRUE)){
      result<-gsub("((?<=\\w{3})\\s\\d{1,2})|((?<=\\w{3})\\s\\d{1,2})", "", refd, perl=TRUE)
    }else{
      result=refd
    }

    mon<-regmatches(result, gregexpr("[[:alpha:]]{3}", result))[[1]]

    for(o in mon){
      if (isFALSE(grepl("^[[:upper:]][[:lower:]][[:lower:]]", o))){
        result<-gsub(o,  gsub("^.",  toupper(substring(tolower(o), 1,1)), o), result)}

      if (!o%in%short.month){
        tr<-sapply(o, function(x, y = short.month){
          index=stringdist(x, y,method='jw');
          min_index <- min(index)
          if(min_index <= 0.8){y[which.min(index)]}else{x}})
        result<-gsub(names(tr),  tr, result)
      }
    }
    return(result)}


  date_reformat<-function(datV, ...){
    datV[datV==""]<-NA
    vector.s.semi<-strsplit(datV, ";")
    vector.s.coma<-lapply(vector.s.semi, function(x) strsplit(x, ","))

    for (i in 1:length(datV)){
      len_vector.s.coma<-length(vector.s.coma[[i]])

      for(j in 1:len_vector.s.coma){
        len_vector.s.s<-length(vector.s.coma[[i]][[j]])

          for(k in 1:len_vector.s.s){
            vector.s.coma[[i]][[j]][[k]]<-dateref(vector.s.coma[[i]][[j]][[k]], short.month)
          }
          vector.s.coma[[i]][[j]]<-paste(vector.s.coma[[i]][[j]], collapse=",")
      }
        vector.s.coma[[i]]<-paste(vector.s.coma[[i]], collapse=";")
    }
    result<-unlist(vector.s.coma)

    return(result)
    }


  extra_reformat<-function(refd){
    refdV<-vector()
    #browser()
    for (i in 1:length(refd)){
      date_item <-refd[i]
      vector.year<-gsub(":|;", "",
                        unlist(regmatches(date_item,
                                          gregexpr("((?<!-)\\d{4}:)|((?<!-)\\d{4};)", date_item, perl=TRUE))))
      vector.s.year<-unlist(strsplit(date_item,
                                     "(\\d{4}:)|(\\d{4};)")); vector.s.year<-vector.s.year[vector.s.year!=""]

      test <- length(regmatches(date_item, gregexpr("\\d{4}", date_item, perl=TRUE))[[1]])

      if(length(vector.year)<test){
        vector.year<-gsub(":", "",
                          unlist(regmatches(
                            date_item, gregexpr("(\\d{3,4}-\\d{3,4}:)", date_item))))

        vector.s.year<-unlist(
          strsplit(date_item,
                   "\\d{3,4}-\\d{3,4}:")); vector.s.year<-vector.s.year[vector.s.year!=""]
      }
      vector.s.year<-gsub("(?<=[[:alpha:]])\\s*$", "",  vector.s.year, perl=TRUE)
      vector.s.year<-gsub(";(?!\\s*$) ", ", ", vector.s.year, perl=TRUE)
      vector.s.year<-gsub(",\\s*", ", ", vector.s.year)
      vector.s.year<-gsub("\\s+-\\s+", "-", vector.s.year)
      vector.s.year<-gsub("(?!(')|(\\))|(\\})|(\\])|(\\>))[[:punct:][:blank:]]+$", "", vector.s.year, perl=TRUE)
      vector_combined<-paste(paste(vector.year, vector.s.year, sep=":"), collapse=" ")
      vector_combined<-gsub("(,\\s*$)|((?<=.)$)|(\\s*$)", "", vector_combined, perl=TRUE)
      vector_combined<-gsub("(\\s(?=\\d{4}:))", "; ", vector_combined, perl=TRUE)
      vector_combined<-gsub("(:;)|(,;)", ";", vector_combined, perl=TRUE)
      vector_combined<-gsub("(^:)|(\n)", "", vector_combined, perl=TRUE)
      vector_combined<-gsub("^[[:punct:][:blank:]]*NA[[:punct:][:blank:]]*$", NA, vector_combined, perl=TRUE)

      refdV<-c(refdV, vector_combined)
    }

    result<-refdV

    return(result)}

  ##Main Body
  #a
  datV <- gsub("^\\s+", "",  datV, perl=TRUE)
  datV <- gsub("\\s*-\\s*", "-",  datV, perl=TRUE)

  #b
  datV <- convert_month(datV, short.month)

  datV <- gsub("\\s*:\\s*", ":",  datV, perl=TRUE)

  #d
  datV <- gsub("(,(?!\\s+))|,\\s+", ", ",  datV, perl=TRUE)

  #e, f
  datV <- date_reformat(datV, short.month)

  #extra
  datV <- extra_reformat(datV)

  #hyphens
  hyphens<-c("-+", "\u002D", "\u05BE", "\u1806","\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2015", "\u207B", "\u208B", "\u2212", "\uFE58", "\uFE63", "\uFF0D")
  hyphens<-paste(hyphens, collapse="|")
  datV <- gsub(hyphens, "-",  datV, perl=TRUE)

  #replace all semicolons with commas
  datV <- gsub(";", ",",  datV, perl=TRUE)

  #remove all NAs
  datV[is.na(datV)]<-""

  result<-datV

  return(result)}
