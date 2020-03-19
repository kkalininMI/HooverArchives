#' @title Latin to Cyrillic conversion function
#' @description This function helps to convert transliterated Cyrillic to original Cyrillic.
#' @param mdat character vector to be back-transliterated to Cyrillic.
#' @param LARU rules of tranliteration from transliterated Cyrillic to original Cyrillic (the rules are listed in the file "transliterationLARU.csv").
#' @param RURU rules to correct transliterated original Cyrillic (the rules are listed in the file "transliterationRURU.csv").
#' @param EnglishDetection if set to TRUE, the script avoids transliteration of words found in the English vocabulary (file: english.txt). If set to FALSE, only user defined stop words are used (file: stopwordsfile.csv).
#' @param EnglishLength  threshold is set to ignore EnglishDectection words below given threshold.
#' @param RussianCorrection if set to TRUE, the script attempts to match every back-transliterated word with the Russian vocabulary (files: russian.txt and russian_surnames.txt).
#' @param SensitivityThreshold is used only if RussianCorrection==TRUE. It determines algorithm's sensitivity to mismatches (numbers closer to 0 define higher sensitivity to mismatches). SensitivityThreshold is set to 0.1 by default.
#' @export
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom stringdist stringdist
#' @import dplyr
#' @import devtools
#' @return Returns the vector of transliterated characters in Cyrillic.
#' @examples
#' library(HooverArchives)
#'
#' dat<-c("Mezhdunarodnaia gazeta. Gl. redaktor: Iu. Zarechkin. Moscow, Russia. Semiweekly. 199?",
#' "DEN' UCHITELIA komissiia po obrazovaniiu ob''edineniia Iabloko",
#' "III-ii RIM vestnik Rossiiskogo patrioticheskogo dvizheniia. Redaktory: M. Artem'ev, V. Rugich. Moscow, Russia.")
#'
#' converteddata <- fromLATtoCYR(dat, LARU=TRUE, RURU=FALSE, EnglishDetection=TRUE, EnglishLength=4)

fromLATtoCYR<-function(mdat, LARU=TRUE, RURU=FALSE, EnglishDetection=TRUE, EnglishLength=NULL, RussianCorrection=FALSE, SensitivityThreshold = 0.1){

  #Global settings
  Sys.setlocale("LC_ALL",locale = "Russian")
  autodetected <- NULL
  transRURU.vector <- NULL
  transLARU.vector <- NULL

  #loading data
  trv <- read.csv(system.file("transliterationLARU.csv", package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE);
  trv <- trv[,-1]
  trv <- data.frame(apply(trv, 2, function(y) gsub("\\_", " ", y)))
  trv[] <- lapply(trv, as.character)

  rrv <- read.csv(system.file("transliterationRURU.csv", package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE)
  rrv <- data.frame(apply(rrv, 2, function(y) gsub("\\_", " ", y)))
  rrv <- rrv[,-1]
  rrv[] <- lapply(rrv, as.character)

  swf <- read.csv(system.file("stopwordsfile.csv", package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE)
  swf <- data.frame(apply(swf, 2, function(y) gsub("\\_", " ", y)))
  stopwords <- as.character(swf$stopwords)

  dicR<-read.table(system.file("russian.txt", package="HooverArchives"),
                   header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)
  dicN<-read.table(system.file("russian_surnames.txt", package="HooverArchives"),
                   header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)
  dicRS<-unname(unlist(dicR),unlist(dicN))

  #Functions
  stopwords_encoder <- function(stopwords){
    stopwordsfixed <- strsplit(stopwords,"")
    result <- unlist(lapply(1:length(stopwordsfixed), function(x){
      paste(paste("@",stopwordsfixed[[x]], sep=""), collapse="")
    }))
    return(result)}

  translit<-function(dat, string_vec,  EnglishDetection, SensitivityThreshold, RussianCorrection){

    english_lookup <- function(a){
      splV <- unlist(strsplit(a," |\\."))
      splV <- gsub("\\d+|[[:punct:]]", "", splV)
      splV <- splV[splV!=""]
      splV <- unique(splV);

      minInd <- sapply(splV, function(x, y = dicE){
        xx <- tolower(x)
        yy <- tolower(y)
        yy <- yy[substring(yy,1,1)%in%substring(xx,1,1)]
        if(xx%in%yy){return(TRUE)}
        return(FALSE)})
      return(minInd)}

    fuzzy_function <- function(a, SensitivityThreshold){
      splV<-unlist(strsplit(a," |\\."))
      splV<-gsub(a," |\\.", splV)
      splV<-gsub("[[:punct:] ]+","", unlist(splV))
      splV<-gsub("\\d+", "", splV)
      splV<-splV[splV!=""]
      splV<-unique(splV);

      minInd <- sapply(splV, function(x, y = dicRS){
        xx<-tolower(x)
        yy<-tolower(y)
        yy = yy[substring(yy,1,1)%in%substring(xx,1,1)]
        if(xx%in%yy){return(x[xx%in%yy])}
        if(nchar(xx)<=2){y<-yy[nchar(yy)<=2]}else{
          yy<-yy[substr(yy,1,3)==substr(xx, 1,3)]
        }
        index <- stringdist(xx, yy,method='jw')

        if(length(index)>0){
          min_index <- min(index)
          if(min_index <= SensitivityThreshold){
            result<-yy[which.min(index)]
          }else{result<-xx}
        }else{result<-xx}
        return(result)})

      AllUpperCase <- function(s) {
        m = regexpr("[[:upper:]]+", s)
        res = regmatches(s, m) == s
        if(length(res)==0) res=FALSE
        return (res)
      }

      AllLowerCase <- function(s) {
        m = regexpr("[[:lower:]]+", s)
        res = regmatches(s, m) == s
        if(length(res)==0) res=FALSE
        return (res)
      }

      FirstUpperCase <- function(s) {
        res = grepl("^[[:upper:]][[:lower:]]+", s)
        if(length(res)==0) res=FALSE
        return (res)
      }

      for(i in 1:length(minInd)){
        tests <- c(AllUpperCase(enc2native(names(minInd)[i])),
                   AllLowerCase(enc2native(names(minInd)[i])),
                   FirstUpperCase(enc2native(names(minInd)[i])))

        if(any(tests)){
          if(tests[1]){minInd[i] <- toupper(enc2native(minInd[i]))}
          if(tests[2]){minInd[i] <- tolower(enc2native(minInd[i]))}
          if(tests[3]){minInd[i] <- gsub(paste('^',substr(enc2native(minInd[i]),1,1), sep=""),
                                         toupper(substr(enc2native(minInd[i]),1,1)), enc2native(minInd[i]))}

          a <- gsub(enc2native(names(minInd)[i]), enc2native(minInd[i]), enc2native(a))
        }}
      result <- a
      return(result)
    }

    #Operations
    colnames(dat) <- c("subV", "withV")

    subV.s <- split(dat$subV, nchar(dat$subV))
    subV.s[[length(subV.s)]]<-c(subV.s[[length(subV.s)]],"''", "'")
    stringWithStopWords <- string_vec
    stringWithStopWords <- gsub("\"", "''", stringWithStopWords)

    if( EnglishDetection==TRUE){
      autodetected <- english_lookup(stringWithStopWords)
      autodetected <- names(autodetected)[autodetected]
      if(length(autodetected) == 0){autodetected <- NULL}
      stopwords <- c(stopwords, autodetected)
    }

    stopWordsReplace <- stopwords_encoder(stopwords)

    for(i in 1:length(stopWordsReplace)){
      stringWithStopWords <- gsub(stopwords[i],stopWordsReplace[i],stringWithStopWords)
    }
    stringWithStopWords <-
      gsub("^''|((?<=[[:punct:][:space:]])+'')|''(?=[[:punct:][:space:]])|''$", "@'@'", stringWithStopWords, perl=TRUE)

    for(i in seq(length(subV.s),1,-1)){
      char_vS <- unlist(subV.s[i])
      char_vL <- toupper(char_vS)
      char_vC <- tryCatch(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                               enc2native(char_vS), perl = TRUE), error = function(e) e)
      if(inherits(char_vC,  "error")) {warning("ERROR"); char_vC <- NULL}
      chars <- unique(c(char_vS, char_vC, char_vL))

      j <- 1
      while (j<(length(chars)+1)) {

        if(chars[j]%in%dat$subV){
          charSubst <- dat$withV[dat$subV%in%chars[j]]
        }else if(chars[j]%in%toupper(dat$subV)){
          charSubst <- toupper(dat$withV)[toupper(dat$subV)%in%chars[j]]
        }else if(grepl("[[:alpha:]]", chars[j])){
          dat$subVS <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", enc2native(dat$subV), perl = TRUE)
          dat$withVS <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", enc2native(dat$withV), perl = TRUE)
          charSubst <- dat$withVS[dat$subVS%in%chars[j]]
        }

        regexpr<-paste("(?<!\\@)", chars[j], sep="")
        if(regexpr=="(?<!\\@)''"){
          if(grepl("(?<=[[:lower:]])''|((?<=\\s[[:upper:]])'')|((?<=^[[:upper:]])'')", string_vec, perl=TRUE)){
            stringWithStopWords<-
              gsub("(?<=[[:lower:]])''|((?<=\\s[[:upper:]])'')|((?<=^[[:upper:]])'')", charSubst[1], stringWithStopWords, perl=TRUE)}

          if(grepl("(?<=[[:upper:]])''", stringWithStopWords, perl=TRUE)){
            stringWithStopWords<-gsub("(?<=[[:upper:]])''", toupper(charSubst[1]), stringWithStopWords, perl=TRUE)}
        }

        if(regexpr=="(?<!\\@)'"){
          if(grepl("((?<=[[:lower:]])')|((?<=\\s[[:upper:]])')|((?<=^[[:upper:]])')", stringWithStopWords, perl=TRUE)){
            stringWithStopWords <-
              gsub("((?<=[[:lower:]])')|((?<=\\s[[:upper:]])')|((?<=^[[:upper:]])')",  charSubst[1], stringWithStopWords, perl=TRUE)}

          if(grepl("(?<=[[:upper:]])'", string_vec, perl=TRUE)){
            stringWithStopWords <- gsub("(?<=[[:upper:]])'",toupper(charSubst[1]), stringWithStopWords, perl=TRUE)}
        }
        if(regexpr!="(?<!\\@)'"|regexpr!="(?<!\\@)''"){
          stringWithStopWords <- gsub(regexpr,charSubst[1], stringWithStopWords, perl=TRUE)
        }
        j = j + 1
      }
    }

    if(RussianCorrection){
      if(!is.na(stringWithStopWords)){
        stringWithStopWords <- fuzzy_function(stringWithStopWords, SensitivityThreshold)
      }
    }
    return(stringWithStopWords)}

  if( EnglishDetection){
    dicE <- read.table(system.file("english.txt", package="HooverArchives"),
                       header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)[,1]
    if(!is.null(EnglishLength)){
      dicE<-dicE[nchar(dicE)>=EnglishLength]
    }
  }

  if (is.character(mdat) & length(mdat)==1){
    if (LARU){
      transLARU.vector <- translit(trv, mdat,  EnglishDetection, SensitivityThreshold, RussianCorrection)
    }

    if (LARU & RURU){
      transRURU.vector <- translit(rrv, transLARU.vector,  EnglishDetection = FALSE, SensitivityThreshold, RussianCorrection)
    }

    if (RURU & isFALSE(LARU)){
      transRURU.vector <- translit(rrv, mdat,  EnglishDetection = FALSE, SensitivityThreshold, RussianCorrection)
    }
  }

  if (is.character(mdat) & length(mdat) > 1){
    if (LARU){
      transLARU.list <- lapply(1:length(mdat), function(iter) {
        translit(trv, mdat[iter],  EnglishDetection, SensitivityThreshold, RussianCorrection)
      })
      transLARU.vector <- unlist(transLARU.list)
    }

    if (RURU & LARU){
      transRURU.list <- lapply(1:length(mdat), function(iter) {
        translit(rrv, transLARU.vector[iter],  EnglishDetection=FALSE, SensitivityThreshold, RussianCorrection)
      })
      transRURU.vector <- unlist(transRURU.list)
    }

    if (RURU & isFALSE(LARU)){

      transRURU.list <- lapply(1:length(mdat), function(iter) {
        translit(rrv, mdat[iter],  EnglishDetection=FALSE, SensitivityThreshold, RussianCorrection)
      })

      transRURU.vector <- unlist(transRURU.list)
    }
  }

  if(isFALSE(RURU) & isFALSE(LARU)){
    result <- mdat
  }else{
    if(RURU){
      result <- transRURU.vector
    }else{
      result <- transLARU.vector
    }
  }

  result<-gsub("@", "", result)

  return(result)}
