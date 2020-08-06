#' @title Latin to Cyrillic conversion function
#' @description This function helps to convert transliterated Cyrillic to original Cyrillic.
#' @param mdat character vector to be back-transliterated to Cyrillic.
#' @param tolanguage language the text needs to be converted to ("Russian" by default)
#' @param LAOR rules of tranliteration from transliterated Cyrillic to original Cyrillic (the rules are listed in the file "transliterationLAOR.csv").
#' @param OROR rules to correct transliterated original Cyrillic (the rules are listed in the file "transliterationOROR.csv").
#' @param EnglishDetection if set to TRUE, the script avoids transliteration of words found in the English vocabulary (file: english.txt). If set to FALSE, only user defined stop words are used (file: stopwordsfile.csv).
#' @param EnglishLength  threshold is set to ignore EnglishDectection words below given threshold.
#' @param RussianCorrection if set to TRUE, the script attempts to match every back-transliterated word with the Russian vocabulary (files: russian.txt and russian_surnames.txt).
#' @param SensitivityThreshold is used only if RussianCorrection==TRUE. It determines algorithm's sensitivity to mismatches (numbers closer to 0 define higher sensitivity to mismatches). SensitivityThreshold is set to 0.1 by default.
#' @export
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom stringdist stringdist
#' @importFrom stringi stri_unescape_unicode
#' @import dplyr
#' @import devtools
#' @return Returns the vector of transliterated characters in Cyrillic.
#' @examples
#' library(HooverArchives)
#'
#' # conversion to Russian
#' dat<-c("Mezhdunarodnaia gazeta. Gl. redaktor: Iu. Zarechkin. Moscow, Russia. Semiweekly. 199?",
#' "DEN' UCHITELIA komissiia po obrazovaniiu ob''edineniia Iabloko",
#' "III-ii RIM vestnik Rossiiskogo patrioticheskogo dvizheniia. Redaktory: M. Artem'ev, V. Rugich. Moscow, Russia.")
#'
#' converteddata_ru <- fromLATtoCYR(dat, LAOR=TRUE, OROR=FALSE, EnglishDetection=TRUE)
#'
#'
#' # conversion to Ukrainian
#'dat<-read.csv(system.file("Ukraine_microform.csv", package="HooverArchives"),
#'                      sep=",", encoding = "UTF-8", stringsAsFactors = FALSE)
#'
#'converteddata_uk <- fromLATtoCYR(dat$FIELD.245, tolanguage="Ukrainian")

fromLATtoCYR<-function(mdat=NULL, tolanguage="Russian", LAOR=TRUE, OROR=FALSE, EnglishDetection=TRUE,
                       EnglishLength=NULL, RussianCorrection=FALSE, SensitivityThreshold = 0.1){

  #Global settings
  #Sys.setlocale("LC_ALL",locale = tolanguage)


  if(tolanguage=="Russian"){
    tryCatch(Sys.setlocale(category='LC_CTYPE', locale='ru_RU'),
             warning = function(c) Sys.setlocale(category='LC_CTYPE', locale='Russian_Russia.1251'))
    trvfilename="transliterationLARU.csv";
    rrvfilename="transliterationRURU.csv"}

  if(tolanguage=="Ukrainian"){tryCatch(Sys.setlocale(category='LC_CTYPE', locale='uk_UA'),
                                       warning = function(c) Sys.setlocale(category='LC_CTYPE', locale='Ukrainian_Ukraine.1251'))
    Sys.setlocale("LC_CTYPE",locale = "Ukrainian_Ukraine.1251");
    trvfilename="transliterationLAUK.csv";
    rrvfilename=NULL}

  if(RussianCorrection){
    dicR<-read.table(system.file("russian.txt", package="HooverArchives"),
                     header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)
    dicN<-read.table(system.file("russian_surnames.txt", package="HooverArchives"),
                     header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)
    dicRS<-unname(unlist(dicR),unlist(dicN))}


  autodetected <- NULL
  transOROR.vector <- NULL
  transLAOR.vector <- NULL
  #loading data
  trv <- read.csv(system.file(trvfilename, package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE);
  trv <- trv[,-1]
  trv <- data.frame(apply(trv, 2, function(y) gsub("\\_", " ", y)))
  trv[] <- lapply(trv, as.character)

  if(!is.null(rrvfilename)){
    rrv <- read.csv(system.file(rrvfilename, package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE)
    rrv <- data.frame(apply(rrv, 2, function(y) gsub("\\_", " ", y)))
    rrv <- rrv[,-1]
    rrv[] <- lapply(rrv, as.character)
  }


  swf <- read.csv(system.file("stopwordsfile.csv", package="HooverArchives"), encoding = "UTF-8", stringsAsFactors=FALSE)
  swf <- data.frame(apply(swf, 2, function(y) gsub("\\_", " ", y)))
  stopwords <- as.character(swf$stopwords)


  #Functions
  stopwords_encoder <- function(stopwords){
    stopwordsfixed <- strsplit(stopwords,"")
    result <- unlist(lapply(1:length(stopwordsfixed), function(x){
      paste(paste("@",stopwordsfixed[[x]], sep=""), collapse="")
    }))
    return(result)}

  translit<-function(dat, string_vec,  EnglishDetection, SensitivityThreshold, RussianCorrection){

    english_lookup <- function(a){
      varSpl <- " |\\-+|:+|!+|\\++|\\#+|\\#+|\\$|\\%+\\^+|\\&+|\\*|\\_+|\\:+|\\,+|\\.+|\\;+|\\'+$|\\\"+"
      splV <- unlist(strsplit(a, varSpl))
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

    if(any(grepl("(?<=\\\\u)\\w{4}", dat$withV, perl=TRUE))){
      dat$withV[grepl("(?<=\\\\u)\\w{4}", dat$withV, perl=TRUE)]<-
        stri_unescape_unicode((dat$withV[grepl("(?<=\\\\u)\\w{4}", dat$withV, perl=TRUE)]))
    }

    if(any(is.na(dat$withV))){
      dat$withV[is.na(dat$withV)]<-""
    }

    subV.s <- split(dat$subV, nchar(dat$subV))
    subV.s[[length(subV.s)]]<-c(subV.s[[length(subV.s)]],"''", "'", "\u0361")
    stringWithStopWords <- string_vec
    stringWithStopWords <- gsub("\"", "''", stringWithStopWords)

    if(EnglishDetection==TRUE){
      autodetected <- english_lookup(stringWithStopWords)
      autodetected <- tryCatch(names(autodetected)[autodetected], error = function(e) e)
      if(inherits(autodetected,  "error")){autodetected <-NULL}
      if(length(autodetected) == 0){autodetected <- NULL}
      stopwords <- unique(c(autodetected, stopwords))
    }

    stopWordsReplace <- stopwords_encoder(stopwords)

    for(i in 1:length(stopWordsReplace)){
      #stringWithStopWords <- gsub(paste0("\\b",stopwords[i], "\\b",
      #                                   "(?!\\')", sep=""), stopWordsReplace[i], stringWithStopWords, perl=TRUE)

      stringWithStopWords <- gsub(paste0("\\b",stopwords[i], "\\b", sep=""), stopWordsReplace[i], stringWithStopWords, perl=TRUE)
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
      chars <- chars[!grepl("\\\\U", chars)]

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

        regexprg<-paste("(?<!\\@)", chars[j], sep="")
        if(regexprg=="(?<!\\@)''"){
          if(grepl("(?<=[[:lower:]])''|((?<=\\s[[:upper:]])'')|((?<=^[[:upper:]])'')", string_vec, perl=TRUE)){
            stringWithStopWords<-
              gsub("(?<=[[:lower:]])''|((?<=\\s[[:upper:]])'')|((?<=^[[:upper:]])'')", charSubst[1], stringWithStopWords, perl=TRUE)}

          if(grepl("(?<=[[:upper:]])''", stringWithStopWords, perl=TRUE)){
            stringWithStopWords<-gsub("(?<=[[:upper:]])''", toupper(charSubst[1]), stringWithStopWords, perl=TRUE)}
        }

        if(regexprg=="(?<!\\@)'"){
          if(grepl("((?<=[[:lower:]])')|((?<=\\s[[:upper:]])')|((?<=^[[:upper:]])')", stringWithStopWords, perl=TRUE)){
            stringWithStopWords <-
              gsub("((?<=[[:lower:]])')|((?<=\\s[[:upper:]])')|((?<=^[[:upper:]])')",  charSubst[1], stringWithStopWords, perl=TRUE)}

          if(grepl("(?<=[[:upper:]])'", string_vec, perl=TRUE)){
            stringWithStopWords <- gsub("(?<=[[:upper:]])'",toupper(charSubst[1]), stringWithStopWords, perl=TRUE)}
        }
        if(regexprg!="(?<!\\@)'"|regexprg!="(?<!\\@)''"){
          if(grepl("(?<=\\\\u)\\w{4}", regexprg, perl=TRUE)){
            regexprg<-paste("(?<!\\@)",
                            "<U\\+",regmatches(regexprg, gregexpr("(?<=\\\\u)\\w{4}", regexprg, perl=TRUE)),">", sep="")

            stringWithStopWords <- gsub(regexprg, charSubst[1], stringWithStopWords, perl=TRUE)
          }else{
            stringWithStopWords <- gsub(regexprg, charSubst[1], stringWithStopWords, perl=TRUE)
          }
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



  if(EnglishDetection){
    dicE <- read.table(system.file("english.txt", package="HooverArchives"),
                       header = FALSE, sep = "", dec = ".", stringsAsFactors=FALSE)[,1]

    if(!is.null(EnglishLength)){
      dicE<-dicE[nchar(dicE)>=EnglishLength]
    }
  }

  if (is.character(mdat) & length(mdat)==1){
    if (LAOR|(isFALSE(LAOR)&isFALSE(OROR))){
      transLAOR.vector <- translit(trv, mdat,  EnglishDetection, SensitivityThreshold, RussianCorrection)
    }

    if (LAOR & OROR){
      transOROR.vector <- translit(rrv, transLAOR.vector,  EnglishDetection, SensitivityThreshold, RussianCorrection)
    }

    if (isFALSE(LAOR) & OROR){
      transOROR.vector <- translit(rrv, mdat,  EnglishDetection, SensitivityThreshold, RussianCorrection)
    }
  }

  if (is.character(mdat) & length(mdat) > 1){
    if (LAOR|(isFALSE(LAOR)&isFALSE(OROR))){
      transLAOR.list <- lapply(1:length(mdat), function(iter) {
        translit(trv, mdat[iter],  EnglishDetection, SensitivityThreshold, RussianCorrection)
      })
      transLAOR.vector <- unlist(transLAOR.list)
    }

    if (OROR & LAOR){
      transOROR.list <- lapply(1:length(mdat), function(iter) {
        translit(rrv, transLAOR.vector[iter],  EnglishDetection, SensitivityThreshold, RussianCorrection)
      })
      transOROR.vector <- unlist(transOROR.list)
    }

    if (OROR & isFALSE(LAOR)){

      transOROR.list <- lapply(1:length(mdat), function(iter) {
        translit(rrv, mdat[iter],  EnglishDetection, SensitivityThreshold, RussianCorrection)
      })

      transOROR.vector <- unlist(transOROR.list)
    }
  }

  if(isFALSE(OROR) & isFALSE(LAOR)){
    result <- transLAOR.vector
  }else{
    if(OROR){
      result <- transOROR.vector
    }else{
      result <- transLAOR.vector
    }
  }

  result<-gsub("@", "", result)

  return(result)}


