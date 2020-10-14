#' @title testCharSystem function
#' @description This function helps to detect characters originated from different Unicode blocks.
#' @param dat data vector
#' @param addCharSys the list of character blocks.  If not defined, c("Latin", "Cyrillic") are used.
#' @param markword if TRUE (default), detect the word containing anomalous  character and mark it. If FALSE, detect an anomalous character within a word and mark that character.
#' @param autochange if TRUE change characters based on the proposed coding rules in "charcodescheme.csv" (for more than one character "|" separator appears, for an unknown character that character is replaced with "?").
#' @export
#' @importFrom utils read.csv
#' @importFrom stringdist stringdist
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stats var
#' @import Unicode
#' @import dplyr
#' @import devtools
#' @return Returns an altered data vector with anomalous words/characters/replacements surrounded by asterisks (*).
#' @examples
#'
#' library(HooverArchives)
#' library(stringi)
#'
#' dat_vectorR <- c("\u0418\u043D\u0444\u043E\u0440\u043C\u0061\u0446\u0438\u044F", "\u0410\u0440\u0078\u0438\u0432\u044B")
#' dat_vector <- stri_unescape_unicode(dat_vectorR)
#'
#' # Mark the word
#' testCharSystem(dat_vector, addCharSys=c("Latin", "Cyrillic"), autochange=FALSE, markword=TRUE)
#'
#' # Mark anamolous character
#' testCharSystem(dat_vector, addCharSys=c("Latin", "Cyrillic"), autochange=FALSE, markword=FALSE)
#'
#' # Replace anamolous character with correct character and mark it
#' testCharSystem(dat_vector, addCharSys=c("Latin", "Cyrillic"), autochange=TRUE, markword=FALSE)



testCharSystem <- function(dat, addCharSys=NULL, markword=TRUE, autochange=FALSE){


  punctuation<-c("U+0021", "U+0022", "U+0023", "U+0025", "U+0026", "U+0027",
                 "U+002A", "U+002C", "U+002E", "U+002F", "U+003B", "U+003F",
                 "U+0040", "U+005C", "U+00A1", "U+00A7", "U+00B6", "U+00B7",
                 "U+00BF", "U+002D", "U+0028", "U+0029", "U+005B", "U+005D",
                 "U+003C", "U+003E", "U+003D", "U+003A", "U+007C", "U+0060",
                 "U+002B", "U+005E", "U+005F", "U+007E", "U+007B", "U+007D",
                 "U+0024",
                 "U+00AB", "U+2039", "U+00BB", "U+203A", "U+201E", "U+201C",
                 "U+201F", "U+201D", "U+2019", "U+0022", "U+275D", "U+275E",
                 "U+276E", "U+276F", "U+2E42", "U+301D", "U+301E", "U+301F",
                 "U+FF02", "U+201A", "U+2018", "U+201B", "U+275B", "U+275C"
                 )


  if(is.null(addCharSys)){CharSysv<-c("Latin", "Cyrillic")
                          }else{
                          CharSysv<-addCharSys}

  unicode_charsets<-as.u_char_seq(u_scripts(addCharSys))
  unicode_charsets[["punctuation"]]<-punctuation


  if(isTRUE(autochange)){
    sch<-read.csv(system.file("charcodescheme.csv", package="HooverArchives"),
                  encoding = "UTF-8", stringsAsFactors=FALSE)
  }


  SentenceIntoWords<-function(sentence){
    punctChars <- c(" ", "\\-", "\\:", "\\!", "\\+", "\\?", "\\#",
                    "\\$", "\\%", "\\^", "\\&", "\\*", "\\\\", "\\/",
                    "\\|", "\\_", "\\,", "\\.",  "\\;", "\\\"", "\\(",
                    "\\)", "\\[", "\\]", "\\>", "\\{", "\\}", "\\~", "'", "`", "\u00AB")
    sentence<-gsub(paste(punctChars, collapse="|"), " " , sentence)
    splS <- unlist(strsplit(sentence, " ", perl=TRUE))
    splS <- splS[splS!=""]
    splS
  }

  FindMisCode <- function(wcs){
    v.wcs <- unlist(wcs)
    v.wcs <- v.wcs[v.wcs!="punctuation"]
    length(as.vector(table(v.wcs)))>1
  }

  WordCodeSys <- function(word){

    word_unicode <- as.character(unlist(as.u_char_seq(enc2utf8(word), "")))

    encodings_search<-lapply(word_unicode, function(x){
      lapply(unicode_charsets, function(y) {
        grepl(gsub("\\+", "\\\\+", gsub("<|>", "", x)), y)})})

    found_encodings<-lapply(encodings_search, function(x) {
      charx<-unlist(lapply(x,any));
      if(all(!charx)){charx=TRUE; names(charx)<-"None"}; charx})

    found_encodings_summary<-lapply(found_encodings, function(x){
      if(any(x)){names(x)[which(x)]}});

    found_encodings_summary
  }

  MarkSentence<-function(sentence, parse.sentence, coding.word, markword){
    marked.sentence <- sentence
    for(word in 1:length(parse.sentence)){
      if(markword){
        if(coding.word[word]){
          marked.sentence<-gsub(parse.sentence[word],
                                paste("*", parse.sentence[word], "*", sep=""),
                                marked.sentence)}
      }else{
          marked.sentence<-gsub(parse.sentence[word],
                                coding.word[word],
                                marked.sentence)}
        }
    marked.sentence
  }

  EntryParser<-function(sentence, markword, autochange){

    parse.sentence <- SentenceIntoWords(sentence)

    if(isTRUE(markword)){

      coding.word <- unlist(lapply(parse.sentence, function(x) {
                   p.wordsys <- WordCodeSys(x)
                   coding.word <- FindMisCode(p.wordsys)}))
      sent.output <- MarkSentence(sentence, parse.sentence, coding.word, markword)
      }

    if(isFALSE(markword)){
      coding.char <- unlist(lapply(parse.sentence, function(x) {
        p.wordsys <- WordCodeSys(x)
        tab.p.wordsys <- table(unlist(p.wordsys))
        if(length(tab.p.wordsys)>1){
          min.letter <- attributes(which.min(table(unlist(p.wordsys))))$names
          if(var(table(unlist(p.wordsys))) == 0){
            min.letter <- CharSysv[1]
          }
          vm.letter <- which(unlist(p.wordsys)==min.letter)
          vm.letter.char <- unique(substring(x, vm.letter, vm.letter))
          for (i in vm.letter.char){
            if(isFALSE(autochange)){
              x<-gsub(i, paste("*", i, "*", sep=""), x)
              }else{
              uni.obs<-gsub("<|>", "", as.character(as.u_char_seq(enc2utf8(i), "")))
              if(uni.obs%in%sch$Observed_Unicode){
                uni.rep<-paste(sch$Expected_Unicode[sch$Observed_Unicode%in%uni.obs], collapse="|")
                uni.char<-unlist(strsplit(stri_unescape_unicode(gsub("U\\+(....)", "\\\\u\\1", uni.rep)), ","))
                x<-gsub(i, paste("*", uni.char, "*", sep=""), x)
              }else if(uni.obs%in%sch$Expected_Unicode){
                uni.rep<-paste(sch$Observed_Unicode[sch$Expected_Unicode%in%uni.obs], collapse="|")
                uni.char<-unlist(strsplit(stri_unescape_unicode(gsub("U\\+(....)", "\\\\u\\1", uni.rep)), ","))
                x<-gsub(i, paste("*", uni.char, "*", sep=""), x)
              }else{
                x<-gsub(i, "*?*", x)
              }
            }
          }
        }
        gsub("\\*+","*",x)}
        ))
      sent.output <- MarkSentence(sentence, parse.sentence, coding.char, markword)
    }

    return(sent.output)}

  if (length(dat)==1){
    ep.output <- EntryParser(dat, markword, autochange)
    }else{
      ep.output <- unlist(
                     lapply(dat, function(x){
                       if(is.na(x)|x==""){x
                         }else{
                           EntryParser(x, markword, autochange)}
                     }))
    }
  ep.output
  }
