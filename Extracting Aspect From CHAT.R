utterance<-vector()
mor<-vector()
aspect.type<-vector()
speaker<-vector()
verb<-vector()
child<-vector()
visit<-vector()
verb.type<-vector()
verb.cat<-vector()

corpus.files <- dir(pattern="*.mor.pst.cex")

for (y in 1:length(corpus.files)) {
  text.file<- scan(corpus.files[y], what="", sep="\n") #select input file
  text.file2<-paste(text.file, collapse="___")
  text.file3<-gsub("___\\s+", " ", text.file2, perl=TRUE)
  text.file4<-unlist(strsplit(text.file3, "___", perl=TRUE))
  for (i in 1:length(text.file4)) {
    if (grepl("^\\%mor", text.file4[i], perl=T)==T) {
    if (grepl("(?<!\\:gerund)\\|(\\w+)?-PROG", text.file4[i], perl=T)==T) { #this is to avoid the extraction of n:gerund utterances
      utterance<-c(unlist(strsplit(text.file4[i-1], "\t"))[2], utterance)
      mor<-c(unlist(strsplit(text.file4[i], "\t"))[2], mor)
      aspect.type<-c("PROG", aspect.type)
      speaker<-c(unlist(strsplit(text.file4[i-1], "\t"))[1], speaker)
      verb.1<-gsub("(.+)?((v|part|aux)\\|\\w+?-PROG).*", "\\2", text.file4[i], perl=T)
      verb<-c(verb.1, verb)
      verb.type<-c(gsub("(.+)?\\|(\\w+)(-|&).+", "\\2", verb.1, perl=T), verb.type)
      verb.cat<-c(unlist(strsplit(verb.1, "\\|"))[1], verb.cat)
      child<-c(gsub("(.+)?visit.+", "\\1", corpus.files[y], ignore.case=T, perl=T), child)
      visit<-c(gsub("(.+)?visit(\\D)*(\\d).+", "\\3", corpus.files[y], ignore.case=T, perl=T), visit)
    }
    if (grepl("(v|part)\\|\\w+?(-|&)P(?!ROG)(?!L).{3}", text.file4[i], perl=T)==T) #this won't give us the copulas but not really concern with copulas right now
    {
      utterance<-c(unlist(strsplit(text.file4[i-1], "\t"))[2], utterance)
      mor<-c(unlist(strsplit(text.file4[i], "\t"))[2], mor)
      aspect.type<-c("PERF", aspect.type)
      speaker<-c(unlist(strsplit(text.file4[i-1], "\t"))[1], speaker)
      verb.1<-gsub("(.+)?((v|part)\\|\\w+?(-|&)P(?!ROG).{3}).*", "\\2", text.file4[i], perl=T)
      verb<-c(verb.1, verb)
      verb.type<-c(gsub("(.+)?\\|(\\w+)(-|&).*", "\\2", verb.1, perl=T), verb.type)
      verb.cat<-c(unlist(strsplit(verb.1, "\\|"))[1], verb.cat)
      child<-c(gsub("(.+)?visit.+", "\\1", corpus.files[y], ignore.case=T, perl=T), child)
      visit<-c(gsub("(.+)?visit(\\D)*(\\d).+", "\\3", corpus.files[y], ignore.case=T, perl=T), visit)
    }
  }
  }
}
  

aspect.utt<-data.frame(child, visit, speaker, utterance, mor, verb, verb.type, verb.cat, aspect.type)

dim(aspect.utt)

write.table(aspect.utt, sep="\t", row.names=F, col.names=T, "aspect.utterances.txt")