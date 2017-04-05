library(twitteR)
library(wordcloud)

#OBTENEZ VOS IDENTIFIANT DE L'API TWITTER SUR DEV TWITTER APPS
consumer_key <- "*****"
consumer_secret <- "*****"
access_token <- "*****"
access_secret <- "*****"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

#MOTS VIDE BASE SUR 
#UTILISATION D'UNE LISTE D EMOTS VIDES CLASSIQUE FORMAT CSV
mots_vides <- read.delim("C:/Users/Rouck/Desktop/mots_vides.csv",";",header=T)
mots_vides <- data.frame(mots_vides)
#mots <- NULL
#for ( i in 1:length(mots_vides[,1])) mots <- c(mots,strsplit(as.character(mots_vides[i,1]),",")[[1]])
mots <- gsub("^\\s{1,}","",mots)
#write.csv2(data.frame(mots),"C:/Users/Rouck/Desktop/mots_vides.csv")
mots_vides <- as.character(mots_vides$MOTS)

mots_usage <- c("https","http","legranddebat")
mots_vides <- c(mots_vides,mots_usage)
candidats <- c("melenchon","hamon","macron","fillon","lepen")

for (cand in candidats) {
mots_vides <- c(mots_vides,cand)
r_stats <- searchTwitter(cand, lang = "fr", n = 10000)
x <- twListToDF(twList = r_stats)
avis <- paste(x[,1],collapse = " ")
assign(paste0("avis_",cand),avis)
print(paste0(cand," avis recoltes"))
Sys.sleep(5)

avis <- get(paste0("avis_",cand))

liste <- unlist(strsplit(avis,split = " "))
res <- ""
for(mot in liste) {
  res <- c(res,mot)
}

rm(mot,liste)

res <- res[which(res != "")]
res <- res[which(res != "-")]
res <- res[which(res != "*")]
res <- tolower(res)

taille <- length(which(!is.na(match(mots_vides,res)))) 
while (taille != 0) {
  table <- match(mots_vides,res)
  table <- table[!is.na(table)]
  res <- res[-table]
  taille <- length(which(!is.na(match(mots_vides,res))))
}

res <- res[which(nchar(res) > 2)]
rm(table,taille)

resul <- data.frame(as.character(res))
res_analyse <- data.frame(table(resul))
res_analyse$ratio <- res_analyse$Freq / sum(res_analyse$Freq,na.rm = T)
res_analyse <- res_analyse[order(res_analyse$Freq,decreasing = T),]
top_analyse <- res_analyse[res_analyse$ratio > 0.005,]
mots_vides <- mots_vides[-length(mots_vides)]

wordcloud(res_analyse[,1],res_analyse[,2],random.color = F,random.order = F,max.word = 1000,color = brewer.pal(n = 8,name = "Set2"))
}
