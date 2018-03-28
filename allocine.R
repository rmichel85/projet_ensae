#install.packages("rvest")
vignette("selectorgadget")

#essai lego

# J'ai fait une modif

library(rvest)
library(dplyr)
library(httr)
#library(jpeg)
#install.packages("imager")
library(imager)
library(stringr)
library(RCurl)

# 
# url<-"http://www.imdb.com/title/tt1490017/"
# lego_page<- read_html(url)
# 
# lego_page %>% html_nodes("strong span") %>% html_text() %>% as.numeric()
# lego_page %>% html_nodes("#titleCast .itemprop") %>% html_text()
# cast<- html_nodes(lego_page,"#titleCast .itemprop span")
# html_attrs(cast)
# ?html_text


#extraction de la liste de toutes les url d'un mois de sortie donné, avec le nom de chaque film
url_mois<- "http://www.allocine.fr/film/agenda/mois/mois-2018-03/"


liens_mois <- html_session(url_mois)
liste_url_mois<- paste0("http://www.allocine.fr",unlist(html_nodes(liens_mois,"h3 a") %>% html_attrs()))


textes_mois<- read_html(url_mois)
noms_films<- textes_mois %>% html_nodes("#col_main strong") %>% html_text() %>% gsub(pattern="\\n",x=.,"")
names(liste_url_mois)<- noms_films
liste_url_mois
length(liste_url_mois)



forme_eau<-"http://www.allocine.fr/film/fichefilm_gen_cfilm=246009.html"
pacific<-"http://www.allocine.fr/film/fichefilm_gen_cfilm=216485.html"
fargo<- "http://www.allocine.fr/film/fichefilm_gen_cfilm=14928.html"


#for (ind_film in 1:3) {
  

###infos à extraire d'un film en particulier



url_film<- "http://www.allocine.fr/film/fichefilm_gen_cfilm=246009.html"

#url_film<- liste_url_mois[ind_film]
id_film<-unlist(str_extract_all(url_film, "[0-9](.*)[0-9]"))
url_film_core<-"http://www.allocine.fr/film/fichefilm-"


id_mois<- "201803"
id<-paste0(id_mois,"_",id_film)



###page principale

url_principal_brut<-getURL(url_film)
nom_save_principal<-paste0(id,"_01principal",".txt")
write.csv(url_principal_brut,nom_save_principal)
principal<- read_html(url_principal_brut)

affiche<- principal %>% html_nodes(".col-md-4 .thumbnail-img") %>% html_attr("src")
#pour afficher l'affiche
nom_save_pprincipal<-paste0(id_film,"_01principal_photo",".jpg")
GET(affiche, write_disk(nom_save_pprincipal))
# im <- load.image("hostiles.jpg")
# plot(im)

date_duree<- principal %>% html_nodes(".meta-body-item:nth-child(1)") %>% html_text()
date_duree<- unlist(str_split(date_duree,"\\n"))
date_duree<- str_trim(date_duree)
date_duree<- date_duree[date_duree!=""]
date<- date_duree[2]

duree<- date_duree[3]
duree<- gsub("\\(","",duree)
duree<- gsub("\\)","",duree)
duree<- gsub(" ","",duree)

genres<- principal %>% html_nodes(".meta-body-item:nth-child(4)") %>% html_text()
genres<- unlist(str_split(genres,"\\n"))
genres<- str_trim(genres)
genres<- genres[genres!=""]
genres<- setdiff(genres,"Genres")
genres<- tolower(gsub(",","",genres))

genres_format<-character(5)
for (i in 1:5) {
  if (is.na(genres[i])) {
    genres_format[i]<-""
  } else {
    genres_format[i]<-genres[i]
  }
}

names(genres_format)<- c("genre1","genre2","genre3","genre4","genre5")


nationalites<- str_trim(principal %>% html_nodes(".nationality") %>% html_text())

nationalites_format<-character(3)
for (i in 1:3) {
  if (is.na(nationalites[i])) {
    nationalites_format[i]<-""
  } else {
    nationalites_format[i]<-nationalites[i]
  }
}

names(nationalites_format)<- c("nationalite1","nationalite2","nationalite3")



synopsis<- principal %>% html_nodes(".synopsis-txt") %>% html_text()

#à obtenir comme différence avec le nom des rubriques standards
titre_original<- principal %>% html_nodes("h2") %>% html_text()
liste_rubriques <-c("Synopsis et détails","Séances","Regarder ce principal","Bandes-annonces","Interviews, making-of et extraits",
                    "Critiques Presse","Critiques spectateurs","Photos","Secrets de tournage","Dernières news",
                    "Si vous aimez ce principal, vous pourriez aimer ...","Commentaires","Acteurs et actrices")
titre_original<-setdiff(titre_original,liste_rubriques)
titre_original<-str_trim(titre_original)

# distributeur <- principal %>% html_nodes("#synopsis-details .blue-link span") %>% html_text()
# distributeur<-str_trim(distributeur)
# 
# synopsis_details<- principal %>% html_nodes("#synopsis-details .blue-link") %>% html_text()
# synopsis_details<- gsub("\\n","",synopsis_details)
# synopsis_details<- str_trim(synopsis_details)
# synopsis_details_liste<-principal %>% html_nodes("#synopsis-details .light") %>% html_text()
# 
# 
# distributeur<-synopsis_details[1]
# recompenses<-synopsis_details[2]
# entrees<-synopsis_details[3]


synopsis_details<- principal %>% html_nodes(".that") %>% html_text()
synopsis_details<- gsub("\\n","",synopsis_details)
synopsis_details<- as.vector(str_trim(synopsis_details))
names_synopsis_details<- principal %>% html_nodes("#synopsis-details .light") %>% html_text()
names_synopsis_details<-str_trim(names_synopsis_details)
names(synopsis_details)<-names_synopsis_details


length(synopsis_details)
synopsis_details_socle<-character(17)
names(synopsis_details_socle)<-c("Titre original","Distributeur","Récompenses","Année de production","Date de sortie DVD",
                                    "Date de sortie Blu-ray","Date de sortie VOD","Type de film","Secrets de tournage",
                                    "Box Office France","Budget","Langues","Format production","Couleur","Format audio",
                                    "Format de projection","N° de Visa")

test<-synopsis_details[-c(2,5)]
condition<-names(synopsis_details_socle) %in% names(test)
synopsis_details_test<-synopsis_details_socle[condition]
synopsis_details_test<-test

# principal_main<-principal %>% html_nodes(".meta-body-item") %>% html_text()
# principal_main<- gsub("\\n","",principal_main)
# principal_main<- str_trim(principal_main)
# names_principal<-principal %>% html_nodes(".meta-body-item .light") %>% html_text()


table_principal<- c(id_mois,id_film,url_film,date,duree,genres_format,nationalites_format,synopsis,synopsis_details)





###pages non principales


##casting

url_casting<-paste0(url_film_core,id_film,"/casting/")

url_casting_brut<-getURL(url_casting)
nom_save_casting<-paste0(id,"_02casting",".txt")
write.csv(url_casting_brut,nom_save_casting)


casting<-read_html(url_casting_brut)

realisateurs<- casting %>% html_nodes(".casting-director .meta-title-link") %>% html_text()
realisateurs<- unlist(str_split(realisateurs,"\\n"))
realisateurs<- str_trim(realisateurs)
realisateurs<- realisateurs[realisateurs!=""]

acteurs1<- casting %>% html_nodes("#actors .meta-title-link") %>% html_text()
acteurs1<- unlist(str_split(acteurs1,"\\n"))
acteurs1<- str_trim(acteurs1)
acteurs1<- acteurs1[acteurs1!=""]

acteurs2<- casting %>% html_nodes("#actors .link") %>% html_text()
acteurs2<- unlist(str_split(acteurs2,"\\n"))
acteurs2<- str_trim(acteurs2)
acteurs2<- acteurs2[acteurs2!=""]

acteurs<-c(acteurs1,acteurs2)


roles1a<- casting %>% html_nodes("#actors .titlebar+ .row-col-padded-10") %>% html_text()
roles1a<- unlist(str_extract_all(roles1a, "Rôle(.*)\\n"))
roles1a<- gsub("\\n","",roles1a)
roles1a<- gsub("Rôle : ","",roles1a)

roles1b<- casting %>% html_nodes(".row-col-padded-10+ .row-col-padded-10 .row-col-padded-10") %>% html_text()
roles1b<- unlist(str_extract_all(roles1b, "Rôle(.*)\\n"))
roles1b<- gsub("\\n","",roles1b)
roles1b<- gsub("Rôle : ","",roles1b)

roles1<- c(roles1a,roles1b)

roles2<- casting %>% html_nodes("#actors span") %>% html_text()
roles2<- gsub("\\n","",roles2)
roles2<- str_trim(roles2)
roles2<- roles2[roles2!=""]
roles2<- setdiff(roles2,acteurs)

roles<- c(roles1,roles2)
length(roles)
length(acteurs)


autres<- casting %>% html_nodes(".casting-list .link") %>% html_text()

autres_tout<- casting %>% html_nodes(".casting-list") %>% html_text()
autres_tout<- gsub("\\n","",autres_tout)
autres_tout<- str_trim(autres_tout)

names_autres_tout<- casting %>% html_nodes(".casting-list .titlebar-title-md") %>% html_text()


autres_fonctions<-autres_tout
autres_fonctions<- unlist(str_split(autres_fonctions,"  "))
autres_fonctions<-setdiff(setdiff(autres_fonctions,"Production"),names_autres_tout)
autres_fonctions<-setdiff(autres_fonctions,autres)
#}

##réalisateur(trice)

url_realisation<- "http://www.allocine.fr/personne/fichepersonne_gen_cpersonne=18940.html"
id_realisation<-unlist(str_extract_all(url_realisation, "[0-9](.*)[0-9]"))

url_realisation_brut<-getURL(url_realisation)
nom_save<-paste0(id_realisation,"_03realisation",".csv")
write.csv(url_realisation_brut,nom_save)


realisation<-read_html(url_realisation_brut)

photo_rea<- realisation %>% html_nodes(".card-person-overview .thumbnail-img") %>% html_attr("src")
#pour afficher l'affiche
nom_save<-paste0(id_realisation,"_03realisation_photo",".jpg")
GET(photo_rea, write_disk(nom_save))


names_realisation<-realisation %>% html_nodes(".meta-body-item .light") %>% html_text()
names_realisation<-str_trim(names_realisation)

metiers<- realisation %>% html_nodes(".meta-body-item:nth-child(1)") %>% html_text()
metiers<- gsub("\\n","",metiers)
metiers<- unlist(str_split(metiers,"  "))
metiers<- gsub(",","",metiers)
metiers<-
metiers<- setdiff(metiers,c(names_realisation,"plus",""))


# url_presse<- "http://www.allocine.fr/film/fichefilm-228473/critiques/presse/"
# url_presse_brut<-getURL(url_presse)
# nom_save<-paste0(id,"_05presse",".csv")
# write.csv(url_presse_brut,nom_save)
# 
# presse<- read_html(url_presse)
# notes<- presse %>% html_nodes("span.stareval-link") %>% html_attr("title")

