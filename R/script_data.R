
###########################################
#    ANALYSE DES SUJETS DE PRESSE FRANCAISE
#    chargement de donnees et traitements
###########################################

# Source des donnees
# la liste des pays provient de 
# https://fr.wikipedia.org/wiki/Liste_des_pays_par_population
# https://fr.wikipedia.org/wiki/Liste_des_pays_ayant_le_fran%C3%A7ais_pour_langue_officielle
# http://www.touteleurope.eu/les-pays-de-l-union-europeenne.html
# liste des pays appartenant a l'OTAN
# ...
# tableau de correspondance des noms de pays, codes ISO 2 et 3 lettres.
# ...
# donnees de vols internationaux
# http://ec.europa.eu/eurostat/web/transport/data/database

mydir %>% paste0("/_data") %>% setwd

# 1. Import des donnees
liste.pays   <- import("liste_pays_pop.csv") # pays + population + appartenance ue/francoph/otan
dist         <- import("dist_capitales_paris.csv") # distance capitales /Paris
convert      <- import("conversion_code_pays_2_3.csv") # conversion ISO 2/3 lettres
pass         <- read.table(file="avia_par_fr.tsv", sep="\t", header=TRUE, stringsAsFactors=FALSE, na.strings=": ") # passagers depart FR
continent    <- import("liste_pays_continent.csv") # continent d'appartenance
# # Webscrapping - site du FIGARO
# liste.pays$pays <- trim.leading(liste.pays$pays)
# url.main <- "http://recherche.lefigaro.fr/recherche/"
# for (annee in 2007:2016){
#   number=numeric(0)
#   for (key in liste.pays$pays){
#     scrap  <- paste0(url.main, key, "/?datemin=01-01-", annee, "&datemax=31-12-", annee) %>% readLines
#     # write text file to ckeck
#     number <- scrap[grep("facettes__nombre", scrap)] %>%
#       str_extract_all("[0-9]", simplify=TRUE) %>%
#       paste0(collapse="") %>%
#       as.numeric %>%
#       c(number, .)
#   }
#   liste.pays[[paste0("articles", annee)]]=number
# }
# end.time <- Sys.time()
# # cette etape est longue (>1H). On recupere a la place le resultat precharge
load(file="liste.pays.figaro.Rdata")


# # web scrapping pour le site de LIBERATION
# # attention le nombre de resultats bloque a 1000 donc faut faire pour chaque mois... helas
# # je le fais pour 2015 dans un premier temps
# # attention faut remplecar les espaces des noms de pays par un '+'
# url.main <- "http://www.liberation.fr/recherche/?sort=-publication_date_time&q="
# for ( annee in 2007:2016){
#   for (key in liste.pays$pays) {
#     number=numeric(0)
#     for (month in 1:12){
#       scrap <- paste0(url.main, gsub(" ", "+", key), "&period=custom&period_start_day=1&period_start_month=", month, "&period_start_year=", annee, "&period_end_day=31&period_end_month=", month, "&period_end_year=", annee, "&editorial_source=&paper_channel=") %>% readLines
#       number <- scrap[grep(" rÃ©sultat", scrap)] %>% 
#         str_extract_all("[0-9]", simplify=TRUE) %>% 
#         paste0(collapse="") %>% 
#         as.numeric %>%
#         c(number, .)
#     }
#     sum.year=sum(number)
#   }
#   liste.pays[[paste0("articles", annee)]]=number
# }
load(file="liste.pays.libe.Rdata")

liste.pays <- liste.pays.figaro

# 3. Pretraitement
pass %<>% separate(unit.tra_meas.airp_pr.time, c("type1", "type2", "dest"), sep=",") %>% 
  filter(type2=="PAS_CRD") %>%
  separate(dest, c("pays.dep", "airp.dep", "pays.arr", "airp.arr"), sep="_") %>%
  select_(.dots=c("pays.arr", "airp.arr", paste0("X", 2007:2015))) %>%
  full_join(convert, by=c("pays.arr"="code2")) %>% 
  group_by(pays.arr, pays, code3) %>% 
  summarise_each(funs(sum.), contains("X")) %>%
  filter(!is.na(code3)) %>% 
  rename(code=code3) %>% ungroup
# malheureusement de nombreux pays ne sont pas repertories ou manquants (la moitie)
pass['zero.check'] <- rowSums(pass %>% select(contains("X")))
#pass %<>% filter(zero.check!=0) %>% select(-zero.check)

# rajout de la donnee passagers on met en NA les valeurs 0
liste.pays %<>% left_join(pass %>% select(-pays), by="code")

# retrait de 2 pays : france evidemment et Dominique qui a un nom trop courant (trop)
liste.pays %<>% filter(!pays %in% c("france", "dominique", "union europeenne", "mali", "georgie"))
liste.pays.figaro %<>% filter(!pays %in% c("france", "dominique", "union europeenne", "mali", "georgie"))
# etrangement quand on rajoute un "i" à la fin d'une recherche ca cherche le mot sans le "i" aussi..
# du coup le mot "mal" est très présent! il faut donc enlever mali partout... 
# et aussi bizarre pour georgie accepte george...
liste.pays.libe   %<>% filter(!pays %in% c("france", "dominique", "union europeenne", "mali", "georgie"))

# traitement des valeurs d'articles manquantes. on traite par interpolation lineaire sur pour le pays
# on doit donc transposer les donnees au prealable
liste.pays[, 9:18] %<>% t %>% na.approx(method="linear") %>% t
# pas d'interpolation des passagers car trop de donnnees manquante... :-(

# on rajoute artificiellement 1 aux articles pour eviter les infinis lors du passage au log. Idem passagers
liste.pays[, 9:18]  %<>% '+'(1) 
liste.pays[, 20:28] %<>% '+'(1) 

# ajout de donnees distance entre capitales
liste.pays %<>% left_join(dist, by="code")

# Creation de variables en LOG naturel
liste.pays %<>% mutate(Lpop     = log(pop),
                      Ldist     = log(distcap),
                      Lpass     = log(X2015),
                      Larticles = log(articles2015))

# jointure des données continent
liste.pays <- continent %>% 
  mutate(pays=trim.leading(pays)) %>%
  select(-is.autre) %>%
  melt(variable.name="continent") %>% 
  filter(value==1) %>% 
  select(-value) %>%
  left_join(liste.pays, ., by="pays")

# ajout d'une variable modifiée distsq
liste.pays$distsq=sqrt(liste.pays$distcap)

# calcul de coordonnees d'ellipses
ell.pop <- liste.pays %>% 
  filter(continent %in% c("is.europe", "is.asie", "is.afrique", "is.moyen.orient")) %>%
  select(continent, Lpop, Larticles) %>% 
  filter(!is.infinite(Larticles)) %>%
  coord.ellipse(npoint=200, level.conf=.5) %>% 
  as.data.frame
ell.pass <- liste.pays %>% 
  #filter(continent %in% c("is.europe", "is.asie", "is.afrique", "is.moyen.orient")) %>%
  select(continent, Lpass, Larticles) %>% 
  filter(Lpass!=0) %>%
  coord.ellipse(npoint=200, level.conf=.5) %>% 
  as.data.frame
ell.dist <- liste.pays %>% 
  #filter(continent %in% c("is.europe", "is.asie", "is.afrique", "is.moyen.orient")) %>%
  select(continent, distsq, Larticles) %>% 
  filter(distsq!=0) %>% filter(!is.infinite(Larticles)) %>%
  coord.ellipse(npoint=200, level.conf=.5) %>% 
  as.data.frame
