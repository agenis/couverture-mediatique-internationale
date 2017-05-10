liste.pays =structure(list(pays = c(" afghanistan", " afrique du sud", " albanie", 
                                    " algerie", " allemagne", " andorre", " angola", " antigua-et-barbuda", 
                                    " arabie saoudite", " argentine", " armenie", " australie", " autriche", 
                                    " azerbaidjan", " bahamas", " bahrein", " bangladesh", " barbade", 
                                    " belgique", " belize", " benin", " bhoutan", " bielorussie", 
                                    " birmanie", " bolivie", " bosnie-herzegovine", " botswana", 
                                    " bresil", " brunei", " bulgarie", " burkina faso", " burundi", 
                                    " cambodge", " cameroun", " canada", " cap-vert", " chili", " chine", 
                                    " chypre", " colombie", " comores", " coree du nord", " coree du sud", 
                                    " costa rica", " côte d'ivoire", " croatie", " cuba", " danemark", 
                                    " djibouti", " dominique", " egypte", " emirats arabes unis", 
                                    " equateur", " erythree", " espagne", " estonie", " etats-unis", 
                                    " ethiopie", " fidji", " finlande", " france", " gabon", " gambie", 
                                    " georgie", " ghana", " grece", " grenade", " guatemala", " guinee", 
                                    " guinee equatoriale", " guinee-bissau", " guyana", " haiti", 
                                    " honduras", " hongrie", " inde", " indonesie", " irak", " iran", 
                                    " irlande", " islande", " israel", " italie", " jamaique", " japon", 
                                    " jordanie", " kazakhstan", " kenya", " kirghizistan", " kiribati", 
                                    " kosovo", " koweit", " laos", " lesotho", " lettonie", " liban", 
                                    " liberia", " libye", " liechtenstein", " lituanie", " luxembourg", 
                                    " macedoine", " madagascar", " malaisie", " malawi", " maldives", 
                                    " mali", " malte", " maroc", " maurice", " mauritanie", " mexique", 
                                    " micronesie", " moldavie", " monaco", " mongolie", " montenegro", 
                                    " mozambique", " namibie", " nauru", " nepal", " nicaragua", 
                                    " niger", " nigeria", " niue", " norvege", " nouvelle-zelande", 
                                    " oman", " ossetie du sud", " ouganda", " ouzbekistan", " pakistan", 
                                    " palaos", " palestine", " panama", " papouasie-nouvelle-guinee", 
                                    " paraguay", " pays-bas", " perou", " philippines", " pologne", 
                                    " portugal", " qatar", " republique centrafricaine", " republique democratique du congo", 
                                    " republique dominicaine", " republique du congo", " republique tcheque", 
                                    " roumanie", " royaume-uni", " russie", " rwanda", " saint-christophe-et-nieves", 
                                    " sainte-lucie", " saint-marin", " saint-vincent-et-les grenadines", 
                                    " salomon", " salvador", " samoa", " sao tome-et-principe", " senegal", 
                                    " serbie", " seychelles", " sierra leone", " singapour", " slovaquie", 
                                    " slovenie", " somalie", " soudan", " soudan du sud", " sri lanka", 
                                    " suede", " suisse", " suriname", " swaziland", " syrie", " tadjikistan", 
                                    " taiwan", " tanzanie", " tchad", " thailande", " timor oriental", 
                                    " togo", " tonga", " trinite-et-tobago", " tunisie", " turkmenistan", 
                                    " turquie", " tuvalu", " ukraine", " union europeenne", " uruguay", 
                                    " vanuatu", " vatican", " venezuela", " viet nam", " yemen", 
                                    " zambie", " zimbabwe", "abkhazie", "chypre du nord", "iles cook", 
                                    "iles marshall", "sahara occidental")), class = "data.frame", row.names = c(NA, 
                                                                                                                -204L), .Names = "pays")
# web scrapping pour le site de liberation
# attention le nombre de resultats bloque a 1000 donc faut faire pour chaque mois... helas
# je le fais pour 2015 dans un premier temps
# attention faut remplecar les espaces des noms de pays par un '+'
library(dplyr)
library(stringr)
trim.leading=function (x)  sub("^\\s+", "", x)
liste.pays$pays <- trim.leading(liste.pays$pays)

url.main <- "http://www.liberation.fr/recherche/?sort=-publication_date_time&q="
for ( annee in 2007:20016){
  sum.year=numeric(0)
  for (key in (liste.pays$pays)) {
    number=numeric(0)
    for (month in 1:12){
      scrap <- paste0(url.main, gsub(" ", "+", key), "&period=custom&period_start_day=1&period_start_month=", month, "&period_start_year=", annee, "&period_end_day=31&period_end_month=", month, "&period_end_year=", annee, "&editorial_source=&paper_channel=") %>% readLines
      number <- scrap[grep(" résultat", scrap)][1] %>%
        str_extract_all("[0-9]", simplify=TRUE) %>%
        paste0(collapse="") %>%
        as.numeric %>%
        c(number, .)
    }
    sum.year=c(sum.year, sum(number, na.rm=TRUE))
  }
  liste.pays[[paste0("articles", annee)]]=sum.year
}
