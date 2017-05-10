
###########################################
#    ANALYSE DES SUJETS DE PRESSE FRANCAISE
#    chargement des fonctions utiles
###########################################


# 1. Import des donnees
import = function(file, desktop=FALSE, ...) { # entre guillemets
  path <- file
  if (desktop) {
    path <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/", file)
  }
  return(read.csv2(path, sep=";", dec=",", header=TRUE, stringsAsFactors=FALSE, ...)) 
}

# 2. suppression d'espace au debut des champs de texte
trim.leading <- function (x)  sub("^\\s+", "", x)

# 3. raccourcir la combinaison select(matches()) ou select(contains())
Where = function(data, string) {data %>% select(matches(string))}  

# 4. gradients de couleur
colfunc <- colorRampPalette(c("yellow", "red"))

