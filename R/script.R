
##############################################
#    ANALYSE DES SUJETS DE PRESSE FRANCAISE
#    exemple du figaro et de liberation
##############################################

# auteur: marc.agenis@gmail.com
# date de dernière mise à jour: 10/05/2017
# version du logiciel: R 3.2.3

# preparation
mydir <- "D:/Mes Documents/Documents/dossier personnel/ACTION/upr/articles figaro/R"
setwd(mydir)
source("mes_fonctions_utiles.R") # temporaire
source("script_packages.R")
source("script_functions.R")
source("script_data.R")

# 1. analyse de l'evolution du nombre d'articles mentionnant un pays etrangers
liste.pays.figaro %>% Where("articles2") %>% colSums(na.rm=TRUE) %>% plot.ts
liste.pays.libe   %>% Where("articles2") %>% colSums(na.rm=TRUE) %>% plot.ts
liste.pays.figaro[, 9:18] %>% t %>% matplot(type="l")
liste.pays.libe[, 9:18]   %>% t %>% matplot(type="l")
# combiner les deux
temp1=colSums(liste.pays.figaro[,9:18], na.rm=TRUE) %>% data.frame %>% add_rownames %>% setNames(c("variable", "value")) %>% mutate("pays"="zzz", ".ID"=999) %>% data.frame
g3 <- ggparcoord(liste.pays.figaro, columns=9:18, groupColumn=1, scale="globalminmax") + 
  geom_line(data=temp1 %>% mutate(value=value/20), color="black", size=3, alpha=0.4) +
  theme(legend.position="none") + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Le Figaro")
temp2=colSums(liste.pays.libe[,9:18], na.rm=TRUE) %>% data.frame %>% add_rownames %>% setNames(c("variable", "value")) %>% mutate("pays"="zzz", ".ID"=999) %>% data.frame
g4 <- ggparcoord(liste.pays.libe, columns=9:18, groupColumn=1, scale="globalminmax") + 
  geom_line(data=temp2 %>% mutate(value=value/20), color="black", size=3, alpha=0.4) +
  theme(legend.position="none") + theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("Libération")
grid.arrange(g3, g4, ncol=2)

# histogramme (a colorier ensuite sous paint X-)
nb=15
g1 <- liste.pays %>% mutate(total=rowSums(liste.pays.figaro[, 9:18])) %>% top_n(nb, total) %>%
  ggplot(.) + aes(x=reorder(pays, total), y=total) + geom_bar(stat="identity") + 
  coord_flip() + ylab("nombre articles sur 10 ans") + xlab("pays") + ggtitle("Le Figaro") + theme(legend.position="none")
g2 <- liste.pays %>% mutate(total=rowSums(liste.pays.libe[, 9:18])) %>% top_n(nb, total) %>%
  ggplot(.) + aes(x=reorder(pays, total), y=total) + geom_bar(stat="identity") + 
  coord_flip() + ylab("nombre articles sur 10 ans") + xlab("pays") + ggtitle("Libération") + theme(legend.position="none")
grid.arrange(g1, g2, ncol=2)


# 2. Analyse quantitative de l'inegalite de citation des pays par indice de Gini
# rappel: indice=0=egalite parfaite indice=1=inegalite parfaite
sapply(liste.pays.figaro %>% Where("articles2"), function(x) ineq(x, type="Gini")) %T>% plot(type="b") %>% print
sapply(liste.pays.libe   %>% Where("articles2"), function(x) ineq(x, type="Gini")) %T>% lines(type="b", col="red") %>% print
# résumés statistiques
liste.pays.libe %>% select(articles2016)    %>% sumstats
liste.pays.libe$articles2016                %>% quantile %>% sign3
liste.pays.figaro %>% select(articles2016)  %>% sumstats
liste.pays.figaro$articles2016              %>% quantile %>% sign3
# les deux indices de Gini sont étonemment convergents! impressionnant.
palette = colorRampPalette(c("yellow", "red"))(16-7+1); par(mfrow=c(1,1))
plot(Lc(liste.pays.figaro[, 9]),col=palette[1], lwd=3, xlab="quantile nb articles", ylab="% du nb total d'articles", main="Le Figaro", cex=1)
for (i in 1:9){  lines(Lc(liste.pays.figaro[, 9+i]),col=palette[i], lwd=3)  }
plot(Lc(liste.pays.libe[, 9]),col=palette[10], lwd=3, xlab="quantile nb articles", ylab="% du nb total d'articles", main="Libération", cex=1)
for (i in 1:9){  lines(Lc(liste.pays.libe[, 9+i]),col=palette[i], lwd=3)  }


# 3. MODELISATION
#################

# constat de non normalite des donnees
fit0 <- lm(articles2015~pop, data=liste.pays)
lmplots(fit0)
e1071::skewness(fit0$residuals)
# box-cox transformation? bof
MASS::boxcox(articles2016~pop, data=liste.pays) # meilleur 0.1
lmplots(lm(articles2016^0.1~pop, data=liste.pays))

# analyse graphique sur passagers avec col continents
# la distribution est assez proche, rien de vraiment particulier...
# le viet nam très bas en pass et en pop
liste.pays %>% filter(Lpass!=0) %>% ggplot(.) + 
  stat_smooth(method = "lm", se=TRUE, alpha=0.1) +
  aes(x=Lpass, y=Larticles) + 
  #geom_text(aes(label=pays, color=continent), size=3)+
  geom_point(aes(col=continent), size=5, alpha=.3) +
  geom_point(data=ell.pass, aes(x=res.Lpass, y=res.Larticles, color=res.continent), alpha=.6) +
  scale_color_brewer(palette="Paired") +
  xlab("Log du traffic aérien") + ylab("Log du nombre de citations") +
  coord_cartesian(ylim=c(0,10))
# analyse graphique sur POP, ou population avec col continents
liste.pays %>% ggplot(.) + 
  stat_smooth(method = "lm", se=TRUE, alpha=0.2) +
  aes(x=Lpop, y=Larticles) + 
  #geom_text(aes(label=pays), col="grey", size=3)+
  geom_point(aes(col=continent), size=5, alpha=.3) +
  geom_point(data=ell.pop, aes(x=res.Lpop, y=res.Larticles, color=res.continent), alpha=.6) +
  scale_color_brewer(palette="Paired") +
  xlab("Log de la population") + ylab("Log du nombre de citations") +
  coord_cartesian(ylim=c(0,10))
# reste la distance:
liste.pays %>% ggplot(.) + 
  stat_smooth(method = "lm", se=TRUE, alpha=0.1) +
  aes(x=distsq, y=Larticles) + 
  #geom_text(aes(label=pays), col="grey", size=3)+
  geom_point(aes(col=continent), size=5, alpha=.3) +
  geom_point(data=ell.dist, aes(x=res.distsq, y=res.Larticles, color=res.continent), alpha=.6) +
  scale_color_brewer(palette="Paired") +
  xlab("distance entre capitales (^0.5)") + ylab("Log du nombre de citations") +
  coord_cartesian(ylim=c(0,10))
# corrélations entre chaque sur la base des pays ayant des valeurs de traffic aérien
with(liste.pays %>% filter(!is.na(Lpass)) %>% filter(Lpass!=0), cor(Larticles, Lpop, use="complete.obs"))
with(liste.pays %>% filter(!is.na(Lpass)) %>% filter(Lpass!=0), cor(Larticles, Ldist, use="complete.obs"))
with(liste.pays %>% filter(!is.na(Lpass)) %>% filter(Lpass!=0), cor(Larticles, Lpass, use="complete.obs"))

# modele complet
fi <- . %>% filter(!is.infinite(Larticles)) # fonction pour enlever les valeurs infinies après log
fit2 <- lm(Larticles~distsq+Lpop, data=liste.pays %>% fi); summary(fit2)
fit2s <- lm(scale(Larticles)~scale(distsq)+scale(Lpop), data=liste.pays %>% fi); summary(fit2s)
# modeles avec UE
fit3 <- lm(Larticles~distsq+Lpop+factor(is.ue), data=liste.pays %>% fi); summary(fit3)
fit3s <- lm(scale(Larticles)~scale(distsq)+scale(Lpop)+factor(is.ue), data=liste.pays %>% fi); summary(fit3s)
# modeles avec traffic aerien
fit4 <- lm(Larticles~distsq+Lpop+Lpass, data=liste.pays %>% fi); summary(fit4)
fit4s <- lm(scale(Larticles)~scale(distsq)+scale(Lpop)+scale(Lpass), data=liste.pays %>% fi); summary(fit4s)
# modeles avec traffic aerien et UE
fit5 <- lm(Larticles~distsq+Lpop+Lpass+is.ue, data=liste.pays %>% fi); summary(fit5)
# modeles avec francophonie
fit6 <- lm(Larticles~distsq+Lpop+factor(is.francophone), data=liste.pays %>% fi); summary(fit6)
fit7 <- lm(Larticles~distsq+Lpop+factor(is.francais.officiel), data=liste.pays %>% fi); summary(fit7)

# résidus du modèle FIT3
liste.pays2 <- liste.pays %>% fi
fit3 <- lm(Larticles~distsq+Lpop+factor(is.ue), data=liste.pays2, na.action = na.exclude)
liste.pays2 %<>% mutate(resid=rstudent(fit3), fitted=fitted(fit3)) 
liste.pays2 %>% ggplot(.) + 
  stat_smooth(method = "lm", se=TRUE, alpha=0.2) +
  aes(x=fitted, y=resid) + 
  geom_text(aes(label=pays, size=abs(resid)), color="black", alpha=0.7)+
  xlab("Nb de citations (log)") + ylab("Ecart au modèle (résidus studentisés)")

# si on étudie une autre année par exemple 2016
# attention il faut également recalculer les ellipses à la fin de script_data.R
liste.pays %<>% mutate(Larticles = log(articles2016))
liste.pays %<>% mutate(Larticles = log(articles2014))

# on remplace les donnees figaro par les donnees LIBE pour tester
liste.pays[, 9:18] <- liste.pays.figaro[, 9:18]
liste.pays[, 9:18] <- liste.pays.libe[, 9:18]
liste.pays %<>% mutate(Larticles = log(articles2015))
liste.pays %<>% mutate(Larticles = log(articles2016))
liste.pays %<>% mutate(Larticles = log(articles2014))
# reproduire à loisir les analyses ci-dessus en changeant l'année


############## FIN
