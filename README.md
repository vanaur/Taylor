# Taylor

![Brook Taylor](https://alchetron.com/cdn/brook-taylor-0fb22d57-1736-41c4-b83e-77f827575c3-resize-750.jpg)

## Qu'est-ce que Taylor ?
Taylor est un logiciel vraiment simple d'usage de *calcul d'erreurs*. En science naturelles, lorsqu'on effectue une mesure, il lui est toujours associée une erreur, relative à la précision de l'appareil utilisé (règle graduée, thermomètre, balance, horloge, ...) Cette erreur nous informe sur le degré d'exaditude de la mesure.

Il arrive souvent que les quantitées mesurées soient utilisées dans des formules physiques qui établissent des liens entre divers grandeurs. Néanmoins, ne pas prendre en compte les erreurs de mesures lors de l'application de ces formules à nos données revient à ignorer que ces dernières sont issues de *mesures expérimentales*. Il convient donc de prendre en compte la manière dont les erreurs se propagent à travers l'usage des équations. Ce domaine des *statistiques* porte le nom de ["propagation de l'inertidude"](https://en.wikipedia.org/wiki/Propagation_of_uncertainty).

Il est très facile de calculer soit même la propagation des erreurs au travers d'une formule, il suffit de calculer les dérivées partielles de la fonction, de multiplier chacune d'elle avec l'erreur associée à la différentielle et enfin de substituer les variables par les données mesurées :

![](http://1plus1font3.com/Metrologie/Niveau4/Medias/propvar.jpg)

Cependant, lorsque l'on possède des dixaines de mesures (voir des centaines) et que l'on doit recalculer Taylor (c'est le nom du procédé) pour *chaqune* d'entre elles, cela peut devenir extrêmement fastidieux et prendre beaucoup de temps.

Tel est donc le but de ce logiciel : gagner du temps en laissant ce dernier calculer Taylor à notre place. Tout ce que nous avons à lui fournir, ce sont les données mesurées, l'erreur associée à chaque variable et, bien sûr, la relation physique. Taylor (le logiciel cette fois-ci) s'empressera de dériver et de calculer l'erreur sur la formule donnée associée à chaque mesure. Autant dire que le gain de temps est conséquent, de plus, contrairement à nous, pauvres humains, il ne fait aucune erreur de calcul ou de dérivée et ressort un résultat propre et directement sous forme utile.

Voici un exemple de sortie pour des données fictives :

![](https://media.discordapp.net/attachments/526499197529227296/909506261094133800/unknown.png)

## Comment utiliser Taylor ?
Le logiciel s'utilise comme un langage de programmation, il suffit de lui donner un fichier contenant les informations utiles et il se charge du reste. Voici un exemple d'utilisation :

```rb
Function: T = 2*pi * sqrt(L/g)  # On utilise ici la formule de la période d'un pendule simple
Constant: g = 9.81 # On défini une constante, ici la constante de l'intensité du champs gravitationnel à la surface de la Terre

# Nous n'avons pas besoin de définir Pi comme constante, il s'agit déjà d'une constante reconue par Taylor

Error: L = 0.1 # On définit l'erreur de mesure de la longueur à 0.1 (on ne précise pas les unités, c'est à l'utilisateur de savoir ce qu'il manipule)

Measures: L = [5, 10, 15, 25, 30, 35, 40, 45, 50] # On a effectué 9 mesures sur la longueur de la corde
```

Indiquez ensuite ce fichier à Taylor, dans le terminal : `./Taylor fichier` (ou `Taylor.exe fichier` sur Windows).

Voici le résultat, Taylor l'affiche dans la console :

![](https://i.ibb.co/cbNR0Zp/dfdf.png)
