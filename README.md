# Openstreet-map---ocaml
						Explication du projet
						
BEN EL HADJ Ali						
DUGUAY Brice

Nous avons créé pour ce projet deux type d'enregistrement:
	-node:
		enregistre les données d'une balise "node" et contient
			-sa latitude en float
			-sa longitude en float
			-sa liste de tag en (string * string) list, par exemple: un element de cette liste ("highway","tertiary") pour une balise <tag k="highway", v="tertiary"/>
	-way:
		enregistre les données d'une balise "way" et contient
			-sa liste de reference de node en int list
			-sa liste de tag (meme idée que pour le type node)

Nous avons utiliser deux tables de hachage:
	-tableNode:
		pour stocker toutes les balises node et leurs informations
			-clé de la table = un numéro id (identifiant du node)
			-val de la table =  un element de type node qui represente une balise node d'identifiant égale à la clé de la table
	-tableWay:
		pour stocker toutes les balises way et leurs informations
			-clé de la table = id du way
			-val de la table = un element de type way qui represente la balise way à identifiant égale à la clef 

La fonction "rempliTable" récupere maxlon, maxlat, minlon et minlat et les stocke dans des variables, et remplit les deux tables de hachage.

On ouvre toujours une page de 1400 sur 800 et on utilise une fonction qui s'arrange pour faire le bon zoom (voir les fonctions "zoomh" et "zooml").

Les fonctions "getlatnodeconverti" et "getlonnodeconverti" prennent un numéro d'id d'un node et retourne la latitude ou la longitude (avec le zoom) de ce node 

Les 4 dernieres fonction calcule les latitude et longitute grace aux définitions suivantes:
	1° de latitude = (40000/360) km
	1° de longitude = [(4000/360) * cos (latitude)] km

Les autres fonctions sont des fonctions d'affichage.


Il y a quelque exemple de fichier osm
le projet requiert libxml-light-ocaml-dev
Lancement: ocaml projet.ml nomfic.osm

