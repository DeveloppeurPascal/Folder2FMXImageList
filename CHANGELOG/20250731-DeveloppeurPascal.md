# 20250731 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* création de plusieurs séries d'images comme fichiers de démo et ajout de quelques images dans /assets/sample-images
* réorganisation des modèles de fichiers pour ajouter la version C++Builder du module de données à exporter
* ajout d'une version C++Builder de module de données contenant une liste d'images et remplacement de son contenu par des tags pris en charge par le générateur de code
* remplacement des TEditButton de sélection de dossier par des boutons classiques (puisque les autres ne sont pas gérés par la TABulation)

* codage du sélecteur de dossier d'import
* codage du sélecteur de dossier d'export

* calcul des prérenseignements des champs (TextPrompt) liés au changement du dossier importé
* modification de la casse des identifiants de prérenseignements pour faire du PascalCase

* codage de l'export de la liste d'images vers le presse papier (et des méthodes permettant de créer l'instance de TImageList et de la remplir depuis les fichiers images sélectionnés)
* codage du remplissage optionnel des destinations depuis les sources de la liste d'images

* ajout des templates Pascal et C++Builder aux ressources du projet

* codage de l'export du module de données

* tests et corrections de la génération des fichiers sources Pascal et C++

* forçage du 'T' en premier caractère des noms de modules de données et génération forcée du nom de la varible associée en prenant ce qui se trouve après le 'T'

* retrait du paramètre permettant de ne pas exporter la déclaration de variable dans les modules de données (à voir si c'est demandé un jour par un utilisateur)

