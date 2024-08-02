# 20220828 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* register icons for Linux platform in project options
* register icons for macOS 64 bits platform in project options
* register icons for macOS ARM 64 bits platform in project options
* màj infos versions Linux, Mac et Mac ARM
* passage des releases en version 1.1
* remove TPL files copy in user folder at startup of the program on Mac
* ajout d'infos de copyright au niveau de l'entête de l'unité du TDataModule généré
* ajout d'un tag %%datetime%% dans le template de l'unité du module de données généré remplacé par la date/heure de la génération
* ajout d'un filtrage des lettres utilisées en nom de TDataModule et de TImageList à partir du nom du dossier traité lors de l'import
* tri alphabétique des images générées en Destination de la liste des images sans tenir compte de l'ordre des images sources
* retrait du doublon de BOM UTF-8 provenant des templates stockés en ressources lors de l'export du TDataModule en tant qu'unité
* génération d'un fichier de correspondance entre les noms des images sources et leur indice dans la liste d'image finale (fait lors de l'export du TDataModule sous forme de fichier .lst)
* prise en charge du drag&drop de dossiers depuis le Finder du Mac ou l'explorateur de fichiers de Windows vers le champ de saisie du dossier à traiter pour 1 ou plusieurs dossiers) avec export automatique en mode TDataModule
* création de la release 1.1 avec [sources sur GitHub](https://github.com/DeveloppeurPascal/Folder2FMXImageList/releases/tag/1.1) et [programmes sur Gumroad](https://boutique.olfsoftware.fr/l/folder2fmximagelist)