# Folder to FMX Image List

[This page in English.](README.md)

Crée un TImageList pour projets FireMonkey en y important toutes les images d'un dossier.

Ce dépôt de code contient un projet développé en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

## Présentations et conférences

### Twitch

Suivez mes streams de développement de logiciels, jeux vidéo, applications mobiles et sites web sur [ma chaîne Twitch](https://www.twitch.tv/patrickpremartin) ou en rediffusion sur [Serial Streameur](https://serialstreameur.fr) la plupart du temps en français.

## Utiliser ce logiciel

Ce logiciel est disponible dans une version de production directement installable ou exécutable. Il est distribué en shareware.

Vous pouvez le télécharger et le rediffuser gratuitement à condition de ne pas en modifier le contenu (installeur, programme, fichiers annexes, ...).

[Télécharger le programme ou son installeur](https://olfsoftware.lemonsqueezy.com/checkout/buy/14bc1e25-f5f0-47a2-b698-5a18f7043d8e)

Si vous utilisez régulièrement ce logiciel et en êtes satisfait vous êtes invité à en acheter une licence d'utilisateur final. L'achat d'une licence vous donnera accès aux mises à jour du logiciel en plus d'activer d'évenuelles fonctionnalités optionnelles.

[Acheter une licence sur Gumroad](https://boutique.olfsoftware.fr/l/folder2fmximagelist)
[Acheter une licence sur Lemon Squeezy](https://olfsoftware.lemonsqueezy.com/checkout/buy/648c0390-1f77-419e-9648-e4815de984a1)

Vous pouvez aussi [consulter le site du logiciel](https://folder2fmximagelist.olfsoftware.fr/) pour en savoir plus sur son fonctionnement, accéder à des vidéos et articles, connaître les différentes versions disponibles et leurs fonctionnalités, contacter le support utilisateurs...

### Comment utiliser Folder2FMXImageList ?

* Spécifiez le chemin du dossier où se trouvent vos images.

* Choisissez votre méthode d'exportation (presse-papier ou unité TDataModule).

* Si vous avez choisi le presse-papier, collez simplement le résultat dans l'éditeur de formulaire Delphi sur un TForm, TFrame ou (mieux) TDataModule dans un projet FireMonkey.

* Si vous avez choisi l'exportation de l'unité TDataModule, il suffit de l'inclure dans un projet FireMonkey comme vous le feriez avec n'importe quelle autre unité.

* Vous pouvez glisser-déposer des dossiers dans le chemin du répertoire source pour exporter chacun d'entre eux en tant qu'unité TDataModule.

### Que fait Folder2FMXImageList ?

Le programme recherche tous les fichiers PNG dans un dossier et ses sous-dossiers.

Il crée une TImageList et ajoute les fichiers en tant que "Source". Chaque fichier est importé tel quel, avec l'échelle 1 si aucune échelle n'est spécifiée dans son nom.

Après avoir rempli la propriété TImageList.Source, le programme crée une Destination pour chaque image Source.

Ensuite, en fonction de votre méthode d'exportation, le programme sérialise la TImageList et la copie dans le presse-papiers ou génère une unité TDataModule et l'enregistre sur le disque dans le dossier images.

### Nom de fichier et échelle

J'ai basé la division du nom de fichier sur le format d'exportation par défaut d'[Adobe Illustrator](https://vasur.fr/illustrator) :

![Exemple d'exportation homothétique d'Adobe Illustrator](https://github.com/DeveloppeurPascal/Folder2FMXImageList/raw/main/screen-captures/04-FilesForHomotheticExport.png)

xxx.png créera une source nommée "xxx" à l'échelle 1
xxx@3x.png créera une source nommée "xxx" à l'échelle 3
xxx@1.5x.png créera une source nommée "xxx" avec une échelle de 1,5

Le programme ne vérifie pas les tailles des bitmaps. Si l'échelle n'est pas la bonne, cela aura un impact sur l'affichage des bitmaps dans vos programmes.

### Erreurs possibles

Si vous avez le même nom de fichier image dans plusieurs sous-dossiers du chemin que vous avez spécifié lors de la même importation, cela soulèvera une exception.

## Installation des codes sources

Pour télécharger ce dépôt de code il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt GitHub](https://github.com/DeveloppeurPascal/Folder2FMXImageList).

Ce projet utilise des dépendances sous forme de sous modules. Ils seront absents du fichier ZIP. Vous devrez les télécharger à la main.

* [DeveloppeurPascal/AboutDialog-Delphi-Component](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) doit être installé dans le sous dossier ./lib-externes/AboutDialog-Delphi-Component
* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) doit être installé dans le sous dossier ./lib-externes/librairies

## Compatibilité

En tant que [MVP Embarcadero](https://www.embarcadero.com/resources/partners/mvp-directory) je bénéficie dès qu'elles sortent des dernières versions de [Delphi](https://www.embarcadero.com/products/delphi) et [C++ Builder](https://www.embarcadero.com/products/cbuilder) dans [RAD Studio](https://www.embarcadero.com/products/rad-studio). C'est donc dans ces versions que je travaille.

Normalement mes librairies et composants doivent aussi fonctionner au moins sur la version en cours de [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

Aucune garantie de compatibilité avec des versions antérieures n'est fournie même si je m'efforce de faire du code propre et ne pas trop utiliser les nouvelles façons d'écrire dedans (type inference, inline var et multilines strings).

Si vous détectez des anomalies sur des versions antérieures n'hésitez pas à [les rapporter](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues) pour que je teste et tente de corriger ou fournir un contournement.

## Licence d'utilisation de ce dépôt de code et de son contenu

Ces codes sources sont distribués sous licence [AGPL 3.0 ou ultérieure](https://choosealicense.com/licenses/agpl-3.0/).

Vous êtes globalement libre d'utiliser le contenu de ce dépôt de code n'importe où à condition :
* d'en faire mention dans vos projets
* de diffuser les modifications apportées aux fichiers fournis dans ce projet sous licence AGPL (en y laissant les mentions de copyright d'origine (auteur, lien vers ce dépôt, licence) obligatoirement complétées par les vôtres)
* de diffuser les codes sources de vos créations sous licence AGPL

Si cette licence ne convient pas à vos besoins vous pouvez acheter un droit d'utilisation de ce projet sous la licence [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) ou une licence commerciale dédiée ([contactez l'auteur](https://developpeur-pascal.fr/nous-contacter.php) pour discuter de vos besoins).

Ces codes sources sont fournis en l'état sans garantie d'aucune sorte.

Certains éléments inclus dans ce dépôt peuvent dépendre de droits d'utilisation de tiers (images, sons, ...). Ils ne sont pas réutilisables dans vos projets sauf mention contraire.

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur GitHub](https://github.com/DeveloppeurPascal/Folder2FMXImageList) et [d'ouvrir un ticket](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Supportez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir le projet (codes sources et binaires).

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

ou si vous parlez français vous pouvez [vous abonner à Zone Abo](https://zone-abo.fr/nos-abonnements.php) sur une base mensuelle ou annuelle et avoir en plus accès à de nombreuses ressources en ligne (vidéos et articles).
