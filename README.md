# Folder 2 FMX Image List

Create a FireMonkey TImageList by importing all images in a folder.

## How to use Folder2FMXImageList ?

* [Buy last release of the program](https://boutique.olfsoftware.fr/l/folder2fmximagelist), compile one from [last source files](https://github.com/DeveloppeurPascal/Folder2FMXImageList/) or compile one from the [releases](https://github.com/DeveloppeurPascal/Folder2FMXImageList/releases).

* Specify the folder path were are your images.

* Choose your export method (clipboard or TDataModule unit).

* In case you choosed the clipboard, just paste the result to Delphi form editor on a TForm, TFrame or (better choice) TDataModule in a FireMonkey Project.

* In case you choosed TDataModule unit export, just include it to a FireMonkey project like you does with any other unit.

## What does Folder2FMXImageList ?

The program searches all PNG files in a folder and its subfolders.

It creates a TImageList and adds the files as "Source". Each file is imported as it, with scale 1 if no scale is specified in its name.

After filling the TImageList.Source property, the program creates a Destination for each Source image.

Then, depending on your export method, the program serialize the TImageList and copy it to the clipboard or generate a TDataModule unit and save it on disk in the images folder.

## File name and scale

I based the file name split in [Adobe Illustrator](https://vasur.fr/illustrator) default export format :

![Adobe Illustrator homothetic export sample](https://github.com/DeveloppeurPascal/Folder2FMXImageList/raw/main/screen-captures/04-FilesForHomotheticExport.png)

xxx.png will create a source named "xxx" with scale 1
xxx@3x.png will create a source named "xxx" with scale 3
xxx@1.5x.png will create a source named "xxx" with scale 1,5

The program don't verify bitmap sizes. If the scale is not the good one, this will impact the display of bitmaps in your programs.

## Possible errors

If you have the same image filename in multiple subfolders of the path you specified during the same import, it will raise an exception.

## Comments

If you use this program, please add a star to [its repository](https://github.com/DeveloppeurPascal/Folder2FMXImageList) and think about sponsoring me on [Zone Abo](https://zone-abo.fr/nos-abonnements.php), [Patreon](https://www.patreon.com/patrickpremartin), [Liberapay](https://fr.liberapay.com/PatrickPremartin/) or by [buying a license](https://boutique.olfsoftware.fr/l/folder2fmximagelist).

You can follow all my coding activites online, projects and courses at [Zone Abo](https://zone-abo.fr/les-contenus.php).

For any comment, new feature or bug, please [open an issue](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues).
