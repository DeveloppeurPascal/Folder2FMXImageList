# Folder 2 FMX Image List

Create a FireMonkey TImageList by importing all images in a folder.

## How to use

Install the program from [GitHub Release](https://github.com/DeveloppeurPascal/Folder2FMXImageList/releases) or download the source code and compile it in Delphi.

Specify the folder path were are your images.
Clic the button.

Paste the result to Delphi form editor on a TForm, TFrame or (better choice) TDataModule in a FireMonkey Project.

## How it works

The program searches all PNG files in a folder and its subfolders.

It creates a TImageList and adds the files as "Source". Each file is imported as it, with scale 1 if no scale is specified in its name.

After filling the TImageList.Source property, the program creates a Destination for each Source.

And then it serializes the TImageList as text and copy it to the clipboard. You only have to paste the result into Delphi.

## File name and scale

I based the file name split in [Adobe Illustrator](https://vasur.fr/illustrator) default export format. Look at screen captures to see what I mean.

xxx.png will create a source named "xxx" with scale 1
xxx@3x.png will create a source named "xxx" with scale 3
xxx@1.5x.png will create a source named "xxx" with scale 1,5

The program don't verify bitmap sizes. If the scale is not the good one, this will only impact the display of bitmaps in your programs.

## Possible errors

If you have the same image filename is multiple folders during the same import, it will raise an exception.

## Comments

If you use this program, don't forget to star this repository and think about sponsoring it if you can.

You can follow all my coding activites online, projects and courses at [Zone Abo](https://zone-abo.fr/les-contenus.php).

For any comment, new feature or bug, please [open an issue](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues).
