# Folder to FMX Image List

[Cette page en français.](LISEZMOI.md)

Create a FireMonkey TImageList by importing all images in a folder.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

## Talks and conferences

### Twitch

Follow my development streams of software, video games, mobile applications and websites on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr) mostly in French.

## Using this software

This software is available in a directly installable or executable production version. It is distributed as shareware.

You can download and redistribute it free of charge, provided you do not modify its content (installer, program, additional files, etc.).

[Download program or installer](https://olfsoftware.lemonsqueezy.com/checkout/buy/14bc1e25-f5f0-47a2-b698-5a18f7043d8e)

If you use this software regularly and are satisfied with it, you are invited to purchase an end-user license. Purchasing a license will give you access to software updates, as well as enabling optional features.

[Buy a license from Gumroad](https://boutique.olfsoftware.fr/l/folder2fmximagelist)
[Buy a license from Lemon Squeezy](https://olfsoftware.lemonsqueezy.com/checkout/buy/648c0390-1f77-419e-9648-e4815de984a1)

You can also [visit the software website](https://folder2fmximagelist.olfsoftware.fr/) to find out more about how it works, access videos and articles, find out about the different versions available and their features, contact user support...

### How to use Folder2FMXImageList ?

* Specify the folder path were are your images.

* Choose your export method (clipboard or TDataModule unit).

* In case you choosed the clipboard, just paste the result to Delphi form editor on a TForm, TFrame or (better choice) TDataModule in a FireMonkey Project.

* In case you choosed TDataModule unit export, just include it to a FireMonkey project like you does with any other unit.

* You can drag & drop folders to the source directory path to export each of them as a TDataModule unit.

### What does Folder2FMXImageList ?

The program searches all PNG files in a folder and its subfolders.

It creates a TImageList and adds the files as "Source". Each file is imported as it, with scale 1 if no scale is specified in its name.

After filling the TImageList.Source property, the program creates a Destination for each Source image.

Then, depending on your export method, the program serialize the TImageList and copy it to the clipboard or generate a TDataModule unit and save it on disk in the images folder.

### File name and scale

I based the file name split on [Adobe Illustrator](https://vasur.fr/illustrator) default export format :

![Adobe Illustrator homothetic export sample](https://github.com/DeveloppeurPascal/Folder2FMXImageList/raw/main/screen-captures/04-FilesForHomotheticExport.png)

xxx.png will create a source named "xxx" with scale 1
xxx@3x.png will create a source named "xxx" with scale 3
xxx@1.5x.png will create a source named "xxx" with scale 1,5

The program don't verify bitmap sizes. If the scale is not the good one, this will impact the display of bitmaps in your programs.

### Possible errors

If you have the same image filename in multiple subfolders of the path you specified during the same import, it will raise an exception.

## Source code installation

To download this code repository, we recommend using "git", but you can also download a ZIP file directly from [its GitHub repository](https://github.com/DeveloppeurPascal/Folder2FMXImageList).

This project uses dependencies in the form of sub-modules. They will be absent from the ZIP file. You'll have to download them by hand.

* [DeveloppeurPascal/AboutDialog-Delphi-Component](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) must be installed in the ./lib-externes/AboutDialog-Delphi-Component subfolder.
* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) must be installed in the ./lib-externes/librairies subfolder.

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later license](https://choosealicense.com/licenses/agpl-3.0/).

You are generally free to use the contents of this code repository anywhere, provided that:
* you mention it in your projects
* distribute the modifications made to the files supplied in this project under the AGPL license (leaving the original copyright notices (author, link to this repository, license) which must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs, you can purchase the right to use this project under the [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) or a dedicated commercial license ([contact the author](https://developpeur-pascal.fr/nous-contacter.php) to explain your needs).

These source codes are provided as is, without warranty of any kind.

Certain elements included in this repository may be subject to third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/Folder2FMXImageList) and [open a new issue](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
