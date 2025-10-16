# Folder to FMX Image List

[Cette page en français.](LISEZMOI.md)

Create a FireMonkey TImageList by importing all images from a folder tree.

This program can export a TImageList to the clipboard or a TDataModule for Delphi and C++Builder.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

## Using this software

Visit [the Folder to FMX Image List website](https://folder2fmximagelist.olfsoftware.fr) to download the compiled version, learn more about how it works, access videos and articles, find out about the different versions available and their features, contact user support...

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

## Talks and conferences

### Twitch

Follow my development streams of software, video games, mobile applications and websites on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr) mostly in French.

## Source code installation

To download this code repository, we recommend using "git", but you can also download a ZIP file directly from [its GitHub repository](https://github.com/DeveloppeurPascal/Folder2FMXImageList).

This project uses dependencies in the form of sub-modules. They will be absent from the ZIP file. You'll have to download them by hand.

* [DeveloppeurPascal/AboutDialog-Delphi-Component](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) must be installed in the ./lib-externes/AboutDialog-Delphi-Component subfolder.
* [DeveloppeurPascal/CilTseg4Delphi](https://github.com/DeveloppeurPascal/CilTseg4Delphi) must be installed in the ./lib-externes/CilTseg4Delphi subfolder.
* [DeveloppeurPascal/Delphi-FMXExtend-Library](https://github.com/DeveloppeurPascal/Delphi-FMXExtend-Library) must be installed in the ./lib-externes/Delphi-FMXExtend-Library subfolder.
* [DeveloppeurPascal/FMX-Styles-Utils](https://github.com/DeveloppeurPascal/FMX-Styles-Utils) must be installed in the ./lib-externes/FMX-Styles-Utils subfolder.
* [DeveloppeurPascal/FMX-Tools-Starter-Kit](https://github.com/DeveloppeurPascal/FMX-Tools-Starter-Kit) must be installed in the ./lib-externes/FMX-Tools-Starter-Kit subfolder.
* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) must be installed in the ./lib-externes/librairies subfolder.

## Documentation and support

I use comments in [XMLDOC](https://docwiki.embarcadero.com/RADStudio/en/XML_Documentation_Comments) format in Delphi to document my projects. They are recognized by Help Insight, which offers real-time input help in the code editor.

I regularly use the [DocInsight](https://devjetsoftware.com/products/documentation-insight/) tool to enter them and check their formatting.

Documentation is exported in HTML by [DocInsight](https://devjetsoftware.com/products/documentation-insight/) or [PasDoc](https://pasdoc.github.io) to the /docs folder of the repository. You can also [access it online](https://developpeurpascal.github.io/Folder2FMXImageList) through the hosting offered by GitHub Pages.

Further information (tutorials, articles, videos, FAQ, talks and links) can be found on [the project website](https://folder2fmximagelist.olfsoftware.fr) or [the project devlog](https://developpeur-pascal.fr/folder2fmximagelist.html).

If you need explanations or help in understanding or using parts of this project in yours, please [contact me](https://developpeur-pascal.fr/nous-contacter.php). I can either direct you to an online resource, or offer you assistance in the form of a paid or free service, depending on the case. You can also contact me at a conference or during an online presentation.

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later](https://choosealicense.com/licenses/agpl-3.0/) license.

You are free to use the contents of this code repository anywhere provided :
* you mention it in your projects
* distribute the modifications made to the files provided in this AGPL-licensed project (leaving the original copyright notices (author, link to this repository, license) must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs (especially for a commercial project) I also offer [classic licenses for developers and companies](https://folder2fmximagelist.olfsoftware.fr).

Some elements included in this repository may depend on third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

The source codes of this code repository as well as any compiled version are provided “as is” without warranty of any kind.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/Folder2FMXImageList) and [open a new issue](https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain this project and all others.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [in French](https://ko-fi.com/patrick_premartin_fr) or [in English](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)

You can buy an end user license for [my softwares](https://lic.olfsoftware.fr/products.php?lng=en) and [my video games](https://lic.gamolf.fr/products.php?lng=en) or [a developer license for my libraries](https://lic.developpeur-pascal.fr/products.php?lng=en) if you use them in your projects.

I'm also available as a service provider to help you use this or other projects, such as software development, mobile applications and websites. [Contact me](https://vasur.fr/about) to discuss.
