/// <summary>
/// ***************************************************************************
///
/// Folder to FMX ImageList
///
/// Copyright 2022-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Create a FireMonkey TImageList by importing all images in a folder.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://folder2fmximagelist.olfsoftware.fr/
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Folder2FMXImageList
///
/// ***************************************************************************
/// File last update : 2024-08-06T19:59:32.000+02:00
/// Signature : 955a0debeb18ca6773536fff4d21606d7eb94628
/// ***************************************************************************
/// </summary>

unit uAboutDescriptionText;

interface

const
  CAboutDescriptionEN = '''
Folder2FMXImageList creates a FireMonkey TImageList by importing all images in a folder.

The program searches all PNG files in the given folder and its subfolders.

It creates a TImageList and adds the files as "Source". Each file is imported as it, with scale 1 if no scale is specified in its name.

After filling the TImageList.Source property, the program creates a Destination for each Source image.

Then, depending on your export method, the program serialize the TImageList and copy it to the clipboard or generate a TDataModule unit and save it on disk in the images folder.

*****************
* Publisher info

This application was developed by Patrick Prémartin in Delphi.

It is published by OLF SOFTWARE, a company registered in Paris (France) under the reference 439521725.

****************
* Personal data

This program is autonomous in its current version. It does not depend on the Internet and communicates nothing to the outside world.

We have no knowledge of what you do with it.

No information about you is transmitted to us or to any third party.

We use no cookies, no tracking, no stats on your use of the application.

***************
* User support

If you have any questions or require additional functionality, please leave us a message on the application''s website or on its code repository.

To find out more, visit https://folder2fmximagelist.olfsoftware.fr/
''';

implementation

end.
