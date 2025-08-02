(* C2PP
  ***************************************************************************

  Folder to FMX Image List

  Copyright 2022-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Create a FireMonkey TImageList by importing all images from a folder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://folder2fmximagelist.olfsoftware.fr/

  Project site :
  https://github.com/DeveloppeurPascal/Folder2FMXImageList

  ***************************************************************************
  File last update : 2025-08-02T18:15:38.046+02:00
  Signature : b92d030ba087322696a3746ca4ba4357a285b310
  ***************************************************************************
*)

unit UDmAdobestock119308227;

// ****************************************
// * Images from folder :
// * C:\Dev\Folder2FMXImageList\assets\sample-images\AdobeStock_119308227
// ****************************************
//
// This file contains a TDataModule with a
// TImageList to use in a FireMonkey project.
//
// ****************************************
// File generator : Folder To FMX Image List v2.0
// Website : https://folder2fmximagelist.olfsoftware.fr
// Generation date : 2025-08-02T18:15:37.966Z
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  FMX.ImgList;

type
  TDmAdobestock119308227 = class(TDataModule)
    ImageList: TImageList;
  private
  public
  end;

var
  DmAdobestock119308227: TDmAdobestock119308227;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
