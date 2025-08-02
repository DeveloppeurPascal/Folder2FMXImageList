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
  File last update : 2025-08-02T18:15:38.363+02:00
  Signature : 92d41c82445699dad5bd79611f3b1f733570b063
  ***************************************************************************
*)

unit UDmAdobestock40061010;

// ****************************************
// * Images from folder :
// * C:\Dev\Folder2FMXImageList\assets\sample-images\AdobeStock_40061010
// ****************************************
//
// This file contains a TDataModule with a
// TImageList to use in a FireMonkey project.
//
// ****************************************
// File generator : Folder To FMX Image List v2.0
// Website : https://folder2fmximagelist.olfsoftware.fr
// Generation date : 2025-08-02T18:15:38.305Z
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
  TDmAdobestock40061010 = class(TDataModule)
    ImageList: TImageList;
  private
  public
  end;

var
  DmAdobestock40061010: TDmAdobestock40061010;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
