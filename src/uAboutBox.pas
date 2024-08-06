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
/// File last update : 2024-08-06T19:57:30.000+02:00
/// Signature : 5f74ec5038c7e17ad7c328e0eace6d70e55ff6c8
/// ***************************************************************************
/// </summary>

unit uAboutBox;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  Olf.FMX.AboutDialog,
  Olf.FMX.AboutDialogForm,
  System.ImageList,
  FMX.ImgList;

type
  TAboutBox = class(TDataModule)
    ImageList1: TImageList;
    OlfAboutDialog1: TOlfAboutDialog;
    procedure OlfAboutDialog1URLClick(const AURL: string);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    class procedure Execute(const Show: boolean = true);
    class function Current: TAboutBox;
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  u_urlOpen,
  uAboutDescriptionText,
  uAboutLicenseText,
  uConsts;

{$R *.dfm}

var
  AboutBox: TAboutBox;

class function TAboutBox.Current: TAboutBox;
begin
  if not assigned(AboutBox) then
    AboutBox := TAboutBox.Create(nil);

  result := AboutBox;
end;

procedure TAboutBox.DataModuleCreate(Sender: TObject);
begin
  OlfAboutDialog1.VersionNumero := CVersionNumber;
  OlfAboutDialog1.VersionDate := CVersionDate;
end;

class procedure TAboutBox.Execute(const Show: boolean);
begin
  if not assigned(AboutBox) then
    AboutBox := TAboutBox.Create(nil);

  if Show then
    AboutBox.OlfAboutDialog1.Execute;
end;

function TAboutBox.OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin // TODO : traduire les textes
  if ATxtID = TOlfAboutDialogTxtID.LicenseText then
    result := CAboutLicenseEN
  else if ATxtID = TOlfAboutDialogTxtID.DescriptionText then
    result := CAboutDescriptionEN;
end;

procedure TAboutBox.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

initialization

AboutBox := nil;
tthread.ForceQueue(nil,
  procedure
  begin
    TAboutBox.Execute(false);
  end);

finalization

AboutBox.free;

end.
