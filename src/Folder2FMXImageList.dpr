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
/// File last update : 2024-08-06T20:16:04.000+02:00
/// Signature : 615a7e5ba0a6168e7c871aa0632462d4307bb270
/// ***************************************************************************
/// </summary>

program Folder2FMXImageList;

{$R *.dres}

uses
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  u_urlOpen in '..\lib-externes\librairies\src\u_urlOpen.pas',
  Olf.FMX.AboutDialog in '..\lib-externes\AboutDialog-Delphi-Component\src\Olf.FMX.AboutDialog.pas',
  Olf.FMX.AboutDialogForm in '..\lib-externes\AboutDialog-Delphi-Component\src\Olf.FMX.AboutDialogForm.pas' {OlfAboutDialogForm},
  uAboutBox in 'uAboutBox.pas' {AboutBox: TDataModule},
  uAboutDescriptionText in 'uAboutDescriptionText.pas',
  uAboutLicenseText in 'uAboutLicenseText.pas',
  uConsts in 'uConsts.pas',
  Olf.FMX.SelectDirectory in '..\lib-externes\Delphi-FMXExtend-Library\src\Olf.FMX.SelectDirectory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

end.
