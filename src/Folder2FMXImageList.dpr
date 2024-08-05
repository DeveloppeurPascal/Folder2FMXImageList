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
/// File last update : 2024-06-30T09:19:08.043+02:00
/// Signature : 31ca93790d8aebc6eb9584b3026f3e0b403290b2
/// ***************************************************************************
/// </summary>

program Folder2FMXImageList;

{$R *.dres}

uses
  // System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
