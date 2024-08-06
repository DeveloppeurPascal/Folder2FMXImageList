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
/// File last update : 2024-08-06T20:38:08.000+02:00
/// Signature : 1efa9bd9b35daef3885001f930a9f75f5c0ccb45
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  System.ImageList,
  FMX.ImgList,
  FMX.Layouts,
  Olf.FMX.SelectDirectory;

type
  TfrmMain = class(TForm)
    lblImportFolder: TLabel;
    edtImportFolder: TEdit;
    btnSaveToClipboard: TButton;
    ZoneBoutons: TGridPanelLayout;
    btnSaveAsTDataModule: TButton;
    ToolBar1: TToolBar;
    btnQuit: TButton;
    btnAbout: TButton;
    btnImportFolder: TEllipsesEditButton;
    OlfSelectDirectoryDialog1: TOlfSelectDirectoryDialog;
    procedure btnSaveToClipboardClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure edtImportFolderEnter(Sender: TObject);
    procedure btnSaveAsTDataModuleClick(Sender: TObject);
    procedure edtImportFolderDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure edtImportFolderDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure btnAboutClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnImportFolderClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure ImportPNGFiles(FolderName: string; ImgList: TImageList;
      Recursive: boolean = true);
    procedure GenerateDestinationFromSource(ImgList: TImageList);
    procedure ValidatePathField;
    function getTemplateFromResource(TemplateName: string): string;
    function FilterPascalIdentifier(NameToFilter: string): string;
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  System.IOUtils,
  FMX.Platform,
  uAboutBox;

/// <summary>
/// ComponentToStringProc sérialise un composant pour l'obtenir sous forme de
/// chaîne de caractères comme dans les sources des écrans (fichiers fmx/dfm).
/// </summary>
/// http://docwiki.embarcadero.com/CodeExamples/en/ComponentToString_(Delphi)
function ComponentToStringProc(Component: TComponent): string;
var
  BinStream: TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  TAboutBox.Execute;
end;

procedure TfrmMain.btnImportFolderClick(Sender: TObject);
begin
  if OlfSelectDirectoryDialog1.Directory.IsEmpty then
    if (not edtImportFolder.Text.IsEmpty) and
      TDirectory.Exists(edtImportFolder.Text) then
      OlfSelectDirectoryDialog1.Directory := edtImportFolder.Text
    else
      OlfSelectDirectoryDialog1.Directory := tpath.GetDocumentsPath;

  if OlfSelectDirectoryDialog1.Execute and
    TDirectory.Exists(OlfSelectDirectoryDialog1.Directory) then
    edtImportFolder.Text := OlfSelectDirectoryDialog1.Directory;
end;

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.btnSaveAsTDataModuleClick(Sender: TObject);
var
  ImgList: TImageList;
  UnitName: string;
  DFMFilePath: string;
  PASFilePath: string;
  TXTFilePath: string;
  UnitDFMSource: string;
  UnitPASSource: string;
  DMName: string;
  s: string;
  i: integer;
begin
  ValidatePathField;

  ZoneBoutons.Enabled := false;

  try
    DMName := 'dm' + FilterPascalIdentifier
      (tpath.GetFileNameWithoutExtension(edtImportFolder.Text));
  except
    DMName := 'dm';
  end;

  ImgList := TImageList.Create(self);
  try
    ImgList.Name := 'ImageList';

    ImportPNGFiles(edtImportFolder.Text, ImgList);

    GenerateDestinationFromSource(ImgList);

    UnitName := 'u' + DMName;
    try
      DFMFilePath := tpath.Combine(edtImportFolder.Text, UnitName + '.dfm');
      UnitDFMSource := getTemplateFromResource('DM_dfm_template');
      tfile.WriteAllText(DFMFilePath, UnitDFMSource.Replace('%%UnitName%%',
        UnitName).Replace('%%DMName%%', DMName).Replace('%%ImageListName%%',
        ImgList.Name).Replace('%%ImageList%%', ComponentToStringProc(ImgList)));
      try
        PASFilePath := tpath.Combine(edtImportFolder.Text, UnitName + '.pas');
        UnitPASSource := getTemplateFromResource('DM_pas_template');
        tfile.WriteAllText(PASFilePath, UnitPASSource.Replace('%%UnitName%%',
          UnitName).Replace('%%DMName%%', DMName).Replace('%%ImageListName%%',
          ImgList.Name).Replace('%%datetime%%', DateToISO8601(now, true))
          .Replace('%%ImportFolder%%', edtImportFolder.Text)
          .Replace('%%AboutCaption%%',
          TAboutBox.Current.OlfAboutDialog1.GetMainFormCaption)
          .Replace('%%AboutURL%%', TAboutBox.Current.OlfAboutDialog1.URL));
        try
          TXTFilePath := tpath.Combine(edtImportFolder.Text, UnitName + '.lst');
          s := '';
          for i := 0 to ImgList.Destination.Count - 1 do
            s := s + ImgList.Destination[i].Layers[0].Name + Tabulator +
              i.ToString + CarriageReturn + LineFeed;
          tfile.WriteAllText(TXTFilePath, s);
          ShowMessage('Image list saved as unit ' + PASFilePath);
        except
          ShowMessage('Can''t save ' + TXTFilePath +
            ' but .pas and .dfm files are okay.');
        end;
      except
        ShowMessage('Can''t save ' + PASFilePath + ' but .dfm is okay.');
      end;
    except
      ShowMessage('Can''t save ' + DFMFilePath);
    end;
  finally
    ImgList.Free;
    ZoneBoutons.Enabled := true;
  end;
end;

procedure TfrmMain.btnSaveToClipboardClick(Sender: TObject);
var
  ImgList: TImageList;
  cb: IFMXClipboardService;
begin
  ValidatePathField;

  ZoneBoutons.Enabled := false;
  ImgList := TImageList.Create(self);
  try
    try
      ImgList.Name := FilterPascalIdentifier
        (tpath.GetFileNameWithoutExtension(edtImportFolder.Text));
    except
      ImgList.Name := 'ImageList1';
    end;

    ImportPNGFiles(edtImportFolder.Text, ImgList);

    GenerateDestinationFromSource(ImgList);

    if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
      iinterface(cb)) then
    begin
      cb.SetClipboard(ComponentToStringProc(ImgList));
      ShowMessage
        ('Paste the clipboard to Delphi form conceptor on a FMX TDataModule, TFrame or TForm.');
    end
    else
      raise exception.Create('Copy to clipboard failed.');
  finally
    ImgList.Free;
    ZoneBoutons.Enabled := true;
  end;
end;

procedure TfrmMain.edtImportFolderDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  i: integer;
begin
  for i := 0 to length(Data.files) - 1 do
    if TDirectory.Exists(Data.files[i]) then
    begin
      edtImportFolder.Text := Data.files[i];
      btnSaveAsTDataModuleClick(Sender);
    end;
  edtImportFolder.Text := '';
  edtImportFolder.SetFocus;
end;

procedure TfrmMain.edtImportFolderDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
var
  ok: boolean;
  i: integer;
begin
  ok := false;
  for i := 0 to length(Data.files) - 1 do
    if TDirectory.Exists(Data.files[i]) then
    begin
      ok := true;
      break;
    end;
  if ok then
    Operation := TDragOperation.Copy
  else
    Operation := TDragOperation.None;
end;

procedure TfrmMain.edtImportFolderEnter(Sender: TObject);
begin
  edtImportFolder.SelectAll;
end;

function TfrmMain.FilterPascalIdentifier(NameToFilter: string): string;
var
  i: integer;
  c: Char;
  ChiffresAutorises: boolean;
begin
  // pas de chiffre en premier caractère
  // que des lettres (majuscules, minuscules), des soulignes, des chiffres
  Result := '';
  ChiffresAutorises := false;
  for i := 0 to NameToFilter.length - 1 do
  begin
    c := NameToFilter.Chars[i];
    if CharInSet(c, ['a' .. 'z', 'A' .. 'Z', '_']) or
      (ChiffresAutorises and (CharInSet(c, ['0' .. '9']))) then
    begin
      Result := Result + c;
      if not ChiffresAutorises then
        ChiffresAutorises := true;
    end;
  end;
  if (Result.IsEmpty) then
    raise exception.Create
      ('Filename not compatible with a Pascal identifier !');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG}
  edtImportFolder.Text :=
    'C:\Users\olfso\Documents\Embarcadero\Studio\Projets\Folder2FMXImageList\TestImages';
{$ENDIF}
  edtImportFolder.SetFocus;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkescape then
  begin
    Key := 0;
    KeyChar := #0;
    close;
  end;
end;

procedure TfrmMain.GenerateDestinationFromSource(ImgList: TImageList);
var
  lst: TStringList;
  i: integer;
begin
  lst := TStringList.Create;
  try
    for i := 0 to ImgList.Source.Count - 1 do
      lst.Add(ImgList.Source[i].Name);

    lst.Sort;

    for i := 0 to lst.Count - 1 do
      with ImgList.Destination.Add.Layers.Add do
      begin
        name := lst[i];
        sourcerect.rect := ImgList.Source[ImgList.Source.IndexOf(name)
          ].MultiResBitmap.ItemByScale(1, true, false).Bitmap.BoundsF;
      end;
  finally
    lst.Free;
  end;
end;

function TfrmMain.getTemplateFromResource(TemplateName: string): string;
var
  rs: TResourceStream;
  st: TStringStream;
begin
  Result := '';
  rs := TResourceStream.Create(MainInstance, TemplateName, RT_RCDATA);
  try
    rs.Position := 0;
    st := TStringStream.Create('', tencoding.UTF8);
    try
      st.CopyFrom(rs);
      st.Position := 0;
      Result := st.ReadString(st.Size);
    finally
      st.Free;
    end;
  finally
    rs.Free;
  end;
end;

procedure TfrmMain.ValidatePathField;
begin
  if (edtImportFolder.Text.IsEmpty) then
  begin
    edtImportFolder.SetFocus;
    raise exception.Create
      ('Please specify the folder where are the PNG files to import.');
  end;
  if (not TDirectory.Exists(edtImportFolder.Text)) then
  begin
    edtImportFolder.SetFocus;
    raise exception.Create('It''s not a valid folder.');
  end;
end;

procedure TfrmMain.ImportPNGFiles(FolderName: string; ImgList: TImageList;
  Recursive: boolean);
var
  lst: tstringdynarray;
  i: integer;
  ImgName: string;
  SizeSeparatorPos: integer;
  ImgId: integer;
  FileNameScale: string;
  LScale: single;
begin
{$IFDEF DEBUG}
  log.d('add files from ' + FolderName);
{$ENDIF}
  lst := TDirectory.GetFiles(FolderName);
  for i := 0 to length(lst) - 1 do
    if (tpath.GetExtension(lst[i]).ToLower = '.png') then
    begin
      ImgName := tpath.GetFileNameWithoutExtension(lst[i]);
      LScale := 0;
      SizeSeparatorPos := ImgName.IndexOf('@');
      if (SizeSeparatorPos >= 0) then
      begin
        FileNameScale := ImgName.Substring(SizeSeparatorPos + 1,
          ImgName.IndexOf('x', SizeSeparatorPos + 1) - SizeSeparatorPos - 1);
        if not FileNameScale.IsEmpty then
          try
            LScale := FileNameScale.ToSingle;
          except
            try
              LScale := FileNameScale.Replace('.', ',').ToSingle;
            except
              try
                LScale := FileNameScale.Replace(',', '.').ToSingle;
              except
                LScale := 0;
              end;
            end;
          end;
        ImgName := ImgName.Substring(0, SizeSeparatorPos);
      end;
      if ImgName.IsEmpty then
        raise exception.Create('File ' + lst[i] + ' can''t be processed.')
      else
      begin
        ImgId := ImgList.Source.IndexOf(ImgName);
        if (ImgId < 0) then
        begin
          ImgList.Source.Add.Name := ImgName;
          ImgId := ImgList.Source.IndexOf(ImgName);
        end;

        if (ImgId < 0) then
          raise exception.Create('Can''t create source for image "' + ImgName +
            '" from file ' + lst[i] + ' !');
        with ImgList.Source[ImgId].MultiResBitmap.Add do
        begin
          Bitmap.LoadFromFile(lst[i]);
          filename := lst[i];
          if LScale > 0 then
            scale := LScale;
        end;
      end;
    end;

  if Recursive then
  begin
    lst := TDirectory.GetDirectories(FolderName);
    for i := 0 to length(lst) - 1 do
      ImportPNGFiles(lst[i], ImgList);
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
