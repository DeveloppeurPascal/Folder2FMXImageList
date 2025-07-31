(* C2PP
  ***************************************************************************

  Folder to FMX Image List

  Copyright 2022-2024 Patrick Prémartin under AGPL 3.0 license.

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
  File last update : 2025-07-31T21:21:08.000+02:00
  Signature : 020c5bf338aaf09861879099d715f46f306021fe
  ***************************************************************************
*)

unit fMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  _MainFormAncestor,
  System.Actions,
  FMX.ActnList,
  FMX.Menus,
  uDocumentsAncestor,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ComboEdit,
  FMX.Edit,
  Olf.FMX.SelectDirectory,
  System.ImageList,
  FMX.ImgList,
  FMX.Objects,
  FMX.DialogService;

type
{$SCOPEDENUMS ON}
  TCaseType = (_DoNotChange, _PascalCase, _camelCase);

  TMainForm = class(T__MainFormAncestor)
    ToolBar1: TToolBar;
    btnAbout: TButton;
    btnQuit: TButton;
    VertScrollBox1: TVertScrollBox;
    GridPanelLayout1: TGridPanelLayout;
    btnExportTImageList: TButton;
    btnExportTDataModuleUnit: TButton;
    pnlImportedImageSettings: TPanel;
    pnlTDataModuleExportSettings: TPanel;
    pnlDragAndDropSettings: TPanel;
    pnlTImageListExportSettings: TPanel;
    lblDragAndDropSettingsTitle: TLabel;
    lblTDataModuleExportSettingsTitle: TLabel;
    lblTImageListExportSettingsTitle: TLabel;
    vsImportedImageSettings: TVertScrollBox;
    vsTImageListExportSettings: TVertScrollBox;
    vsTDataModuleExportSettings: TVertScrollBox;
    vsDragAndDropSettings: TVertScrollBox;
    lblImportedImageSettingsTitle: TLabel;
    lblFolderToImport: TLabel;
    edtFolderToImport: TEdit;
    lblSizeSeparatorChar: TLabel;
    ceSizeSeparatorChar: TComboEdit;
    lblImagesExtensions: TLabel;
    flImagesExtensions: TFlowLayout;
    cbImportJPEG: TCheckBox;
    cbImportPNG: TCheckBox;
    lblTImageListFieldName: TLabel;
    edtTImageListFieldName: TEdit;
    cbFillTImageListDestinationProperty: TCheckBox;
    lblTDataModuleUnitName: TLabel;
    edtTDataModuleUnitName: TEdit;
    edtTDataModuleUnitNamePrefix: TEdit;
    edtTDataModuleUnitNameSuffix: TEdit;
    lblTDataModuleTypeName: TLabel;
    edtTDataModuleTypeName: TEdit;
    edtTDataModuleTypeNamePrefix: TEdit;
    edtTDataModuleTypeNameSuffix: TEdit;
    lblExportChoosenFolder: TLabel;
    edtExportChoosenFolder: TEdit;
    lblExportToDelphiOrCPPBuilder: TLabel;
    flExportToDelphiOrCPPBuilder: TFlowLayout;
    cbExportDelphiUnit: TCheckBox;
    cbExportCPPBuilderUnit: TCheckBox;
    cbDragAndDropExportTImageList: TCheckBox;
    cbDragAndDropExportTDataModule: TCheckBox;
    btnSaveSettings: TButton;
    btnFolderToImportSelect: TButton;
    btnExportChoosenFolderSelect: TButton;
    osddFolderToImportSelect: TOlfSelectDirectoryDialog;
    osddExportChoosenFolderSelect: TOlfSelectDirectoryDialog;
    LockedAreaBackground: TRectangle;
    LockedAreaAniIndicator: TAniIndicator;
    LockedArea: TLayout;
    procedure actAboutExecute(Sender: TObject);
    procedure btnExportTImageListClick(Sender: TObject);
    procedure btnExportTDataModuleUnitClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnFolderToImportSelectClick(Sender: TObject);
    procedure btnExportChoosenFolderSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CopySettingsToScreenFields;
    procedure CopyScreenFieldsToSettings;
    procedure SetDefaultSettings;
    function FilterPascalIdentifier(const NameToFilter: string;
      const CaseType: TCaseType): string;
    function getTemplateFromResource(const TemplateName: string): string;
    function GetImageListFromSourceFolder: TImageList;
    procedure FillImageListSource(const ImageList: TImageList);
    procedure FillImageListDestination(const ImageList: TImageList);
    procedure LockTheScreenAndSubmitAThread(const Proc: TProc);
    procedure UnlockTheScreen;
    function GetImagesFilesList: TStringList;
  protected
    function GetNewDoc(const FileName: string = ''): TDocumentAncestor;
      override;
  public
    procedure TranslateTexts(const Language: string); override;
    procedure AfterConstruction; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  uConfig,
  uConfigHelpers,
  System.IOUtils,
  FMX.Platform,
  FMX.MultiResBitmap,
  System.DateUtils,
  uConsts;

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

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  DoAboutAction(Sender);
end;

procedure TMainForm.AfterConstruction;
begin
  inherited;
  CopySettingsToScreenFields;
end;

procedure TMainForm.CopyScreenFieldsToSettings;
begin
  TConfig.Current.beginupdate;
  try
    TConfig.Current.F2FilDragAndDropExportTDataModule :=
      cbDragAndDropExportTDataModule.IsChecked;
    TConfig.Current.F2FilDragAndDropExportTImageList :=
      cbDragAndDropExportTImageList.IsChecked;
    TConfig.Current.F2FilExportChoosenFolder := edtExportChoosenFolder.Text;
    TConfig.Current.F2FilExportCPPBuilderUnit :=
      cbExportCPPBuilderUnit.IsChecked;
    TConfig.Current.F2FilExportDelphiUnit := cbExportDelphiUnit.IsChecked;
    TConfig.Current.F2FilFillTImageListDestinationProperty :=
      cbFillTImageListDestinationProperty.IsChecked;
    TConfig.Current.F2FilFolderToImport := edtFolderToImport.Text;
    TConfig.Current.F2FilImportJPEG := cbImportJPEG.IsChecked;
    TConfig.Current.F2FilImportPNG := cbImportPNG.IsChecked;
    TConfig.Current.F2FilSizeSeparatorChar := ceSizeSeparatorChar.Text;
    TConfig.Current.F2FilTDataModuleTypeName := edtTDataModuleTypeName.Text;
    TConfig.Current.F2FilTDataModuleTypeNamePrefix :=
      edtTDataModuleTypeNamePrefix.Text;
    TConfig.Current.F2FilTDataModuleTypeNameSuffix :=
      edtTDataModuleTypeNameSuffix.Text;
    TConfig.Current.F2FilTDataModuleUnitName := edtTDataModuleUnitName.Text;
    TConfig.Current.F2FilTDataModuleUnitNamePrefix :=
      edtTDataModuleUnitNamePrefix.Text;
    TConfig.Current.F2FilTDataModuleUnitNameSuffix :=
      edtTDataModuleUnitNameSuffix.Text;
    TConfig.Current.F2FilTImageListFieldName := edtTImageListFieldName.Text;
  finally
    TConfig.Current.endupdate;
  end;
end;

procedure TMainForm.CopySettingsToScreenFields;
begin
  cbDragAndDropExportTDataModule.IsChecked :=
    TConfig.Current.F2FilDragAndDropExportTDataModule;
  cbDragAndDropExportTImageList.IsChecked :=
    TConfig.Current.F2FilDragAndDropExportTImageList;
  edtExportChoosenFolder.Text := TConfig.Current.F2FilExportChoosenFolder;
  cbExportCPPBuilderUnit.IsChecked := TConfig.Current.F2FilExportCPPBuilderUnit;
  cbExportDelphiUnit.IsChecked := TConfig.Current.F2FilExportDelphiUnit;
  cbFillTImageListDestinationProperty.IsChecked :=
    TConfig.Current.F2FilFillTImageListDestinationProperty;
  edtFolderToImport.Text := TConfig.Current.F2FilFolderToImport;
  SetDefaultSettings;
  cbImportJPEG.IsChecked := TConfig.Current.F2FilImportJPEG;
  cbImportPNG.IsChecked := TConfig.Current.F2FilImportPNG;
  ceSizeSeparatorChar.Text := TConfig.Current.F2FilSizeSeparatorChar;
  edtTDataModuleTypeName.Text := TConfig.Current.F2FilTDataModuleTypeName;
  edtTDataModuleTypeNamePrefix.Text :=
    TConfig.Current.F2FilTDataModuleTypeNamePrefix;
  edtTDataModuleTypeNameSuffix.Text :=
    TConfig.Current.F2FilTDataModuleTypeNameSuffix;
  edtTDataModuleUnitName.Text := TConfig.Current.F2FilTDataModuleUnitName;
  edtTDataModuleUnitNamePrefix.Text :=
    TConfig.Current.F2FilTDataModuleUnitNamePrefix;
  edtTDataModuleUnitNameSuffix.Text :=
    TConfig.Current.F2FilTDataModuleUnitNameSuffix;
  edtTImageListFieldName.Text := TConfig.Current.F2FilTImageListFieldName;
end;

procedure TMainForm.FillImageListDestination(const ImageList: TImageList);
var
  lst: TStringList;
  i: integer;
  CustBitmap: TCustomBitmapItem;
  Layer: TLayer;
begin
  if not assigned(ImageList) then
    raise exception.Create('A valid TImageList instance is needed.');
  // TODO : traduire ce texte

  if not cbFillTImageListDestinationProperty.IsChecked then
    exit;

  lst := TStringList.Create;
  try
    for i := 0 to ImageList.Source.Count - 1 do
      lst.Add(ImageList.Source[i].Name);

    lst.Sort;

    for i := 0 to lst.Count - 1 do
    begin
      Layer := ImageList.Destination.Add.Layers.Add;
      try
        Layer.Name := lst[i];
        CustBitmap := ImageList.Source[ImageList.Source.IndexOf(Layer.Name)
          ].MultiResBitmap.ItemByScale(1, true, false);
        if not assigned(CustBitmap) then
          raise exception.Create('Missing scale 1 for image "' +
            Layer.Name + '"')
          // TODO : traduire texte
        else
          Layer.SourceRect.Rect := CustBitmap.Bitmap.BoundsF;
      except
        Layer.Free;
        raise;
      end;
    end;
  finally
    lst.Free;
  end;
end;

procedure TMainForm.FillImageListSource(const ImageList: TImageList);
var
  Files: TStringList;
  i: integer;
  SizeSeparatorPos: integer;
  ImgId: integer;
  ImgScale: string;
  LScale: single;
  ImgName: string;
begin
  if not assigned(ImageList) then
    raise exception.Create('A valid TImageList instance is needed.');
  // TODO : traduire ce texte

  if ceSizeSeparatorChar.Text.Trim.IsEmpty then
  begin
    ceSizeSeparatorChar.SetFocus;
    raise exception.Create
      ('Give a separator between the image name and its size.');
    // TODO : traduire le texte
  end;

  Files := GetImagesFilesList;
  try
    if (Files.Count < 1) then
    begin
      edtFolderToImport.SetFocus;
      raise exception.Create('No image found in this folder !');
      // TODO : traduire texte
    end;

    for i := 0 to Files.Count - 1 do
    begin
      ImgName := tpath.GetFileNameWithoutExtension(Files[i]);

      // Search the name and size of this picture in its filename
      LScale := 0;
      SizeSeparatorPos := ImgName.IndexOf(ceSizeSeparatorChar.Text);
      if (SizeSeparatorPos >= 0) then
      begin
        ImgScale := ImgName.Substring(SizeSeparatorPos +
          ceSizeSeparatorChar.Text.Length);
        ImgName := ImgName.Substring(0, SizeSeparatorPos);
        while (ImgScale.Length > 0) and
          (not charinset(ImgScale.Chars[ImgScale.Length - 1], ['0' .. '9'])) do
          ImgScale := ImgScale.Substring(0, ImgScale.Length - 1);
        if not ImgScale.IsEmpty then
          try
            LScale := ImgScale.Replace('.', ',').ToSingle;
          except
            try
              LScale := ImgScale.Replace(',', '.').ToSingle;
            except
              LScale := 0;
            end;
          end;
      end;

      // Load the image to the image list
      if ImgName.IsEmpty then
        raise exception.Create('File ' + Files[i] + ' can''t be processed.')
        // TODO : traduire le texte
      else
      begin
        ImgId := ImageList.Source.IndexOf(ImgName);
        if (ImgId < 0) then
        begin
          ImageList.Source.Add.Name := ImgName;
          ImgId := ImageList.Source.IndexOf(ImgName);
        end;

        if (ImgId < 0) then
          raise exception.Create('Can''t create the source for image "' +
            ImgName + '" from file ' + Files[i] + ' !');
        // TODO : traduire le texte

        with ImageList.Source[ImgId].MultiResBitmap.Add do
        begin
          Bitmap.LoadFromFile(Files[i]);
          FileName := Files[i];
          if LScale > 0 then
            scale := LScale;
        end;
      end;
    end;
  finally
    Files.Free;
  end;
end;

function TMainForm.FilterPascalIdentifier(const NameToFilter: string;
  const CaseType: TCaseType): string;
var
  i: integer;
  c: Char;
  ChiffresAutorises: boolean;
  FirstLetter: boolean;
begin
  // pas de chiffre en premier caractère
  // que des lettres (majuscules, minuscules), des soulignes, des chiffres
  Result := '';
  ChiffresAutorises := false;
  FirstLetter := true;
  for i := 0 to NameToFilter.Length - 1 do
  begin
    c := NameToFilter.Chars[i];
    if charinset(c, ['-', ' ']) then
      c := '_';
    if charinset(c, ['a' .. 'z', 'A' .. 'Z', '_']) or
      (ChiffresAutorises and (charinset(c, ['0' .. '9']))) then
    begin
      case CaseType of
        TCaseType._PascalCase:
          if (c <> '_') then
            if FirstLetter then
              Result := Result + uppercase(c)
            else
              Result := Result + lowercase(c);
        TCaseType._camelCase:
          if (c <> '_') then
            if FirstLetter and ChiffresAutorises then
              Result := Result + uppercase(c)
            else
              Result := Result + lowercase(c);
      else
        Result := Result + c;
      end;
      FirstLetter := (c = '_');
      if not ChiffresAutorises then
        ChiffresAutorises := true;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  UnlockTheScreen;
end;

procedure TMainForm.btnExportChoosenFolderSelectClick(Sender: TObject);
begin
  if edtExportChoosenFolder.Text.Trim.IsEmpty then
  begin
    if not edtFolderToImport.Text.Trim.IsEmpty then
      osddExportChoosenFolderSelect.Directory := edtFolderToImport.Text
    else
      osddExportChoosenFolderSelect.Directory := tpath.GetDocumentsPath;
  end
  else
    osddExportChoosenFolderSelect.Directory := edtExportChoosenFolder.Text;

  if osddExportChoosenFolderSelect.Execute and
    (not osddExportChoosenFolderSelect.Directory.IsEmpty) and
    tdirectory.Exists(osddExportChoosenFolderSelect.Directory) then
    edtExportChoosenFolder.Text := osddExportChoosenFolderSelect.Directory;
end;

procedure TMainForm.btnExportTDataModuleUnitClick(Sender: TObject);
  function GetEditValue(const edt: TEdit;
    const AsPascalIdentifier: boolean = true): string;
  begin
    if AsPascalIdentifier then
      Result := FilterPascalIdentifier(edt.Text, TCaseType._DoNotChange)
    else
      Result := edt.Text.Trim;
    if Result.IsEmpty then
      Result := edt.TextPrompt.Trim;
  end;

var
  UnitName, DMTypeName, DMVarName, ImageListVarName, FromPath, ToPath: string;
  SrcDFM, SrcPas, SrcH, SrcCPP: string;
begin
  if not(cbExportDelphiUnit.IsChecked or cbExportCPPBuilderUnit.IsChecked) then
  begin
    cbExportDelphiUnit.SetFocus;
    raise exception.Create
      ('Choose a language before exporting the TDataModule unit.');
    // TODO : traduire texte
  end;

  UnitName := GetEditValue(edtTDataModuleUnitNamePrefix) +
    GetEditValue(edtTDataModuleUnitName) +
    GetEditValue(edtTDataModuleUnitNameSuffix);
  if UnitName.IsEmpty then
  begin
    edtTDataModuleUnitNamePrefix.SetFocus;
    raise exception.Create('The unit name is needed !');
    // TODO : traduire texte
  end;

  DMTypeName := GetEditValue(edtTDataModuleTypeNamePrefix) +
    GetEditValue(edtTDataModuleTypeName) +
    GetEditValue(edtTDataModuleTypeNameSuffix);
  if DMTypeName.IsEmpty then
  begin
    edtTDataModuleTypeNamePrefix.SetFocus;
    raise exception.Create('The data module type identifier is needed !');
    // TODO : traduire texte
  end
  else if not DMTypeName.StartsWith('T', true) then
    DMTypeName := 'T' + DMTypeName;

  DMVarName := DMTypeName.Substring(1);

  ImageListVarName := GetEditValue(edtTImageListFieldName);
  if ImageListVarName.IsEmpty then
  begin
    edtTImageListFieldName.SetFocus;
    raise exception.Create('The image list needs a name !');
    // TODO : traduire texte
  end;

  FromPath := GetEditValue(edtFolderToImport, false);
  if FromPath.IsEmpty then
  begin
    edtFolderToImport.SetFocus;
    raise exception.Create('Choose a source folder.');
    // TODO : traduire texte
  end
  else if not tdirectory.Exists(FromPath) then
  begin
    edtFolderToImport.SetFocus;
    raise exception.Create('The source folder doesn''t exist.');
    // TODO : traduire texte
  end;

  ToPath := GetEditValue(edtExportChoosenFolder, false);
  if ToPath.IsEmpty then
  begin
    edtExportChoosenFolder.SetFocus;
    raise exception.Create('Where do you want to save the files ?');
    // TODO : traduire texte
  end
  else if not tdirectory.Exists(ToPath) then
  begin
    edtExportChoosenFolder.SetFocus;
    raise exception.Create('The destination folder doesn''t exist.');
    // TODO : traduire texte
  end;

  if cbExportDelphiUnit.IsChecked then
  begin
    SrcDFM := getTemplateFromResource('DM_dfm_template');
    SrcPas := getTemplateFromResource('DM_pas_template');
  end
  else
  begin
    SrcDFM := '';
    SrcPas := '';
  end;

  if cbExportCPPBuilderUnit.IsChecked then
  begin
    SrcDFM := getTemplateFromResource('DM_dfm_template');
    SrcH := getTemplateFromResource('DM_h_template');
    SrcCPP := getTemplateFromResource('DM_cpp_template');
  end
  else
  begin
    SrcH := '';
    SrcCPP := '';
  end;

  LockTheScreenAndSubmitAThread(
    procedure function ReplaceTexts(const ImgList: TImageList;
      const Template: string): string;
    begin
      Result := Template.Replace('%%UnitName%%', UnitName).Replace('%%DMType%%',
        DMTypeName).Replace('%%DMName%%', DMVarName)
        .Replace('%%ImageListName%%', ImgList.Name).Replace('%%datetime%%',
        DateToISO8601(now, true)).Replace('%%ImportFolder%%', FromPath)
        .Replace('%%AboutCaption%%', CAboutTitle + ' v' + CAboutVersionNumber)
        .Replace('%%AboutURL%%', CAboutURL).Replace('%%ImageList%%',
        ComponentToStringProc(ImgList));
    end;
    var
      ImgList: TImageList;
      i: integer;
      s: string;
    begin
      ImgList := GetImageListFromSourceFolder;
      try
        if not SrcDFM.IsEmpty then
          tfile.WriteAllText(tpath.combine(ToPath, UnitName + '.dfm'),
            ReplaceTexts(ImgList, SrcDFM));

        if not SrcPas.IsEmpty then
          tfile.WriteAllText(tpath.combine(ToPath, UnitName + '.pas'),
            ReplaceTexts(ImgList, SrcPas));

        if not SrcH.IsEmpty then
          tfile.WriteAllText(tpath.combine(ToPath, UnitName + '.h'),
            ReplaceTexts(ImgList, SrcH));

        if not SrcCPP.IsEmpty then
          tfile.WriteAllText(tpath.combine(ToPath, UnitName + '.cpp'),
            ReplaceTexts(ImgList, SrcCPP));

        s := '';
        for i := 0 to ImgList.Destination.Count - 1 do
          s := s + ImgList.Destination[i].Layers[0].Name + Tabulator +
            i.ToString + CarriageReturn + LineFeed;
        tfile.WriteAllText(tpath.combine(ToPath, UnitName + '.lst'), s);

        tthread.Synchronize(nil,
          procedure
          begin
            TDialogService.ShowMessage('Export done.');
            // TODO : traduire le texte
          end);
      finally
        ImgList.Free;
      end;
      tthread.Synchronize(nil,
        procedure
        begin
          CopyScreenFieldsToSettings;
        end);
    end);
end;

procedure TMainForm.btnExportTImageListClick(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    iinterface(Clipboard)) then
    raise exception.Create('Clipboard not available !'); // TODO : à traduire

  LockTheScreenAndSubmitAThread(
    procedure
    var
      ImgList: TImageList;
    begin
      ImgList := GetImageListFromSourceFolder;
      try
        tthread.Synchronize(nil,
          procedure
          begin
            Clipboard.SetClipboard(ComponentToStringProc(ImgList));
            TDialogService.ShowMessage
              ('TImageList exported. Paste it on a FMX TDataModule, TFrame or TForm.');
            // TODO : traduire le texte
          end);
      finally
        ImgList.Free;
      end;
      tthread.Synchronize(nil,
        procedure
        begin
          CopyScreenFieldsToSettings;
        end);
    end);
end;

procedure TMainForm.btnFolderToImportSelectClick(Sender: TObject);
begin
  if edtFolderToImport.Text.Trim.IsEmpty then
    osddFolderToImportSelect.Directory := tpath.GetDocumentsPath
  else
    osddFolderToImportSelect.Directory := edtFolderToImport.Text;

  if osddFolderToImportSelect.Execute and
    (not osddFolderToImportSelect.Directory.IsEmpty) and
    tdirectory.Exists(osddFolderToImportSelect.Directory) then
  begin
    edtFolderToImport.Text := osddFolderToImportSelect.Directory;
    SetDefaultSettings;
  end;
end;

procedure TMainForm.btnSaveSettingsClick(Sender: TObject);
begin
  CopyScreenFieldsToSettings;
end;

function TMainForm.GetImageListFromSourceFolder: TImageList;
var
  s: string;
begin
  Result := TImageList.Create(nil);
  try
    s := FilterPascalIdentifier(edtTImageListFieldName.Text,
      TCaseType._DoNotChange);
    if s.IsEmpty then
      s := edtTImageListFieldName.TextPrompt;
    Result.Name := s;

    FillImageListSource(Result);

    FillImageListDestination(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TMainForm.GetImagesFilesList: TStringList;
  procedure AddImages(const FromFolder: string; var ToList: TStringList);
  var
    Files: TStringDynArray;
    Folders: TStringDynArray;
    i: integer;
  begin
    if not assigned(ToList) then
      exit;
    if FromFolder.IsEmpty then
      exit;
    if not tdirectory.Exists(FromFolder) then
      exit;

    Files := tdirectory.GetFiles(FromFolder,
      function(const Path: string; const SearchRec: TSearchRec): boolean
      begin
        Result := (cbImportPNG.IsChecked and string(SearchRec.Name)
          .EndsWith('.png', true)) or (cbImportJPEG.IsChecked and
          (string(SearchRec.Name).EndsWith('.jpg', true) or
          string(SearchRec.Name).EndsWith('.jpeg', true)));
      end);
    for i := 0 to Length(Files) - 1 do
      ToList.Add(Files[i]);

    Folders := tdirectory.GetDirectories(FromFolder);
    for i := 0 to Length(Folders) - 1 do
      AddImages(Folders[i], ToList);
  end;

var
  SourceFolder: string;
begin
  SourceFolder := edtFolderToImport.Text.Trim;

  if SourceFolder.IsEmpty then
  begin
    edtFolderToImport.SetFocus;
    raise exception.Create('Image source folder is needed !');
    // TODO : traduire texte
  end;

  if not tdirectory.Exists(SourceFolder) then
  begin
    edtFolderToImport.SetFocus;
    raise exception.Create('Image source folder doesn''t exist !');
    // TODO : traduire texte
  end;

  if not(cbImportJPEG.IsChecked or cbImportPNG.IsChecked) then
  begin
    cbImportJPEG.SetFocus;
    raise exception.Create('Enable JPEG or PNG images format.');
    // TODO : traduire le texte
  end;

  Result := TStringList.Create;
  AddImages(SourceFolder, Result);
end;

function TMainForm.GetNewDoc(const FileName: string): TDocumentAncestor;
begin
  Result := nil;
end;

function TMainForm.getTemplateFromResource(const TemplateName: string): string;
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

procedure TMainForm.LockTheScreenAndSubmitAThread(const Proc: TProc);
begin
  LockedArea.visible := true;
  LockedArea.BringToFront;
  LockedAreaAniIndicator.Enabled := true;
  LockedAreaAniIndicator.BringToFront;

  try
    tthread.CreateAnonymousThread(
      procedure
      var
        Error: boolean;
      begin
        Error := false;
        try
          Proc;
        except
          on e: exception do
          begin
            Error := true;
            tthread.Synchronize(nil,
              procedure
              begin
                TDialogService.ShowMessage(e.Message,
                  procedure(const AModalResult: TModalResult)
                  begin
                    UnlockTheScreen;
                  end);
              end);
          end;
        end;
        if not Error then
          UnlockTheScreen;
      end).start;
  except
    UnlockTheScreen;
  end;
end;

procedure TMainForm.SetDefaultSettings;
var
  FolderNameAsPascalIdentifier: string;
begin
  if edtFolderToImport.Text.Trim.IsEmpty then
    FolderNameAsPascalIdentifier := ''
  else
    FolderNameAsPascalIdentifier := FilterPascalIdentifier
      (tpath.GetFileNameWithoutExtension(edtFolderToImport.Text),
      TCaseType._PascalCase);

  edtTImageListFieldName.TextPrompt := 'ImageList';
  edtTDataModuleUnitNamePrefix.TextPrompt := 'UDm';
  edtTDataModuleUnitName.TextPrompt := FolderNameAsPascalIdentifier;
  edtTDataModuleTypeNamePrefix.TextPrompt := 'TDm';
  edtTDataModuleTypeName.TextPrompt := FolderNameAsPascalIdentifier;
  edtExportChoosenFolder.TextPrompt := edtFolderToImport.Text;
end;

procedure TMainForm.TranslateTexts(const Language: string);
begin
  inherited;

  if (Language = 'fr') then
  begin
    btnExportTImageList.Text := 'Copie d''une TImageList';
    btnExportTDataModuleUnit.Text := 'Export en TDataModule';
    btnAbout.Text := 'A propos';
    btnSaveSettings.Text := 'Enregistrer';

    lblImportedImageSettingsTitle.Text := 'Images à importer';
    lblFolderToImport.Text := 'Dossier des images';
    osddFolderToImportSelect.Text :=
      'Choisissez le dossier contenant les images à importer';
    lblSizeSeparatorChar.Text := 'Séparateur de taille (ex : "image1@1.png")';
    lblImagesExtensions.Text := 'Choix des images';

    lblTImageListExportSettingsTitle.Text := 'Paramètres de la liste d''images';
    lblTImageListFieldName.Text := 'Nom de la liste';
    cbFillTImageListDestinationProperty.Text :=
      'Remplir la propriété "destination"';

    lblTDataModuleExportSettingsTitle.Text := 'Paramètres du module de données';
    lblTDataModuleUnitName.Text := 'Nom de l''unité';
    lblTDataModuleTypeName.Text := 'Nom du type';
    lblExportChoosenFolder.Text := 'Exporter les fichiers vers ce dossier :';
    osddExportChoosenFolderSelect.Text := 'Où enregistrer les codes sources ?';
    lblExportToDelphiOrCPPBuilder.Text := 'Exporter pour :';

    lblDragAndDropSettingsTitle.Text :=
      'Action en cas de drag&&drop de dossier';
    cbDragAndDropExportTImageList.Text :=
      'Copie de la liste d''images dans le presse papier';
    cbDragAndDropExportTDataModule.Text := 'Export du module de données';
  end
  else
  begin
    btnExportTImageList.Text := 'Copy as a TImageList';
    btnExportTDataModuleUnit.Text := 'Save as a TDataModule unit';
    btnAbout.Text := 'About';
    btnAbout.Text := 'Save settings';

    lblImportedImageSettingsTitle.Text := 'Images to import';
    lblFolderToImport.Text := 'Source folder';
    osddFolderToImportSelect.Text :=
      'Choose the folder containing the images to import';
    lblSizeSeparatorChar.Text := 'Size separator (ex : "image1@1.png")';
    lblImagesExtensions.Text := 'Extension';

    lblTImageListExportSettingsTitle.Text := 'Image list settings';
    lblTImageListFieldName.Text := 'List name';
    cbFillTImageListDestinationProperty.Text :=
      'Fill the "destination" property';

    lblTDataModuleExportSettingsTitle.Text := 'Data module settings';
    lblTDataModuleUnitName.Text := 'Unit name';
    lblTDataModuleTypeName.Text := 'Type name';
    lblExportChoosenFolder.Text := 'Export files to this folder :';
    osddExportChoosenFolderSelect.Text :=
      'Folder where to store the generated units';
    lblExportToDelphiOrCPPBuilder.Text := 'Export unit for :';

    lblDragAndDropSettingsTitle.Text :=
      'Action when a folder is dropped in the images source foldr';
    cbDragAndDropExportTImageList.Text :=
      'Copy the TImageList to the clipboard';
    cbDragAndDropExportTDataModule.Text := 'Export the data module unit';
  end;
end;

procedure TMainForm.UnlockTheScreen;
begin
  tthread.Synchronize(nil,
    procedure
    begin
      LockedAreaAniIndicator.Enabled := false;
      LockedArea.visible := false;
    end);
end;

end.
