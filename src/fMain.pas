unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, System.ImageList, FMX.ImgList,
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    btnSaveToClipboard: TButton;
    ZoneBoutons: TGridPanelLayout;
    btnSaveAsTDataModule: TButton;
    procedure btnSaveToClipboardClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure btnSaveAsTDataModuleClick(Sender: TObject);
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
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.IOUtils, FMX.Platform;

/// <summary>
/// ComponentToStringProc sérialise un composant pour l'obtenir sous forme de
/// chaîne de caractères comme dans les sources des écrans (fichiers fmx/dfm).
/// </summary>
/// http://docwiki.embarcadero.com/CodeExamples/Sydney/en/ComponentToString_(Delphi)
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

procedure TForm1.btnSaveAsTDataModuleClick(Sender: TObject);
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
      (tpath.GetFileNameWithoutExtension(Edit1.Text));
  except
    DMName := 'dm';
  end;

  ImgList := TImageList.Create(self);
  try
    ImgList.Name := 'ImageList';

    ImportPNGFiles(Edit1.Text, ImgList);

    GenerateDestinationFromSource(ImgList);

    UnitName := 'u' + DMName;
    try
      DFMFilePath := tpath.Combine(Edit1.Text, UnitName + '.dfm');
      UnitDFMSource := getTemplateFromResource('DM_dfm_template');
      tfile.WriteAllText(DFMFilePath, UnitDFMSource.Replace('%%UnitName%%',
        UnitName).Replace('%%DMName%%', DMName).Replace('%%ImageListName%%',
        ImgList.Name).Replace('%%ImageList%%', ComponentToStringProc(ImgList)));
      try
        PASFilePath := tpath.Combine(Edit1.Text, UnitName + '.pas');
        UnitPASSource := getTemplateFromResource('DM_pas_template');
        tfile.WriteAllText(PASFilePath, UnitPASSource.Replace('%%UnitName%%',
          UnitName).Replace('%%DMName%%', DMName).Replace('%%ImageListName%%',
          ImgList.Name).Replace('%%datetime%%', DateTimetoStr(now)));
        try
          TXTFilePath := tpath.Combine(Edit1.Text, UnitName + '.lst');
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

procedure TForm1.btnSaveToClipboardClick(Sender: TObject);
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
        (tpath.GetFileNameWithoutExtension(Edit1.Text));
    except
      ImgList.Name := 'ImageList1';
    end;

    ImportPNGFiles(Edit1.Text, ImgList);

    GenerateDestinationFromSource(ImgList);

    if TPlatformServices.current.SupportsPlatformService(IFMXClipboardService,
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

procedure TForm1.Edit1Enter(Sender: TObject);
begin
  Edit1.SelectAll;
end;

function TForm1.FilterPascalIdentifier(NameToFilter: string): string;
var
  i: integer;
  c: Char;
  ChiffresAutorises: boolean;
begin
  // pas de chiffre en premier caractère
  // que des lettres (majuscules, minuscules), des soulignes, des chiffres
  Result := '';
  ChiffresAutorises := false;
  for i := 0 to NameToFilter.Length - 1 do
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

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF DEBUG}
  Edit1.Text :=
    'C:\Users\olfso\Documents\Embarcadero\Studio\Projets\Folder2FMXImageList\TestImages';
{$ENDIF}
  Edit1.SetFocus;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkescape then
  begin
    Key := 0;
    KeyChar := #0;
    close;
  end;
end;

procedure TForm1.GenerateDestinationFromSource(ImgList: TImageList);
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

function TForm1.getTemplateFromResource(TemplateName: string): string;
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

procedure TForm1.ValidatePathField;
begin
  if (Edit1.Text.IsEmpty) then
  begin
    Edit1.SetFocus;
    raise exception.Create
      ('Please specify the folder where are the PNG files to import.');
  end;
  if (not TDirectory.Exists(Edit1.Text)) then
  begin
    Edit1.SetFocus;
    raise exception.Create('It''s not a valid folder.');
  end;
end;

procedure TForm1.ImportPNGFiles(FolderName: string; ImgList: TImageList;
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
  for i := 0 to Length(lst) - 1 do
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
    for i := 0 to Length(lst) - 1 do
      ImportPNGFiles(lst[i], ImgList);
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
