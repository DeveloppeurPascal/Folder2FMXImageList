unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, System.ImageList, FMX.ImgList;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
  private
    { D?clarations priv?es }
    procedure ImportPNGFiles(FolderName: string; ImgList: TImageList;
      Recursive: boolean = true);
    procedure GenerateDestinationFromSource(ImgList: TImageList);
  public
    { D?clarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.IOUtils, FMX.Platform;

/// <summary>
/// ComponentToStringProc s?rialise un composant pour l'obtenir sous forme de
/// cha?ne de caract?res comme dans les sources des ?crans (fichiers fmx/dfm).
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

procedure TForm1.Button1Click(Sender: TObject);
var
  ImgList: TImageList;
var
  cb: IFMXClipboardService;
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

  Button1.Enabled := false;
  ImgList := TImageList.Create(self);
  try
    try
      ImgList.Name := tpath.GetFileNameWithoutExtension(Edit1.Text);
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
    Button1.Enabled := true;
  end;
end;

procedure TForm1.Edit1Enter(Sender: TObject);
begin
  Edit1.SelectAll;
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
  lst: array of string;
  i: integer;
begin
  setlength(lst, ImgList.Source.Count);
  for i := 0 to ImgList.Source.Count - 1 do
    lst[i] := ImgList.Source[i].Name;

  // TODO : Tri alpha de la liste

  for i := 0 to length(lst) - 1 do
  begin
    with ImgList.Destination.Add.Layers.Add do
    begin
      name := lst[i];
      sourcerect.rect := ImgList.Source[ImgList.Source.IndexOf(name)
        ].MultiResBitmap.ItemByScale(1, true, false).Bitmap.BoundsF;
    end;
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
              LScale := FileNameScale.replace('.', ',').ToSingle;
            except
              try
                LScale := FileNameScale.replace(',', '.').ToSingle;
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
