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
  File last update : 2025-07-29T20:04:08.000+02:00
  Signature : 2c5f8a108a5bac9eaba050c45224245e1443017c
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
  FMX.Edit;

type
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
    btnFolderToImportSelect: TEditButton;
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
    lblTDataModuleVarName: TLabel;
    edtTDataModuleVarName: TEdit;
    cbExportTDataModuleVariable: TCheckBox;
    cbExportToSourceFolder: TCheckBox;
    lblExportChoosenFolder: TLabel;
    edtExportChoosenFolder: TEdit;
    btnExportChoosenFolderSelect: TEditButton;
    lblExportToDelphiOrCPPBuilder: TLabel;
    flExportToDelphiOrCPPBuilder: TFlowLayout;
    cbExportDelphiUnit: TCheckBox;
    cbExportCPPBuilderUnit: TCheckBox;
    cbDragAndDropExportTImageList: TCheckBox;
    cbDragAndDropExportTDataModule: TCheckBox;
    btnSaveSettings: TButton;
    procedure actAboutExecute(Sender: TObject);
    procedure btnExportTImageListClick(Sender: TObject);
    procedure btnExportTDataModuleUnitClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
  private
    procedure CopySettingsToScreenFields;
    procedure CopyScreenFieldsToSettings;
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
  uConfigHelpers;

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
    TConfig.Current.F2FilExportTDataModuleVariable :=
      cbExportTDataModuleVariable.IsChecked;
    TConfig.Current.F2FilExportToSourceFolder :=
      cbExportToSourceFolder.IsChecked;
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
    TConfig.Current.F2FilTDataModuleVarName := edtTDataModuleVarName.Text;
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
  cbExportTDataModuleVariable.IsChecked :=
    TConfig.Current.F2FilExportTDataModuleVariable;
  cbExportToSourceFolder.IsChecked := TConfig.Current.F2FilExportToSourceFolder;
  cbFillTImageListDestinationProperty.IsChecked :=
    TConfig.Current.F2FilFillTImageListDestinationProperty;
  edtFolderToImport.Text := TConfig.Current.F2FilFolderToImport;
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
  edtTDataModuleVarName.Text := TConfig.Current.F2FilTDataModuleVarName;
  edtTImageListFieldName.Text := TConfig.Current.F2FilTImageListFieldName;
end;

procedure TMainForm.btnExportTDataModuleUnitClick(Sender: TObject);
begin
  CopyScreenFieldsToSettings;
  // TODO : à compléter
end;

procedure TMainForm.btnExportTImageListClick(Sender: TObject);
begin
  CopyScreenFieldsToSettings;
  // TODO : à compléter
end;

procedure TMainForm.btnSaveSettingsClick(Sender: TObject);
begin
  CopyScreenFieldsToSettings;
end;

function TMainForm.GetNewDoc(const FileName: string): TDocumentAncestor;
begin
  result := nil;
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
    lblSizeSeparatorChar.Text := 'Séparateur de taille (ex : "image1@1.png")';
    lblImagesExtensions.Text := 'Choix des images';

    lblTImageListExportSettingsTitle.Text := 'Paramètres de la liste d''images';
    lblTImageListFieldName.Text := 'Nom de la liste';
    cbFillTImageListDestinationProperty.Text :=
      'Remplir la propriété "destination"';

    lblTDataModuleExportSettingsTitle.Text := 'Paramètres du module de données';
    lblTDataModuleUnitName.Text := 'Nom de l''unité';
    lblTDataModuleTypeName.Text := 'Nom du type';
    cbExportTDataModuleVariable.Text := 'Ajouter une variable globale';
    lblTDataModuleVarName.Text := 'Nom de la variable globale';
    cbExportToSourceFolder.Text := 'Enregistrer dans le dossier des images';
    lblExportChoosenFolder.Text := 'ou dans ce dossier :';
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
    lblSizeSeparatorChar.Text := 'Size separator (ex : "image1@1.png")';
    lblImagesExtensions.Text := 'Extension';

    lblTImageListExportSettingsTitle.Text := 'Image list settings';
    lblTImageListFieldName.Text := 'List name';
    cbFillTImageListDestinationProperty.Text :=
      'Fill the "destination" property';

    lblTDataModuleExportSettingsTitle.Text := 'Data module settings';
    lblTDataModuleUnitName.Text := 'Unit name';
    lblTDataModuleTypeName.Text := 'Type name';
    cbExportTDataModuleVariable.Text := 'Add a global variable';
    lblTDataModuleVarName.Text := 'Global variable name';
    cbExportToSourceFolder.Text := 'Save unit in images source folder';
    lblExportChoosenFolder.Text := 'or in this folder :';
    lblExportToDelphiOrCPPBuilder.Text := 'Export unit for :';

    lblDragAndDropSettingsTitle.Text :=
      'Action when a folder is dropped in the images source foldr';
    cbDragAndDropExportTImageList.Text :=
      'Copy the TImageList to the clipboard';
    cbDragAndDropExportTDataModule.Text := 'Export the data module unit';
  end;
end;

end.
