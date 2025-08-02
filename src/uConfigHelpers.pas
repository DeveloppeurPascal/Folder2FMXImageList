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
  File last update : 2025-07-31T21:33:15.847+02:00
  Signature : ba8c7b6741adb3410ba75542c2ee6946ab158d28
  ***************************************************************************
*)

unit uConfigHelpers;

interface

uses
  uConfig;

type
  TConfigHelpers = class helper for TConfig
  private
    procedure SetF2FilDragAndDropExportTDataModule(const Value: boolean);
    procedure SetF2FilDragAndDropExportTImageList(const Value: boolean);
    procedure SetF2FilExportChoosenFolder(const Value: string);
    procedure SetF2FilExportCPPBuilderUnit(const Value: boolean);
    procedure SetF2FilExportDelphiUnit(const Value: boolean);
    procedure SetF2FilFillTImageListDestinationProperty(const Value: boolean);
    procedure SetF2FilFolderToImport(const Value: string);
    procedure SetF2FilImportJPEG(const Value: boolean);
    procedure SetF2FilImportPNG(const Value: boolean);
    procedure SetF2FilSizeSeparatorChar(const Value: string);
    procedure SetF2FilTDataModuleTypeName(const Value: string);
    procedure SetF2FilTDataModuleTypeNamePrefix(const Value: string);
    procedure SetF2FilTDataModuleTypeNameSuffix(const Value: string);
    procedure SetF2FilTDataModuleUnitName(const Value: string);
    procedure SetF2FilTDataModuleUnitNamePrefix(const Value: string);
    procedure SetF2FilTDataModuleUnitNameSuffix(const Value: string);
    procedure SetF2FilTImageListFieldName(const Value: string);
    function GetF2FilDragAndDropExportTDataModule: boolean;
    function GetF2FilDragAndDropExportTImageList: boolean;
    function GetF2FilExportChoosenFolder: string;
    function GetF2FilExportCPPBuilderUnit: boolean;
    function GetF2FilExportDelphiUnit: boolean;
    function GetF2FilFillTImageListDestinationProperty: boolean;
    function GetF2FilFolderToImport: string;
    function GetF2FilImportJPEG: boolean;
    function GetF2FilImportPNG: boolean;
    function GetF2FilSizeSeparatorChar: string;
    function GetF2FilTDataModuleTypeName: string;
    function GetF2FilTDataModuleTypeNamePrefix: string;
    function GetF2FilTDataModuleTypeNameSuffix: string;
    function GetF2FilTDataModuleUnitName: string;
    function GetF2FilTDataModuleUnitNamePrefix: string;
    function GetF2FilTDataModuleUnitNameSuffix: string;
    function GetF2FilTImageListFieldName: string;
  protected
  public
    property F2FilFolderToImport: string read GetF2FilFolderToImport
      write SetF2FilFolderToImport;
    property F2FilSizeSeparatorChar: string read GetF2FilSizeSeparatorChar
      write SetF2FilSizeSeparatorChar;
    property F2FilImportJPEG: boolean read GetF2FilImportJPEG
      write SetF2FilImportJPEG;
    property F2FilImportPNG: boolean read GetF2FilImportPNG
      write SetF2FilImportPNG;
    property F2FilTImageListFieldName: string read GetF2FilTImageListFieldName
      write SetF2FilTImageListFieldName;
    property F2FilFillTImageListDestinationProperty: boolean
      read GetF2FilFillTImageListDestinationProperty
      write SetF2FilFillTImageListDestinationProperty;
    property F2FilTDataModuleUnitName: string read GetF2FilTDataModuleUnitName
      write SetF2FilTDataModuleUnitName;
    property F2FilTDataModuleUnitNamePrefix: string
      read GetF2FilTDataModuleUnitNamePrefix
      write SetF2FilTDataModuleUnitNamePrefix;
    property F2FilTDataModuleUnitNameSuffix: string
      read GetF2FilTDataModuleUnitNameSuffix
      write SetF2FilTDataModuleUnitNameSuffix;
    property F2FilTDataModuleTypeName: string read GetF2FilTDataModuleTypeName
      write SetF2FilTDataModuleTypeName;
    property F2FilTDataModuleTypeNamePrefix: string
      read GetF2FilTDataModuleTypeNamePrefix
      write SetF2FilTDataModuleTypeNamePrefix;
    property F2FilTDataModuleTypeNameSuffix: string
      read GetF2FilTDataModuleTypeNameSuffix
      write SetF2FilTDataModuleTypeNameSuffix;
    property F2FilExportChoosenFolder: string read GetF2FilExportChoosenFolder
      write SetF2FilExportChoosenFolder;
    property F2FilExportDelphiUnit: boolean read GetF2FilExportDelphiUnit
      write SetF2FilExportDelphiUnit;
    property F2FilExportCPPBuilderUnit: boolean
      read GetF2FilExportCPPBuilderUnit write SetF2FilExportCPPBuilderUnit;
    property F2FilDragAndDropExportTImageList: boolean
      read GetF2FilDragAndDropExportTImageList
      write SetF2FilDragAndDropExportTImageList;
    property F2FilDragAndDropExportTDataModule: boolean
      read GetF2FilDragAndDropExportTDataModule
      write SetF2FilDragAndDropExportTDataModule;
  end;

implementation

{ TConfigHelpers }

function TConfigHelpers.GetF2FilDragAndDropExportTDataModule: boolean;
begin
  result := GetParams.getValue('F2FilDragAndDropExportTDataModule', true);
end;

function TConfigHelpers.GetF2FilDragAndDropExportTImageList: boolean;
begin
  result := GetParams.getValue('F2FilDragAndDropExportTImageList', false);
end;

function TConfigHelpers.GetF2FilExportChoosenFolder: string;
begin
  result := GetParams.getValue('F2FilExportChoosenFolder', '');
end;

function TConfigHelpers.GetF2FilExportCPPBuilderUnit: boolean;
begin
  result := GetParams.getValue('F2FilExportCPPBuilderUnit', true);
end;

function TConfigHelpers.GetF2FilExportDelphiUnit: boolean;
begin
  result := GetParams.getValue('F2FilExportDelphiUnit', true);
end;

function TConfigHelpers.GetF2FilFillTImageListDestinationProperty: boolean;
begin
  result := GetParams.getValue('F2FilFillTImageListDestinationProperty', true);
end;

function TConfigHelpers.GetF2FilFolderToImport: string;
begin
  result := GetParams.getValue('F2FilFolderToImport', '');
end;

function TConfigHelpers.GetF2FilImportJPEG: boolean;
begin
  result := GetParams.getValue('F2FilImportJPEG', false);
end;

function TConfigHelpers.GetF2FilImportPNG: boolean;
begin
  result := GetParams.getValue('F2FilImportPNG', true);
end;

function TConfigHelpers.GetF2FilSizeSeparatorChar: string;
begin
  result := GetParams.getValue('F2FilSizeSeparatorChar', '@');
end;

function TConfigHelpers.GetF2FilTDataModuleTypeName: string;
begin
  result := GetParams.getValue('F2FilTDataModuleTypeName', '');
end;

function TConfigHelpers.GetF2FilTDataModuleTypeNamePrefix: string;
begin
  result := GetParams.getValue('F2FilTDataModuleTypeNamePrefix', '');
end;

function TConfigHelpers.GetF2FilTDataModuleTypeNameSuffix: string;
begin
  result := GetParams.getValue('F2FilTDataModuleTypeNameSuffix', '');
end;

function TConfigHelpers.GetF2FilTDataModuleUnitName: string;
begin
  result := GetParams.getValue('F2FilTDataModuleUnitName', '');
end;

function TConfigHelpers.GetF2FilTDataModuleUnitNamePrefix: string;
begin
  result := GetParams.getValue('F2FilTDataModuleUnitNamePrefix', '');
end;

function TConfigHelpers.GetF2FilTDataModuleUnitNameSuffix: string;
begin
  result := GetParams.getValue('F2FilTDataModuleUnitNameSuffix', '');
end;

function TConfigHelpers.GetF2FilTImageListFieldName: string;
begin
  result := GetParams.getValue('F2FilTImageListFieldName', '');
end;

procedure TConfigHelpers.SetF2FilDragAndDropExportTDataModule
  (const Value: boolean);
begin
  GetParams.setValue('F2FilDragAndDropExportTDataModule', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilDragAndDropExportTImageList
  (const Value: boolean);
begin
  GetParams.setValue('F2FilDragAndDropExportTImageList', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilExportChoosenFolder(const Value: string);
begin
  GetParams.setValue('F2FilExportChoosenFolder', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilExportCPPBuilderUnit(const Value: boolean);
begin
  GetParams.setValue('F2FilExportCPPBuilderUnit', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilExportDelphiUnit(const Value: boolean);
begin
  GetParams.setValue('F2FilExportDelphiUnit', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilFillTImageListDestinationProperty
  (const Value: boolean);
begin
  GetParams.setValue('F2FilFillTImageListDestinationProperty', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilFolderToImport(const Value: string);
begin
  GetParams.setValue('F2FilFolderToImport', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilImportJPEG(const Value: boolean);
begin
  GetParams.setValue('F2FilImportJPEG', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilImportPNG(const Value: boolean);
begin
  GetParams.setValue('F2FilImportPNG', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilSizeSeparatorChar(const Value: string);
begin
  GetParams.setValue('F2FilSizeSeparatorChar', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleTypeName(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleTypeName', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleTypeNamePrefix(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleTypeNamePrefix', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleTypeNameSuffix(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleTypeNameSuffix', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleUnitName(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleUnitName', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleUnitNamePrefix(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleUnitNamePrefix', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTDataModuleUnitNameSuffix(const Value: string);
begin
  GetParams.setValue('F2FilTDataModuleUnitNameSuffix', Value);
  GetParams.Save;
end;

procedure TConfigHelpers.SetF2FilTImageListFieldName(const Value: string);
begin
  GetParams.setValue('F2FilTImageListFieldName', Value);
  GetParams.Save;
end;

end.
