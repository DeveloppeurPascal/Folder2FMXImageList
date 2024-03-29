﻿unit %%UnitName%%;
{
  Unit generated by folder2FMXImageList the %%datetime%%

  Don't change it if you want to refresh the images by the program :
  it will erase all your changes !

  ******************************************************************************
  * Folder2FMXImageList
  ******************************************************************************
  (c) Patrick Prémartin - Olf Software

  Source : https://github.com/DeveloppeurPascal/Folder2FMXImageList
  Issue : https://github.com/DeveloppeurPascal/Folder2FMXImageList/issues
  New features : https://github.com/DeveloppeurPascal/Folder2FMXImageList/discussions
  Download : https://folder2fmximagelist.olfsoftware.fr/
}
interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

type
  T%%DMName%% = class(TDataModule)
    %%ImageListName%%: TImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  %%DMName%%: T%%DMName%%;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
