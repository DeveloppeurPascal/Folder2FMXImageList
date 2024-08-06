unit %%UnitName%%;

// ****************************************
// * Images from folder :
// * %%ImportFolder%%
// ****************************************
//
// This file contains a TDataModule with a
// TImageList to use in a FireMonkey project.
//
// ****************************************
// File generator : %%AboutCaption%%
// Website : %%AboutURL%%
// Generation date : %%datetime%%
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  FMX.ImgList;

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
