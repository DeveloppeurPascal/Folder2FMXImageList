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
  %%DMType%% = class(TDataModule)
    %%ImageListName%%: TImageList;
  private
  public
  end;

var
  %%DMName%%: %%DMType%%;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
