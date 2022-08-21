unit %%UnitName%%;

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
