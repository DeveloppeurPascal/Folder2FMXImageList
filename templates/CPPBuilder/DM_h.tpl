//---------------------------------------------------------------------------

#ifndef %%UnitName%%
#define %%UnitName%%
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.ImgList.hpp>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class %%DMType%% : public TDataModule
{
__published:
	TImageList *%%ImageListName%%;
private:
public:
	__fastcall %%DMType%%(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE %%DMType%% *%%DMName%%;
//---------------------------------------------------------------------------
#endif
