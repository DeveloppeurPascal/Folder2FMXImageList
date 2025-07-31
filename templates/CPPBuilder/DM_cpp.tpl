//---------------------------------------------------------------------------

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

#pragma hdrstop

#include "%%UnitName%%.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "FMX.Controls.TControl"
#pragma resource "*.dfm"
%%DMType%% *%%DMName%%;
//---------------------------------------------------------------------------
__fastcall %%DMType%%::%%DMType%%(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
