unit uSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  IniFiles, Registry;

type

  { TFrmSettings }

  TFrmSettings = class(TForm)
    { --- UI Komponensek deklarációja --- }
    BtnBrowse1: TButton;
    BtnBrowse10: TButton;
    BtnBrowse11: TButton;
    BtnBrowse12: TButton;
    BtnBrowse2: TButton;
    BtnBrowse3: TButton;
    BtnBrowse4: TButton;
    BtnBrowse5: TButton;
    BtnBrowse6: TButton;
    BtnBrowse7: TButton;
    BtnBrowse8: TButton;
    BtnBrowse9: TButton;
    BtnSave: TButton;
    Caption10: TEdit;
    Caption11: TEdit;
    Caption12: TEdit;
    Caption2: TEdit;
    Caption3: TEdit;
    Caption4: TEdit;
    Caption5: TEdit;
    Caption6: TEdit;
    Caption7: TEdit;
    Caption8: TEdit;
    Caption9: TEdit;
    CheckTrans: TCheckBox;
    chkAutostart: TCheckBox;
    ColorBtnBg: TColorButton;
    ColorBtnText: TColorButton;
    ComboMod: TComboBox;
    ComboKey: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Caption1: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Params1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    Params10: TEdit;
    Params11: TEdit;
    Params12: TEdit;
    Params2: TEdit;
    Params3: TEdit;
    Params4: TEdit;
    Params5: TEdit;
    Params6: TEdit;
    Params7: TEdit;
    Params8: TEdit;
    Params9: TEdit;
    { --- Eseménykezelők deklarációja --- }
    procedure BtnBrowse10Click(Sender: TObject);
    procedure BtnBrowse11Click(Sender: TObject);
    procedure BtnBrowse12Click(Sender: TObject);
    procedure BtnBrowse1Click(Sender: TObject);
    procedure BtnBrowse2Click(Sender: TObject);
    procedure BtnBrowse3Click(Sender: TObject);
    procedure BtnBrowse4Click(Sender: TObject);
    procedure BtnBrowse5Click(Sender: TObject);
    procedure BtnBrowse6Click(Sender: TObject);
    procedure BtnBrowse7Click(Sender: TObject);
    procedure BtnBrowse8Click(Sender: TObject);
    procedure BtnBrowse9Click(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FrmSettings: TFrmSettings;

implementation

{$R *.lfm}

{ TFrmSettings }

procedure TFrmSettings.BtnSaveClick(Sender: TObject);
var
  Ini: TIniFile;
  i: Integer;
  CompEdit, CompCap, CompParam: TComponent;
  Reg: TRegistry;
begin
  { --- 1. Automatikus indítás (Autostart) beállítása a Registry-ben --- }
  Reg := TRegistry.Create;
  try
    { A Windows Registry aktuális felhasználóra vonatkozó gyökérkönyvtára }
    Reg.RootKey := HKEY_CURRENT_USER;
    { Megnyitjuk a Windows automatikus indításért felelős kulcsát (Run) }
    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run', True) then
    begin
      if chkAutostart.Checked then
        { Ha a checkbox be van pipálva, beírjuk az EXE útvonalát a Registry-be }
        Reg.WriteString('DDLauncher', ParamStr(0))
      else
        { Ha nincs bepipálva, töröljük a bejegyzést, így a program nem indul el a Windows-zal }
        Reg.DeleteValue('DDLauncher');
      Reg.CloseKey;
    end;
  finally
    Reg.Free; // Registry objektum felszabadítása
  end;

  { --- 2. Beállítások mentése az INI fájlba --- }
  { Létrehozzuk az INI objektumot a program melletti mappában }
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  try
    { Ciklus a 12 programhely mentéséhez }
    for i := 1 to 12 do
    begin
      { Dinamikus komponenskeresés név alapján (pl. Edit1, Edit2...) }
      CompEdit := FindComponent('Edit' + IntToStr(i));
      if (CompEdit <> nil) and (CompEdit is TEdit) then
        Ini.WriteString('Apps', 'F' + IntToStr(i), TEdit(CompEdit).Text);

      { Feliratok mentése (Caption1, Caption2...) }
      CompCap := FindComponent('Caption' + IntToStr(i));
      if (CompCap <> nil) and (CompCap is TEdit) then
         Ini.WriteString('Captions', 'F' + IntToStr(i), TEdit(CompCap).Text);

      { Indítási paraméterek mentése (Params1, Params2...) }
      CompParam := FindComponent('Params' + IntToStr(i));
      if (CompParam <> nil) and (CompParam is TEdit) then
         Ini.WriteString('Args', 'F' + IntToStr(i), TEdit(CompParam).Text);
    end;

    { Vizuális beállítások mentése (Színek és Átlátszóság) }
    Ini.WriteString('Visual', 'BgColor', ColorToString(ColorBtnBg.ButtonColor));
    Ini.WriteString('Visual', 'TextColor', ColorToString(ColorBtnText.ButtonColor));
    Ini.WriteBool('Visual', 'Transparent', CheckTrans.Checked);

    { A ShowModal hívás visszatérési értékének beállítása, hogy a főablak tudja: mentés történt }
    ModalResult := mrOk;

    { Gyorsbillentyű választók (ComboBox) indexének mentése }
    Ini.WriteInteger('Hotkey', 'Modifier', ComboMod.ItemIndex);
    Ini.WriteInteger('Hotkey', 'Key', ComboKey.ItemIndex);

    ShowMessage('Beállítások elmentve!');
  finally
    Ini.Free; // INI objektum felszabadítása
  end;
end;

procedure TFrmSettings.FormShow(Sender: TObject);
var
  Ini: TIniFile;
  i: Integer;
  Comp: TComponent;
  Reg: TRegistry;
begin
  { --- 1. Autostart állapotának ellenőrzése megnyitáskor --- }
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    { Csak olvasásra nyitjuk meg a Registry kulcsot }
    if Reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Run') then
    begin
      { Ha létezik a bejegyzés, bepipáljuk a checkboxot a felhasználó tájékoztatására }
      chkAutostart.Checked := Reg.ValueExists('DDLauncher');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  { --- 2. Korábbi adatok visszatöltése az INI fájlból a mezőkbe --- }
  try
    for i := 1 to 12 do
    begin
      { Útvonalak visszatöltése (Edit1-12) }
      Comp := FindComponent('Edit' + IntToStr(i));
      if (Comp <> nil) and (Comp is TEdit) then
        TEdit(Comp).Text := Ini.ReadString('Apps', 'F' + IntToStr(i), '');

      { Paraméterek visszatöltése (Params1-12) }
      Comp := FindComponent('Params' + IntToStr(i));
      if (Comp <> nil) and (Comp is TEdit) then
        TEdit(Comp).Text := Ini.ReadString('Args', 'F' + IntToStr(i), '');

      { Feliratok visszatöltése (Caption1-12) }
      Comp := FindComponent('Caption' + IntToStr(i));
      if (Comp <> nil) and (Comp is TEdit) then
        TEdit(Comp).Text := Ini.ReadString('Captions', 'F' + IntToStr(i), '');
    end;

    { Színek és átlátszóság visszatöltése (alapértelmezett értékekkel) }
    ColorBtnBg.ButtonColor := StringToColor(Ini.ReadString('Visual', 'BgColor', ColorToString(clBlack)));
    ColorBtnText.ButtonColor := StringToColor(Ini.ReadString('Visual', 'TextColor', ColorToString(clWhite)));
    CheckTrans.Checked := Ini.ReadBool('Visual', 'Transparent', False);

    { Gyorsbillentyű választók indexének visszatöltése }
    ComboMod.ItemIndex := Ini.ReadInteger('Hotkey', 'Modifier', 0);
    ComboKey.ItemIndex := Ini.ReadInteger('Hotkey', 'Key', 0);

  finally
    Ini.Free;
  end;
end;

{ --- Tallózás (Browse) gombok eseménykezelői --- }
{ Megnyitják a fájlválasztó ablakot, majd a kiválasztott útvonalat beírják a megfelelő Edit mezőbe }

procedure TFrmSettings.BtnBrowse1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit1.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit2.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit3.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse4Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit4.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse5Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit5.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse6Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit6.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse7Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit7.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse8Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit8.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse9Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit9.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse10Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit10.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse11Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit11.Text := OpenDialog1.FileName;
end;

procedure TFrmSettings.BtnBrowse12Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit12.Text := OpenDialog1.FileName;
end;

end.
