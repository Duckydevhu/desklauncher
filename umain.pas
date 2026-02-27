unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, IniFiles, ShellAPI, LCLIntf, LCLType, Windows, uSettings, StdCtrls,
  dwmapi, FileUtil, LazUTF8;

type

  { TFrmBar }

  TFrmBar = class(TForm)
    MenuItem1: TMenuItem;    // Beállítások menüpont a tálca ikonhoz
    MenuItem2: TMenuItem;    // Kilépés menüpont a tálca ikonhoz
    PnlIcons: TPanel;       // A fő konténer panel, ezen helyezkednek el az ikonok
    PopupMenu1: TPopupMenu;  // Jobb klikk menü a tálca ikonhoz
    TrayIcon1: TTrayIcon;    // Az óra melletti értesítési ikon komponense
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PnlIconsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Dinamikus tömbök az objektumok tárolására a kódból való eléréshez }
    Images: array[1..12] of TImage;
    Labels: array[1..12] of TLabel;
    { Segédeljárás az ablak lekerekítéséhez }
    procedure MakeRounded(Control: TWinControl; Rounding: Integer);
    { Windows üzenetkezelő a regisztrált globális gyorsbillentyűhöz }
    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;
    { Beállítások betöltése fájlból }
    procedure LoadSettings;
    { Közös eseménykezelő az ikonokra való kattintáshoz }
    procedure IconClick(Sender: TObject);
  public

  end;

var
  FrmBar: TFrmBar;

implementation

{$R *.lfm}

{ TFrmBar }

procedure TFrmBar.FormCreate(Sender: TObject);
var
  i: Integer;
  TotalIconsWidth: Integer;
  StartLeft: Integer;
  IconW: Integer = 32; // Ikon szélessége pixelben
  Gap: Integer = 24;   // Ikonok közötti távolság pixelben
begin
  { Kiszámoljuk az összes ikon és köz szélességét a középre igazításhoz }
  TotalIconsWidth := (12 * IconW) + (11 * Gap);
  { Meghatározzuk az első ikon kezdőpozícióját a panelen belül }
  StartLeft := (PnlIcons.Width - TotalIconsWidth) div 2;

  { Az ablakot vízszintesen középre, függőlegesen a képernyő tetejére pozicionáljuk }
  Self.Left := (Screen.Width - Self.Width) div 2;
  Self.Top := 0;

  { Ciklus a 12 darab Image és Label dinamikus létrehozásához }
  for i := 1 to 12 do
  begin
    { Kép (Ikon) létrehozása és beállítása }
    Images[i] := TImage.Create(Self);
    Images[i].Parent := PnlIcons;
    Images[i].Width := IconW;
    Images[i].Height := IconW;
    Images[i].Top := 10;
    { Pozíció kiszámítása az index alapján }
    Images[i].Left := StartLeft + ((i - 1) * (IconW + Gap));

    Images[i].Stretch := True;      // Kép nyújtása a mérethez
    Images[i].Proportional := True; // Arányok megtartása
    Images[i].Center := True;       // Középre igazítás a kereten belül
    Images[i].Tag := i;             // Eltároljuk az indexet a későbbi azonosításhoz
    Images[i].OnClick := @IconClick;// Hozzárendeljük a kattintás eseményt
    Images[i].Cursor := crHandPoint; // Kurzor megváltoztatása (mutató kéz)

    { Felirat (Label) létrehozása és beállítása }
    Labels[i] := TLabel.Create(Self);
    Labels[i].Parent := PnlIcons;
    Labels[i].AutoSize := False;
    Labels[i].Width := IconW + 12; // Kicsit szélesebb, mint az ikon
    Labels[i].Height := 26;
    Labels[i].Top := 55;
    { A feliratot az ikon alá és középre igazítjuk }
    Labels[i].Left := Images[i].Left - ((Labels[i].Width - Images[i].Width) div 2);
    Labels[i].Alignment := taCenter;
    Labels[i].Font.Size := 7;
    Labels[i].WordWrap := True;     // Szövegtörés engedélyezése
    Labels[i].Tag := i;             // Index eltárolása
    Labels[i].OnClick := @IconClick;// Kattintás esemény hozzárendelése
    Labels[i].Cursor := crHandPoint;
  end;

  LoadSettings; // Beállítások betöltése indításkor

  { A korábbi fix gyorsbillentyű regisztráció kikommentelve, már a LoadSettings kezeli }
{  if not RegisterHotKey(Handle, 1, 1, VK_SPACE) then
  begin
    ShowMessage('Hiba: Nem sikerült lefoglalni a gyorsbillentyűt! Próbálj másikat.');
  end;
  }

  Self.Visible := False; // Indításkor maradjon rejtve az ablak
end;

procedure TFrmBar.FormDeactivate(Sender: TObject);
begin
  { Ha az ablak elveszíti a fókuszt (máshova kattintunk), elrejtjük }
  Self.Hide;
end;

procedure TFrmBar.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Idx: Integer;
begin
  { ESC gombra elrejtjük az ablakot }
  if Key = VK_ESCAPE then
  begin
    Self.Hide;
    Exit;
  end;

  { F1-F12 billentyűk figyelése a megfelelő program indításához }
  if (Key >= VK_F1) and (Key <= VK_F12) then
  begin
    { Kiszámoljuk az indexet (F1=1, F2=2, stb.) }
    Idx := Key - VK_F1 + 1;

    { Szimuláljuk az objektum átadását az IconClick-nek az index segítségével }
    Images[Idx].Tag := Idx;
    IconClick(Images[Idx]);
  end;
end;

procedure TFrmBar.FormShow(Sender: TObject);
var
  DWM_Policy: Integer;
  DWM_Attr: Integer;
begin
  { Az ablak fix méreteinek beállítása megjelenítéskor }
  Self.Width := 730;
  Self.Height := 90;

  { Pozicionálás felülre középre }
  Self.Left := (Screen.Width - Self.Width) div 2;
  Self.Top := 0;

  { Ablak lekerekítése }
  MakeRounded(Self, 20);

  { Windows DWM (Desktop Window Manager) beállítások az árnyék kényszerítéséhez bsNone stílusnál }
  DWM_Policy := 2; // DWMNCRP_ENABLED
  DWM_Attr := 2;   // DWMWA_NCRENDERING_POLICY
  DwmSetWindowAttribute(Handle, DWM_Attr, @DWM_Policy, sizeof(DWM_Policy));

  { Windows 11 lekerekítési preferencia beállítása (Corner Preference = 33) }
  DWM_Policy := 2; // DWMWCP_ROUND
  DwmSetWindowAttribute(Handle, 33, @DWM_Policy, sizeof(DWM_Policy));

  { Ablak előtérbe hozása és fókusz kérése }
  Windows.SetForegroundWindow(Self.Handle);
  Self.SetFocus;
end;

procedure TFrmBar.MenuItem1Click(Sender: TObject);
begin
  { Beállítások ablak megnyitása és az adatok újratöltése, ha OK-val zárták be }
  if FrmSettings.ShowModal = mrOk then
  begin
    LoadSettings;
  end;
end;

procedure TFrmBar.MenuItem2Click(Sender: TObject);
begin
  { Alkalmazás teljes bezárása }
  Application.Terminate;
end;

procedure TFrmBar.PnlIconsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { Kattintáskor biztosítjuk, hogy az ablak kapja meg a fókuszt }
  Self.SetFocus;
end;

procedure TFrmBar.WMHotKey(var Msg: TMessage);
begin
  { Globális gyorsbillentyű esemény kezelése (id=1) }
  if Msg.WParam = 1 then
  begin
    if Self.Visible then
      Self.Hide // Ha látszik, elrejtjük
    else
    begin
      { Ha nem látszik, megjelenítjük és az előtérbe kényszerítjük }
      Self.Show;
      ShowWindow(Self.Handle, SW_SHOW);
      SetForegroundWindow(Self.Handle);
      Self.SetFocus;
    end;
  end;
end;

procedure TFrmBar.LoadSettings;
var
  Ini: TIniFile;
  i: Integer;
  ExePath: string;
  SFI: TSHFileInfoW; // Windows Shell fájlinformáció struktúra (Unicode)
  IconHandle: HICON;
  BgColorStr, TextColorStr: string;
  IsTransparent: Boolean;
  HotMod, HotKeyIndex: Integer;
  WinMod, WinKey: UINT;
begin
  { INI fájl megnyitása a program futtatható fájlja melletti mappából }
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  try
    { Vizuális alapbeállítások beolvasása alapértelmezett értékekkel }
    BgColorStr := Ini.ReadString('Visual', 'BgColor', ColorToString(clBlack));
    TextColorStr := Ini.ReadString('Visual', 'TextColor', ColorToString(clWhite));
    IsTransparent := Ini.ReadBool('Visual', 'Transparent', False);

    { Gyorsbillentyű módosító és alapgomb beolvasása }
    HotMod := Ini.ReadInteger('Hotkey', 'Modifier', 0);
    HotKeyIndex := Ini.ReadInteger('Hotkey', 'Key', 0);

    { Módosító billentyű (Alt, Ctrl, stb.) kódjának meghatározása }
    case HotMod of
      0: WinMod := MOD_ALT;
      1: WinMod := MOD_CONTROL;
      2: WinMod := MOD_SHIFT;
      3: WinMod := MOD_CONTROL or MOD_ALT;
      else WinMod := MOD_ALT;
    end;

    { A gomb (Space, Enter, stb.) kódjának meghatározása }
    case HotKeyIndex of
      0: WinKey := VK_SPACE;
      1: WinKey := VK_RETURN;
      else WinKey := VK_SPACE;
    end;

    { Meglévő gyorsbillentyű törlése és az új regisztrálása }
    UnregisterHotKey(Handle, 1);
    if not RegisterHotKey(Handle, 1, WinMod, WinKey) then
      ShowMessage('Hiba: Ez a billentyűkombináció már foglalt!');

    { Ablak színeinek beállítása }
    Self.Color := StringToColor(BgColorStr);
    PnlIcons.Color := Self.Color;

    { Átlátszóság (AlphaBlend) beállítása }
    if IsTransparent then
    begin
      Self.AlphaBlend := True;
      Self.AlphaBlendValue := 220;
    end
    else
    begin
      Self.AlphaBlend := False;
    end;

    { Ciklus a 12 programhely adatainak beolvasásához }
    for i := 1 to 12 do
    begin
      ExePath := Ini.ReadString('Apps', 'F' + IntToStr(i), '');

      { Ha van megadott útvonal és a fájl létezik, lekérjük az ikonját }
      if (ExePath <> '') and FileExists(ExePath) then
      begin
        FillChar(SFI, SizeOf(SFI), 0);
        { A Windows Shell API-t használjuk az ikon kinyeréséhez }
        if SHGetFileInfoW(PWideChar(UnicodeString(ExePath)), 0, SFI, SizeOf(SFI),
           SHGFI_ICON or SHGFI_LARGEICON) <> 0 then
        begin
          Images[i].Picture.Icon.Handle := SFI.hIcon;
        end
        else
          Images[i].Picture := nil;
      end
      else
        Images[i].Picture := nil;

      { Felirat és betűszín beállítása }
      Labels[i].Caption := Ini.ReadString('Captions', 'F' + IntToStr(i), '');
      Labels[i].Font.Color := StringToColor(TextColorStr);
    end;
  finally
    Ini.Free; // INI objektum felszabadítása
  end;
end;

procedure TFrmBar.MakeRounded(Control: TWinControl; Rounding: Integer);
var
  Rgn: HRGN;
begin
  { Windows GDI függvény használata lekerekített téglalap régió létrehozásához }
  Rgn := CreateRoundRectRgn(0, 0, Control.Width, Control.Height, Rounding, Rounding);
  { A régió hozzárendelése az ablakhoz (levágja a sarkokat) }
  SetWindowRgn(Control.Handle, Rgn, True);
end;

procedure TFrmBar.IconClick(Sender: TObject);
var
  Idx: Integer;
  Ini: TIniFile;
  ExePath, ExeArgs, ExeDir: string;
begin
  { Megállapítjuk az indexet a Tag alapján }
  Idx := TControl(Sender).Tag;
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  try
    { Adatok beolvasása: útvonal és indítási paraméterek }
    ExePath := Ini.ReadString('Apps', 'F' + IntToStr(Idx), '');
    ExeArgs := Ini.ReadString('Args', 'F' + IntToStr(Idx), '');

    if (ExePath <> '') and FileExists(ExePath) then
    begin
      { Munkakönyvtár meghatározása az EXE mappájaként }
      ExeDir := ExtractFilePath(ExePath);

      { Alkalmazás indítása Unicode támogatással }
      { ShellExecuteW paraméterei: Ablak, Művelet, Fájl, Paraméterek, Munkakönyvtár, Megjelenítés }
      ShellExecuteW(0, 'open',
                    PWideChar(UnicodeString(ExePath)),
                    PWideChar(UnicodeString(ExeArgs)),
                    PWideChar(UnicodeString(ExeDir)),
                    SW_SHOWNORMAL);

      Self.Hide; // Indítás után elrejtjük a launchert
    end;
  finally
    Ini.Free;
  end;
end;

end.
