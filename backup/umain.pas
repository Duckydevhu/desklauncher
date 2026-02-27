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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PnlIcons: TPanel;
    PopupMenu1: TPopupMenu;
    TrayIcon1: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PnlIconsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Images: array[1..12] of TImage;
    Labels: array[1..12] of TLabel;
    procedure MakeRounded(Control: TWinControl; Rounding: Integer);
    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;
    procedure LoadSettings;
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
  IconW: Integer = 32;
  Gap: Integer = 24;
begin
  TotalIconsWidth := (12 * IconW) + (11 * Gap);
  StartLeft := (PnlIcons.Width - TotalIconsWidth) div 2;


  Self.Left := (Screen.Width - Self.Width) div 2;
  Self.Top := 0;


  for i := 1 to 12 do
  begin

    Images[i] := TImage.Create(Self);
    Images[i].Parent := PnlIcons;
    Images[i].Width := IconW;
    Images[i].Height := IconW;
    Images[i].Top := 10;

    Images[i].Left := StartLeft + ((i - 1) * (IconW + Gap));

    Images[i].Stretch := True;
    Images[i].Proportional := True;
    Images[i].Center := True;
    Images[i].Tag := i;
    Images[i].OnClick := @IconClick;
    Images[i].Cursor := crHandPoint;


    Labels[i] := TLabel.Create(Self);
    Labels[i].Parent := PnlIcons;
    Labels[i].AutoSize := False;
    Labels[i].Width := IconW + 12;
    Labels[i].Height := 26;
    Labels[i].Top := 55;

    Labels[i].Left := Images[i].Left - ((Labels[i].Width - Images[i].Width) div 2);
    Labels[i].Alignment := taCenter;
    Labels[i].Font.Size := 7;
    Labels[i].WordWrap := True;
    Labels[i].Tag := i;
    Labels[i].OnClick := @IconClick;
    Labels[i].Cursor := crHandPoint;
  end;

  LoadSettings;
{  if not RegisterHotKey(Handle, 1, 1, VK_SPACE) then
  begin
    ShowMessage('Hiba: Nem sikerült lefoglalni a gyorsbillentyűt! Próbálj másikat.');
  end;
 }

  Self.Visible := False;
end;

procedure TFrmBar.FormDeactivate(Sender: TObject);
begin
  Self.Hide;
end;

procedure TFrmBar.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Idx: Integer;
begin
  if Key = VK_ESCAPE then
  begin
    Self.Hide;
    Exit;
  end;

  if (Key >= VK_F1) and (Key <= VK_F12) then
  begin
    Idx := Key - VK_F1 + 1;

    Images[Idx].Tag := Idx;
    IconClick(Images[Idx]);
  end;
end;

procedure TFrmBar.FormShow(Sender: TObject);
var
  DWM_Policy: Integer;
  DWM_Attr: Integer;
begin

  Self.Width := 730;
  Self.Height := 90;

  Self.Left := (Screen.Width - Self.Width) div 2;
  Self.Top := 0;

  MakeRounded(Self, 20);



  DWM_Policy := 2;
  DWM_Attr := 2;

  DwmSetWindowAttribute(Handle, DWM_Attr, @DWM_Policy, sizeof(DWM_Policy));



  DWM_Policy := 2;
  DwmSetWindowAttribute(Handle, 33, @DWM_Policy, sizeof(DWM_Policy));


  Windows.SetForegroundWindow(Self.Handle);
  Self.SetFocus;
end;

procedure TFrmBar.MenuItem1Click(Sender: TObject);
begin

  if FrmSettings.ShowModal = mrOk then
  begin

    LoadSettings;
  end;
end;

procedure TFrmBar.MenuItem2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFrmBar.PnlIconsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  Self.SetFocus;
end;

procedure TFrmBar.WMHotKey(var Msg: TMessage);
begin
  if Msg.WParam = 1 then
  begin
    if Self.Visible then
      Self.Hide
    else
    begin

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
  SFI: TSHFileInfoW;
  IconHandle: HICON;
  BgColorStr, TextColorStr: string;
  IsTransparent: Boolean;
  HotMod, HotKeyIndex: Integer;
  WinMod, WinKey: UINT;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  try
    BgColorStr := Ini.ReadString('Visual', 'BgColor', ColorToString(clBlack));
    TextColorStr := Ini.ReadString('Visual', 'TextColor', ColorToString(clWhite));
    IsTransparent := Ini.ReadBool('Visual', 'Transparent', False);

    HotMod := Ini.ReadInteger('Hotkey', 'Modifier', 0);
    HotKeyIndex := Ini.ReadInteger('Hotkey', 'Key', 0);


    case HotMod of
      0: WinMod := MOD_ALT;
      1: WinMod := MOD_CONTROL;
      2: WinMod := MOD_SHIFT;
      3: WinMod := MOD_CONTROL or MOD_ALT;
      else WinMod := MOD_ALT;
    end;


    case HotKeyIndex of
      0: WinKey := VK_SPACE;
      1: WinKey := VK_RETURN;

      else WinKey := VK_SPACE;
    end;


    UnregisterHotKey(Handle, 1);
    if not RegisterHotKey(Handle, 1, WinMod, WinKey) then
      ShowMessage('Hiba: Ez a billentyűkombináció már foglalt!');


    Self.Color := StringToColor(BgColorStr);
    PnlIcons.Color := Self.Color;


    if IsTransparent then
    begin
      Self.AlphaBlend := True;
      Self.AlphaBlendValue := 220;
    end
    else
    begin
      Self.AlphaBlend := False;
    end;

    for i := 1 to 12 do
    begin
      ExePath := Ini.ReadString('Apps', 'F' + IntToStr(i), '');

      if (ExePath <> '') and FileExists(ExePath) then
      begin

        FillChar(SFI, SizeOf(SFI), 0);
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


      Labels[i].Caption := Ini.ReadString('Captions', 'F' + IntToStr(i), '');
      Labels[i].Font.Color := StringToColor(TextColorStr);

    end;
  finally
    Ini.Free;
  end;
end;

procedure TFrmBar.MakeRounded(Control: TWinControl; Rounding: Integer);
var
  Rgn: HRGN;
begin



  Rgn := CreateRoundRectRgn(0, 0, Control.Width, Control.Height, Rounding, Rounding);


  SetWindowRgn(Control.Handle, Rgn, True);
end;

procedure TFrmBar.IconClick(Sender: TObject);
var
  Idx: Integer;
  Ini: TIniFile;
  ExePath, ExeArgs, ExeDir: string;
begin
  Idx := TControl(Sender).Tag;
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'settings.ini');
  try
    ExePath := Ini.ReadString('Apps', 'F' + IntToStr(Idx), '');

    ExeArgs := Ini.ReadString('Args', 'F' + IntToStr(Idx), '');

    if (ExePath <> '') and FileExists(ExePath) then
    begin
      ExeDir := ExtractFilePath(ExePath);





      ShellExecuteW(0, 'open',
                    PWideChar(UnicodeString(ExePath)),
                    PWideChar(UnicodeString(ExeArgs)),
                    PWideChar(UnicodeString(ExeDir)),
                    SW_SHOWNORMAL);

      Self.Hide;
    end;
  finally
    Ini.Free;
  end;
end;

end.

