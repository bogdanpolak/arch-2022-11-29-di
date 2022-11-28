unit ConsoleForVcl;

interface

uses
  System.SysUtils,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TDemoFormInitializer = class
    class procedure InitilizeDemoForm(
      const aTopPanel: TPanel;
      const aFontSize: integer = 10);
  end;

type
  IConsole = interface(IInvokable)
  ['{EB55A4AA-1F41-436E-9883-D769ED7CEFE2}']
    procedure OutSeparator(
      const aBaseCharacter: char = '-';
      const aLength: integer = 72);
    procedure Out (const aMessage: string); overload;
    procedure Out (
      const aMessage: string;
      const aFormatArgs: array of const); overload;
  end;

  TVclConsole = class(TInterfacedObject, IConsole)
  private
    fMemo: TMemo;
  public
    constructor Create(aMemo: TMemo);
    procedure OutSeparator(
      const aBaseCharacter: char = '-';
      const aLength: integer = 72);
    procedure Out (const aMessage: string); overload;
    procedure Out (
      const aMessage: string;
      const aFormatArgs: array of const); overload;
  end;

var
  Console: IConsole;

implementation

class procedure TDemoFormInitializer.InitilizeDemoForm(
  const aTopPanel: TPanel;
  const aFontSize: integer = 10);
var
  idx: integer;
  mainMemo: TMemo;
begin
  mainMemo := TMemo.Create(aTopPanel.Parent);
  Console := TVclConsole.Create(mainMemo);
  with aTopPanel do
  begin
    BevelOuter := bvLowered;
    Align := alTop;
    AlignWithMargins := True;
    Height := 38;
    Caption := '';
    for idx := 0 to ControlCount - 1 do
    begin
      Controls[idx].Align := alLeft;
      Controls[idx].AlignWithMargins := True;
    end;
  end;
  with TBevel.Create(aTopPanel.Parent) do
  begin
    Align := alTop;
    Shape := bsSpacer;
    Height := 5;
  end;
  with mainMemo do
  begin
    Name := 'MainMemo';
    Parent := aTopPanel.Parent;
    Align := alClient;
    AlignWithMargins := True;
    Font.Name := 'Consolas';
    Font.Size := aFontSize;
    ScrollBars := ssVertical;
    Clear;
  end;
end;

{ TVclConsole }

constructor TVclConsole.Create(aMemo: TMemo);
begin
  fMemo := aMemo;
end;

procedure TVclConsole.Out(const aMessage: string);
begin
  fMemo.Lines.Add(aMessage);
end;

procedure TVclConsole.Out(
  const aMessage: string;
  const aFormatArgs: array of const);
begin
  fMemo.Lines.Add(Format(aMessage, aFormatArgs));
end;

procedure TVclConsole.OutSeparator(
  const aBaseCharacter: char;
  const aLength: integer);
begin
  Out(StringOfChar(aBaseCharacter, aLength));
end;

end.
