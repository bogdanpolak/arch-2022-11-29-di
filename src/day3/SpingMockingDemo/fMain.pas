unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Spring.Mocking;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    Memo1: TMemo;
    btnMock: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnMockClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  ISample1 = interface(IInvokable)
    function GetInt(const s: string): integer;
  end;

var
  Form1: TForm1;

implementation

uses
  Rtti,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.DesignPatterns,
  Spring.Interception,
  Spring.Mocking.Matching,
  Spring.Times;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Align := alClient;
  Memo1.Clear;
end;

procedure SetupMock(const m: Mock<ISample1>; const data: TArray<TArray<variant>>);
var
  i: Integer;
begin
  for i := 0 to Length(data)-1 do
    m.Setup().Returns<integer>(data[i][1]).When.GetInt(data[i][0]);
end;

procedure TForm1.btnMockClick(Sender: TObject);
var
  m: Mock<ISample1>;
  sample: ISample1;
begin
  sample := m;
  Memo1.Lines.Add(Format('Default returns: %d', [sample.GetInt('')]));
  m.Setup().Returns<integer>(-1).When(Args.Any).GetInt('');
  Memo1.Lines.Add(Format('GetInt("book") = %d', [sample.GetInt('book')]));
  m.Setup().Returns<integer>(10).When.GetInt('ten');
  Memo1.Lines.Add(Format('GetInt("ten") = %d', [sample.GetInt('ten')]));
  m.Setup().Returns<integer>(0).When().GetInt(Arg.IsIn<string>(['zero','','null']));
  Memo1.Lines.Add(Format('GetInt("null") = %d', [sample.GetInt('null')]));
  SetupMock(m, [['one',1],['two',2],['three',3]]);
  Memo1.Lines.Add(Format('GetInt("two") = %d', [sample.GetInt('two')]));
end;

end.
