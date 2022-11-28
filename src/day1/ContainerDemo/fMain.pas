unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Spring.Container,
  Spring.Collections;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
  private
    fContainers: IList<TContainer>;
    function CreateNewContainer(): TContainer;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  ConsoleForVcl;

{$R *.dfm}

function TFormMain.CreateNewContainer: TContainer;
begin
  Result := TContainer.Create;
  fContainers.Add(Result);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  fContainers := TCollections.CreateList<TContainer>(true);
  TDemoFormInitializer.InitilizeDemoForm(Panel1);
end;

end.
