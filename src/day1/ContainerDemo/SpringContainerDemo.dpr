program SpringContainerDemo;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {FormMain},
  ConsoleForVcl in 'ConsoleForVcl.pas',
  DemoSimpleService in 'DemoSimpleService.pas',
  DemoLottoMachineFactory in 'DemoLottoMachineFactory.pas',
  DemoDelegateTo in 'DemoDelegateTo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
