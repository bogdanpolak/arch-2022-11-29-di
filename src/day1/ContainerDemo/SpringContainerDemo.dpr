﻿program SpringContainerDemo;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {FormMain},
  ConsoleForVcl in 'ConsoleForVcl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
