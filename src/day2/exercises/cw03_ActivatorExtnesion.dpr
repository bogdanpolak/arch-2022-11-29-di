program cw03_ActivatorExtnesion;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Spring.Container,
  Spring.Container.Common,
  Spring.Container.ActivatorExtension,
  Spring;

type
  IService = interface
    ['{CDF3322E-90FC-4F26-943B-0F7CB3F9E42F}']
    procedure Run();
  end;

  TService = class(TInterfacedObject, IService)
    procedure Run();
  end;

  IRoot = interface
    ['{25C8D44F-B0A4-4380-895E-E8827765CDBF}']
    procedure Run();
  end;

  TRoot = class(TInterfacedObject, IRoot)
  private
    fService: IService;
  public
    constructor Create(const aService: IService);
    procedure Run();
  end;

{ TRoot }

constructor TRoot.Create(const aService: IService);
begin
  fService := aService;
  writeln(Format('TRoot.Create - aService=%p',[Pointer(aService)]));
end;

procedure TRoot.Run;
begin
  fService.Run();
end;

{ TService }

procedure TService.Run;
begin
  writeln('TService.Run');
end;

{ Demo }

procedure RunDemo();
var
  root: IRoot;
begin
  GlobalContainer.RegisterType<TRoot>();
  GlobalContainer.RegisterType<TService>();

  // GlobalContainer.AddExtension<TActivatorContainerExtension>();
  // More information:
  // https://stackoverflow.com/questions/71424345/spring4d-should-not-call-inherited-constructor-if-not-all-parameters-can-be-reso

  GlobalContainer.Build();
  root := GlobalContainer.Resolve<IRoot>();
  writeln('IRoot was resolved');
  root.Run;
end;

begin
  try
    RunDemo();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

