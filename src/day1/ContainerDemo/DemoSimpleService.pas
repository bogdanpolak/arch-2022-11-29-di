unit DemoSimpleService;

interface

uses
  System.SysUtils,
  Spring.Container,
  ConsoleForVcl;

type
  TDemoSimpleService = class
    class procedure Run(const aContainer: TContainer);
  end;

implementation

type
   IService = interface
    ['{1FA2C997-8831-4C5F-B233-B497966ABA8D}']
    function WhoAmI(): string;
  end;

  TService = class(TInterfacedObject, IService)
  private
    fName: string;
  public
    constructor Create;
    function WhoAmI(): string;
  end;

  TCompositionRoot = class
  private
    fService: IService;
  public
    constructor Create(const aService: IService);
    function GetService(): IService;
  end;

{ TDemoSimpleService }

class procedure TDemoSimpleService.Run(const aContainer: TContainer);
var
  root: TCompositionRoot;
begin
  aContainer.RegisterType<TService>();
  aContainer.RegisterType<TCompositionRoot>();
  aContainer.Build();
  root := aContainer.Resolve<TCompositionRoot>();
  try
    Console.Out('Service name: %s',[root.GetService().WhoAmI()]);
  finally
    root.Free;
  end;
  ReportMemoryLeaksOnShutdown := true;
end;

{ TService }

constructor TService.Create;
begin
  fName := DateTimeToStr(Now);
end;

function TService.WhoAmI: string;
begin
  Result := fName;
end;

{ TCompositionRoot }

constructor TCompositionRoot.Create(const aService: IService);
begin
  fService := aService;
end;

function TCompositionRoot.GetService: IService;
begin
  Result := fService;
end;

end.
