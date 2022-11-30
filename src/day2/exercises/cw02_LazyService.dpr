program cw02_LazyService;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Spring,
  Spring.Container;

type
  INow = interface
    ['{933FB19C-80B7-4484-9047-7110A66AAE19}']
    function GetNow(): TDateTime;
  end;

  TNow = class(TInterfacedObject, INow)
    function GetNow(): TDateTime;
  end;

  IRarelyUsedFeature = interface
    ['{9722C609-5444-4054-B544-24D30DE8B3B3}']
    function TimePassed(const aDate: TDateTime): TTimeSpan;
  end;

  TRarelyUsedFeature = class(TInterfacedObject, IRarelyUsedFeature)
  private
    fNow: INow;
  public
    constructor Create(const aNow: INow);
    function TimePassed(const aDate: TDateTime): TTimeSpan;
  end;

  IRoot = interface
    ['{8C7B55F5-59D6-45CB-AEBC-EADD4C5CD763}']
    procedure RunRarelyUsedFeature();
  end;

  TRoot = class(TInterfacedObject, IRoot)
  private
    fRarelyUsedFeature: Lazy<IRarelyUsedFeature>;
  public
    constructor Create(const aRarelyUsedFeature: Lazy<IRarelyUsedFeature>);
    procedure RunRarelyUsedFeature();
  end;

  { TNow }

function TNow.GetNow: TDateTime;
begin
  Result := Now();
end;

{ TRarelyUsedFeature }

constructor TRarelyUsedFeature.Create(const aNow: INow);
begin
  fNow := aNow;
  writeln('TRarelyUsedFeature.Create');
end;

function TRarelyUsedFeature.TimePassed(const aDate: TDateTime): TTimeSpan;
begin
  Result := TTimeSpan.Subtract(Now, aDate);
end;

{ TRoot }

constructor TRoot.Create(const aRarelyUsedFeature: Lazy<IRarelyUsedFeature>);
begin
  fRarelyUsedFeature := aRarelyUsedFeature;
end;

procedure TRoot.RunRarelyUsedFeature;
var
  span: TTimeSpan;
begin
  writeln('We are going to resolve IRarelyUsedFeature');
  span := fRarelyUsedFeature.Value.TimePassed(EncodeDate(2022, 10, 25) +
    EncodeTime(16, 0, 0, 0));
  Writeln(Format('Passes %d days %d hours',[span.Days,span.Hours]));
end;

{ Demo }

procedure RunDemo();
var
  root: IRoot;
begin
  GlobalContainer.RegisterType<TRoot>();
  GlobalContainer.RegisterType<TRarelyUsedFeature>();
  GlobalContainer.RegisterType<TNow>();
  GlobalContainer.Build();
  writeln('We are going to resolve IRoot');
  root := GlobalContainer.Resolve<IRoot>();
  writeln('...... lot of other code ......');
  root.RunRarelyUsedFeature();
end;

begin
  try
    RunDemo();
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;
end.

