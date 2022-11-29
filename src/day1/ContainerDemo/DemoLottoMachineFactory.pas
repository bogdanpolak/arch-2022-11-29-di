unit DemoLottoMachineFactory;

interface

uses
  System.SysUtils,
  System.Classes,
  Spring.Container,
  ConsoleForVcl;

type
  TDemoLottoMachineFactory = class
    class procedure Run(const aContainer: TContainer);
  end;

implementation

type
  ILottoDrawMachine = interface
    ['{23A22610-C401-4F8B-A607-19928DFAAE95}']
    function GetLuckyNumbers: TArray<string>;
  end;

  IMachineFactory = interface(IInvokable)
    ['{097F08AB-0A1C-4E53-A250-763AF5EA64A3}']
    function BuildDrawMachine(
      const slBalls: TStringList;
      const aNumberOfBalls: Integer): ILottoDrawMachine;
  end;

  IGame = interface
    ['{E16A562E-8D65-4915-A52D-C8074DCD0FEE}']
    procedure RunGame();
  end;

  TLottoDrawMachine = class(TInterfacedObject, ILottoDrawMachine)
  private
    fLuckyNumbers: TArray<string>;
  public
    constructor Create(
      const slBalls: TStringList;
      const aNumberOfBalls: Integer);
    function GetLuckyNumbers: TArray<string>;
  end;

  TGame = class(TInterfacedObject, IGame)
  private
    fMachineFactory: IMachineFactory;
    function InitializeBalls(): TStringList;
  public
    constructor Create(const aMachineFactory: IMachineFactory);
    procedure RunGame();
  end;

  { TDemoLottoMachineFactory }

class procedure TDemoLottoMachineFactory.Run(const aContainer: TContainer);
var
  game: IGame;
begin
  aContainer.RegisterType<ILottoDrawMachine, TLottoDrawMachine>();
  aContainer.RegisterType<IMachineFactory>.AsFactory();
  aContainer.RegisterType<IGame, TGame>();
  aContainer.Build;
  game := aContainer.Resolve<IGame>();
  game.RunGame();
end;

{ TLottoDrawMachine }

constructor TLottoDrawMachine.Create(
  const slBalls: TStringList;
  const aNumberOfBalls: Integer);
var
  step: Integer;
  ballIdx: Integer;
begin
  step := 0;
  SetLength(fLuckyNumbers, aNumberOfBalls);
  repeat
    ballIdx := random(slBalls.Count);
    fLuckyNumbers[step] := slBalls[ballIdx];
    slBalls.Delete(ballIdx);
    step := step + 1;
  until step >= aNumberOfBalls;
end;

function TLottoDrawMachine.GetLuckyNumbers: TArray<string>;
begin
  Result := fLuckyNumbers;
end;

{ TGame }

constructor TGame.Create(const aMachineFactory: IMachineFactory);
begin
  fMachineFactory := aMachineFactory;
end;

function TGame.InitializeBalls: TStringList;
const
  MAX_BALL = 47;
var
  ball: Integer;
begin
  Result := TStringList.Create;
  for ball := 1 to MAX_BALL do
    Result.Add(ball.ToString);
end;

procedure TGame.RunGame;
var
  balls: TStringList;
  lottoMachine: ILottoDrawMachine;
  numbers: TArray<string>;
begin
  balls := InitializeBalls();
  try
    lottoMachine := fMachineFactory.BuildDrawMachine(balls, 6);
  finally
    balls.Free;
  end;
  numbers := lottoMachine.GetLuckyNumbers();
  Console.Out('Lucky numbers %s', [string.Join(', ', numbers)]);
end;

end.
