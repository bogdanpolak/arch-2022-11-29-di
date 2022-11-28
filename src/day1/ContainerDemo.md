## Zad.1. Kontroler, klasy Transient oraz Singleton

```
  IService = interface
    function WhoAmI(): string;
  end;

  TService = class(TInterfacedObject, IService)
    constructor Create;          // ustaw fId: Word;
    function WhoAmI(): string;   // zwróc ID
  end;

  TCompositionRoot = class
    constructor Create(const aService: IService);
    function GetChild(): IService;
  end;
```

1. Zarejestruj `TCompositionRoot` jako Singleton
1. Resolve: 2x `TCompositionRoot` i porównaj ID dzieci
1. Sprawdź wycieki pamięci (`ReportMemoryLeaksOnShutdown`)
1. Zarejestruj `TCompositionRoot` jako Transient a `TService` jako Singleton
1. Porównaj i omów różnice

## Zad.2. Lotto Draw Machine

```
type
  ILottoDrawMachine = interface
    function GetLuckyNumbers: TArray<string>;
  end;

  IMachineFactory = interface(IInvokable)
    function GetMachine(
      const slBalls: TStringList;
      const aBallsNumber: Integer): ILottoDrawMachine;
  end;

  IGame = interface
    procedure RunGame();
  end;
```

1. Stwórz implementację `ILottoDrawMachine`, która generuje w konsrutorze miesza kule z `slBalls` i wybiera zwycięskich 6 kul
1. Metoda `GetLuckyNumbers` ma zwrócić zwycięskie kule
1. Metoda `RunGame` ma wyświetlić na konsoli zwycięskie numery
1. Nie twórz implenetacji interfejsu `IMachineFactory` ale zarejestruj go jako Factory
1. Kule powinny być usuwane ze StringListy i za każdym razem ponownie inicjowane
1. Proszę unikać wycieków pamięci

## Zad.3. Ćwiczenie z użyciem DelegateTo()

```
type
  TWeather = record
    Temperature: Double;
  end;

  IWeatherAPI = interface(IInvokable)
    ['{E8AF0977-3299-4B32-BA29-732842231CD0}']
    function GetWeather(const location: string): TWeather;
  end;

  TWeatherApiOptions = record
    Token: string;
    BaseUrl: string;
  end;

  TWeatherAPI = class(TInterfacedObject, IWeatherAPI)
  private
    [Inject]
    fWeatherApiOptions: TWeatherApiOptions;
  public
    function GetWeather(const location: string): TWeather;
  end;

  ITemperatureService = interface
    ['{DF7DDE54-780C-453B-BF8A-C5A23A04DDBE}']
    function Get(const location: string): string;
  end;

  TTemperatureScale = (tsCelsius, tsFahrenheit);

  TTemperatureService = class(TInterfacedObject, ITemperatureService)
  private
    fScale: TTemperatureScale;
    fWeatherAPI: IWeatherAPI;
  public
    constructor Create(
      const aScale: TTemperatureScale;
      const aWeatherAPI: IWeatherAPI);
    function Get(const location: string): string;
  end;
```

## Przypominające materiały szkoleniowe

![](/resources/container01.png)
![](/resources/container02.png)
![](/resources/container03.png)
![](/resources/container04.png)
![](/resources/container05.png)
![](/resources/container06.png)
