## Zad.1) Kontroler, klasy Transient oraz Singleton

```
  IService = interface
    function WhoAmI(): string;
  end;

  TCompositionRoot = class
    constructor Create(const aService: IService);
    function GetService(): IService;
  end;
```

1. Przygotowanie typów
   - Dodaj brakujące idetyfikatory GUID (sktót: `Shift`+`Control`+`D`)
   - Dodaj klasę `TService`, która implementuje interfejs `IService`
   - Do `TService` dodaj konstruktor, który ma zainicjować "w miarę” unikalną tekstową nazwa obiektu (sposób dowolny: losowa liczba, time stamp, GUID, etc.). Nazwę trzeba zapisać w prywatnym polu i zwrócić jako wynik metody `WhoAmI`
   - W klasie `TCompositionRoot` Zaimplementuj konstruktor `Create` i metodę `GetService`. Jak widać w deklaracji powyżej `IService` ma być wstrzykiwany przez konstruktor do `TCompositionRoot`, czyli trzeba go przechować w prywatnym polu
1. Rejestracja typów
   - Zarejestruj `TService` w `GlobalContainer` (lub w nowo stworzonym kontenerze DI typu `TContainer`)
   - Zarejestruj `TCompositionRoot` w tym samym kontenerze
1. Resolve
   - Zrób 2x resolve na `TCompositionRoot` i porównaj nazwy dzieci wywołując metody `WhoAmI`. Sprawdź czy zwracane nazwy są takie same czy różne
   - Sprawdź wycieki pamięci `ReportMemoryLeaksOnShutdown := true;`
   - Jeśli masz wycieki pamięci spróbuj je usunąć
1. Dodatkowe próby / ekspaerymenty
   1. Zarejestruj `TService` jako Singleton i porównaj wyniki
   2. Zarejestruj `TCompositionRoot` jako Singleton i porównaj wyniki, jeżeli pojawiły się błędy pamięci w czasie uruchomienia to spróbuj je zlikwidować
1. Omów wnioski i różnice z innymi uczestnikami szkolenia

## Zad.2) Lotto Draw Machine

```
type
  ILottoDrawMachine = interface
    function GetLuckyNumbers: TArray<string>;
  end;

  IMachineFactory = interface(IInvokable)
    function BuildDrawMachine(
      const slBalls: TStringList;
      const aNumberOfBalls: Integer): ILottoDrawMachine;
  end;

  IGame = interface
    procedure RunGame();
  end;
```

1. Dodaj brakujące idetyfikatory GUID
1. Implementacja i weryfikacja klasy `TLottoDrawMachine`
   - Parametry konstruktora: `aNumberOfBalls` zawiera informację ile kul trzeba wylosować, `slBalls` jest obiektem `TStringList`, który zawiera listę kul, każda linia w liście to osobna kula. Zapisana tekstem, np. kula 11 to `'11'`
   - Stwórz implementację interfejsu `ILottoDrawMachine`, która w konstruktorze miesza kule z `slBalls` i wybiera `aNumberOfBalls` zwycięskich kul
   - Metoda `GetLuckyNumbers` ma zwrócić zwycięskie kule jako `TArray<string>`
   - Kule powinny być usuwane z obiektu `slBalls` i przed każdym wywołaniem ponownie inicjowane wartościami początkowymi (1, 2, 3 … 47)
   - Przetestuj obiekt klasy `TLottoDrawMachine` i sprawdź czy `GetLuckyNumbers` zwraca poprawne wyniki - szczęśliwe kule
1. Rejestracja i użycie fabryki
   - Zarejestruj klasę `TLottoDrawMachine` oraz interfejs `IMachineFactory` jako fabrykę `AsFactory` w kontenerze DI. Nie twórz implementacji interfejsu `IMachineFactory`.
   - Przetestuj `Resolve<IMachineFactory>` i sprawdź czy metoda `BuildDrawMachine` dostarcza w pełni funkcjonalną instancję Lotto Draw Machine.
1. Implementacja klasy `TGame`:
   - Fabryka `IMachineFactory` ma być wstrzykiwana przez konstruktor do klasy `TGame`
   - Metoda `RunGame` ma przy pomocy wstrzykniętej fabryki stworzyć nową `ILottoDrawMachine`, wywołać metodę `GetLuckyNumbers` i  wyświetlić zwrócone przez `GetLuckyNumbers` zwycięskie numery na konsoli `Console.Out()`
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
