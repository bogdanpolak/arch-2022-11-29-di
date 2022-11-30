unit DemoDelegateTo;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  Spring.Container,
  Spring.Container.Common,
  ConsoleForVcl;

type
  TDemoDelegateTo = class
    class procedure Run(const aContainer: TContainer);
  end;

implementation

type
  TWeather = record
    TemperatureC: Double;
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

  { TDemoDelegateTo }

procedure ComposeContainer(
  const aContainer: TContainer;
  const aScale: TTemperatureScale);
begin
  aContainer.RegisterType<TTemperatureService>.DelegateTo(
    function: TTemperatureService
    begin
      Result := TTemperatureService.Create(aScale,
        aContainer.Resolve<IWeatherAPI>)
    end);
  aContainer.RegisterType<TWeatherAPI>;
  aContainer.RegisterType<TWeatherApiOptions>.DelegateTo(
    function: TWeatherApiOptions
    begin
      Result.Token := '--super-secure-token--';
      Result.BaseUrl := 'https://weather.org/api';
    end);
  aContainer.Build;
end;

class procedure TDemoDelegateTo.Run(const aContainer: TContainer);
var
  defaultScale: TTemperatureScale;
  temperatureService: ITemperatureService;
  location: string;
begin
  defaultScale := tsFahrenheit;
  ComposeContainer(aContainer, defaultScale);
  temperatureService := aContainer.Resolve<ITemperatureService>();
  location := 'poland/warsaw';
  Console.Out('Temprature at location %s is %s',[location,
    temperatureService.Get(location)]);
end;

{ TWeatherAPI }

function TWeatherAPI.GetWeather(const location: string): TWeather;
begin
  Console.Out('GET %s?token=%s',[fWeatherApiOptions.BaseUrl,
    fWeatherApiOptions.Token]);
  Result.TemperatureC := random(10) - 1;
end;

{ TTemperatureService }

constructor TTemperatureService.Create(
  const aScale: TTemperatureScale;
  const aWeatherAPI: IWeatherAPI);
begin
  fScale := aScale;
  fWeatherAPI := aWeatherAPI;
end;

function TTemperatureService.Get(const location: string): string;
var
  weather: TWeather;
begin
  weather := fWeatherAPI.GetWeather(location);
  Result := IfThen(fScale = tsCelsius, Format('%.1f°C', [weather.TemperatureC]),
    Format('%.1f°F', [weather.TemperatureC * 1.8 + 32]));
end;

end.
