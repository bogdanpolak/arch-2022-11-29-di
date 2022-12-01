unit Service;

interface

uses
  System.SysUtils,
  System.TimeSpan;

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

implementation

{ TNow }

function TNow.GetNow: TDateTime;
begin
  Result := Now();
end;

{ TRarelyUsedFeature }

constructor TRarelyUsedFeature.Create(const aNow: INow);
begin
  fNow := aNow;
end;

function TRarelyUsedFeature.TimePassed(const aDate: TDateTime): TTimeSpan;
begin
  Result := TTimeSpan.Subtract(fNow.GetNow, aDate);
end;

end.
