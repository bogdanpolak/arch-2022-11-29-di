unit TestDouble.StaticNow;

interface

uses
  Service;

type
  TStaticNow = class(TInterfacedObject, INow)
  private
    fStaticNow: TDateTime;
  public
    procedure Initialize(const aStaticNow: TDateTime);
    function GetNow(): TDateTime;
  end;

implementation

{ TStaticNow }

function TStaticNow.GetNow: TDateTime;
begin
  Result := fStaticNow;
end;

procedure TStaticNow.Initialize(const aStaticNow: TDateTime);
begin
  fStaticNow := aStaticNow;
end;

end.
