﻿unit ServiceTest;

interface

uses
  System.SysUtils,
  System.TimeSpan,
  DUnitX.TestFramework,
  Service,
  Spring.Mocking;

type

  [TestFixture]
  TMyTestObject = class
  private
    fSut: TRarelyUsedFeature;
    fNowMock: Mock<INow>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TimePassed;
  end;

implementation

type
  Day = integer;

  TDayHelper = record helper for Day
    function June(year: word): TDateTime;
    function November(year: word): TDateTime;
    function December(year: word): TDateTime;
  end;

  TDateTimeHelper = record helper for TDateTime
    function At(
      hours: word;
      minutes: word): TDateTime;
  end;

procedure TMyTestObject.Setup;
begin
  fSut := TRarelyUsedFeature.Create(fNowMock);
end;

procedure TMyTestObject.TearDown;
begin
  fSut.Free();
end;

procedure TMyTestObject.TimePassed;
var
  aNowDateTime: TDateTime;
  TimeSpan: TTimeSpan;
begin
  fNowMock.Setup().Returns<TDateTime>(Day(1).December(2022).At(16, 0))
    .When.GetNow();

  TimeSpan := fSut.TimePassed(Day(30).November(2022).At(12, 0));

  Assert.AreEqual(1, TimeSpan.Days);
  Assert.AreEqual(4, TimeSpan.hours);
  Assert.AreEqual(0, TimeSpan.minutes);
end;

{ TDayHelper }

function TDayHelper.December(year: word): TDateTime;
begin
  Result := EncodeDate(year, 12, self);
end;

function TDayHelper.June(year: word): TDateTime;
begin
  Result := EncodeDate(year, 6, self);
end;

function TDayHelper.November(year: word): TDateTime;
begin
  Result := EncodeDate(year, 11, self);
end;

{ TDateTimeHelper }

function TDateTimeHelper.At(hours, minutes: word): TDateTime;
begin
  Result := self + EncodeTime(hours, minutes, 0, 0);
end;

initialization

TDUnitX.RegisterTestFixture(TMyTestObject);

end.
