// Optimized DateTime routines for speed
// David Berneda @davidberneda July-2016
unit Tee.FastDateTime;

// Important for speed:
// When not debugging, always compile this unit with Optimization ON,
// Inlining ON, and Range-checking, Overflow-checking, Assertions OFF
// {$O+,C-,R-,Q-}

interface

uses
  System.SysUtils;

type
  TFastDateTime=record
  private
    const
      D1 = 365;
      D4 = D1 * 4 + 1;
      D100 = D4 * 25 - 1;
      D400 = D100 * 4 + 1;

    class function CalcLeap(T: Integer; out D:Word): Boolean; static;
    class function DayMonth(const T:Integer; out M:Byte): Word; overload; static;
  public
    class function DayOf(const DateTime: TTimeStamp): Word; overload; static;
    class function DayOf(const DateTime: TDateTime): Word; overload; inline; static;

    class function MonthOf(const DateTime: TTimeStamp): Byte; overload; static;
    class function MonthOf(const DateTime: TDateTime): Byte; overload; inline; static;

    class function YearOf(const DateTime: TTimeStamp): Word; overload; static;
    class function YearOf(const DateTime: TDateTime): Word; overload; inline; static;
  end;

implementation

uses
  System.Math;

// Optimized from DateUtils.pas
class function TFastDateTime.YearOf(const DateTime: TTimeStamp): Word;
var
  D, I: Word;
  T: Integer;
begin
  T := DateTime.Date;

  if T <= 0 then
     result:= 0
  else
  begin
    Dec(T);
    result := 1;

    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(result, 400);
    end;

    DivMod(T, D100, I, D);

    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;

    Inc(result, I * 100);
    DivMod(D, D4, I, D);

    Inc(result, I * 4);

    I := D div D1;

    if I = 4 then
       Dec(I);

    Inc(result, I);
  end;
end;

class function TFastDateTime.CalcLeap(T: Integer; out D:Word): Boolean;
var I,
    Y : Word;
begin
  Dec(T);

  Y := 1;

  while T >= D400 do
  begin
    Dec(T, D400);
    Inc(Y, 400);
  end;

  DivMod(T, D100, I, D);

  if I = 4 then
  begin
    Dec(I);
    Inc(D, D100);
  end;

  Inc(Y, I * 100);
  DivMod(D, D4, I, D);
  Inc(Y, I * 4);
  DivMod(D, D1, I, D);

  if I = 4 then
  begin
    Dec(I);
    Inc(D, D1);
  end;

  Inc(Y, I);

  result:=IsLeapYear(Y);
end;

class function TFastDateTime.DayMonth(const T: Integer; out M:Byte): Word;
type
  TDayTable = array[1..12] of Byte;

const
  MonthDays: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  LeapMonthDays: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

var I: Byte;
begin
  M:=1;

  if CalcLeap(T,result) then
  begin
    while True do
    begin
      I := LeapMonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
  end
  else
  begin
    while True do
    begin
      I := MonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
  end;

  Inc(result);
end;

class function TFastDateTime.MonthOf(const DateTime: TTimeStamp): Byte;
var T: Integer;
begin
  T:=DateTime.Date;

  if T<=0 then
     result:=0
  else
     DayMonth(T,result);
end;

class function TFastDateTime.DayOf(const DateTime: TTimeStamp): Word;
var T: Integer;
    M: Byte;
begin
  T:=DateTime.Date;

  if T<=0 then
     result:=0
  else
     result:=DayMonth(T,M);
end;

class function TFastDateTime.DayOf(const DateTime: TDateTime): Word;
begin
  result:=DayOf(DateTimeToTimeStamp(DateTime));
end;

class function TFastDateTime.MonthOf(const DateTime: TDateTime): Byte;
begin
  result:=MonthOf(DateTimeToTimeStamp(DateTime));
end;

class function TFastDateTime.YearOf(const DateTime: TDateTime): Word;
begin
  result:=YearOf(DateTimeToTimeStamp(DateTime));
end;

end.
