// Optimized DateTime routines for speed
// David Berneda @davidberneda July-2016
unit Tee.FastDateTime;

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
  public
    class function DayOf(const DateTime: TTimeStamp): Word; overload; static;
    class function DayOf(const DateTime: TDateTime): Word; overload; inline; static;

    class function MonthOf(const DateTime: TTimeStamp): Word; overload; static;
    class function MonthOf(const DateTime: TDateTime): Word; overload; inline; static;

    class function YearOf(const DateTime: TTimeStamp): Word; overload; static;
    class function YearOf(const DateTime: TDateTime): Word; overload; inline; static;
  end;

implementation

const
  MonthDays: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  LeapMonthDays: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

procedure DivMod(Dividend: Integer; Divisor: Word; out Result, Remainder: Word); inline;
begin
  Result    := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

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

class function TFastDateTime.MonthOf(const DateTime: TTimeStamp): Word;
var
  Y, D, I: Word;
  T: Integer;
begin
  T := DateTime.Date;

  if T <= 0 then
     result := 0
  else
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

    result := 1;

    if IsLeapYear(Y) then
    begin
      while True do
      begin
        I := LeapMonthDays[result];
        if D < I then Break;
        Dec(D, I);
        Inc(result);
      end;
    end
    else
    begin
      while True do
      begin
        I := MonthDays[result];
        if D < I then Break;
        Dec(D, I);
        Inc(result);
      end;
    end;

  end;
end;

class function TFastDateTime.DayOf(const DateTime: TTimeStamp): Word;
var
  Y, M, I: Word;
  T: Integer;
begin
  T := DateTime.Date;

  if T <= 0 then
     result := 0
  else
  begin
    Dec(T);

    Y := 1;

    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;

    DivMod(T, D100, I, result);

    if I = 4 then
    begin
      Dec(I);
      Inc(result, D100);
    end;

    Inc(Y, I * 100);
    DivMod(result, D4, I, result);

    Inc(Y, I * 4);
    DivMod(result, D1, I, result);

    if I = 4 then
    begin
      Dec(I);
      Inc(result, D1);
    end;

    Inc(Y, I);

    M := 1;

    if IsLeapYear(Y) then
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
end;

class function TFastDateTime.DayOf(const DateTime: TDateTime): Word;
begin
  result:=DayOf(DateTimeToTimeStamp(DateTime));
end;

class function TFastDateTime.MonthOf(const DateTime: TDateTime): Word;
begin
  result:=MonthOf(DateTimeToTimeStamp(DateTime));
end;

class function TFastDateTime.YearOf(const DateTime: TDateTime): Word;
begin
  result:=YearOf(DateTimeToTimeStamp(DateTime));
end;

end.
