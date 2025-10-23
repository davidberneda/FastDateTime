// Optimized DateTime routines for speed
// David Berneda @davidberneda July-2016
unit Tee.FastDateTime;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

// Important for speed:
// When not debugging, always compile this unit with Optimization ON,
// Inlining ON, and Range-checking, Overflow-checking, Assertions OFF
// {$O+,C-,R-,Q-}

// Enable or disable fast methods
{$DEFINE FASTDATE}

{$IFDEF FASTDATE}

 {$IFDEF CPUX64}

  // 64bit

  {$IFDEF FPC}
   {$UNDEF FASTDATE} // FPC 64bit RTL is faster !
  {$ELSE}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
   {$DEFINE FASTDAYOFYEAR}
   {$DEFINE FASTYEAROF}
  {$ENDIF}
 {$ELSE}

  // 32bit

  {$IFNDEF FPC}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
  {$ENDIF}

  {$DEFINE FASTDAYOFYEAR}
  {$DEFINE FASTYEAROF}
{$ENDIF}

{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF};

// Speed optimization, use only the Date part of timestamp
{$DEFINE DATESTAMP}

// Speed optimization, use lookup tables instead of loops
{$DEFINE LOOKUP}

type
  TFastDateTime=record
  private
    {$IFDEF FASTDATE}
    const
      D1 = 365;
      D4 = D1 * 4 + 1;
      D100 = D4 * 25 - 1;
      D400 = D100 * 4 + 1;

    class function CalcDayOfYear(T: Integer; out Y:Word): Word; static;
    class function CalcLeap(T: Integer; out D:Word): Boolean; static;

    {$IFDEF DATESTAMP}
    class function DateTimeToDateStamp(const DateTime: TDateTime): Integer; static; inline;
    {$ENDIF}

    class function DayMonth(const T:Integer; out M:Byte): Word; overload; static;

    class function DayOf(const Date: Integer): Word; overload; static;
    class function DayOfTheYear(const Date: Integer): Word; overload; static;
    class function MonthOf(const Date: Integer): Byte; overload; static;
    class function YearOf(Date: Integer): Word; overload; static;
    {$ENDIF}
  public
    {$IFDEF FASTDATE}
    class function DayOf(const DateTime: TTimeStamp): Word; overload; inline; static;
    class function DayOfTheYear(const DateTime: TTimeStamp):Word; overload; inline; static;
    class function MonthOf(const DateTime: TTimeStamp): Byte; overload; inline; static;
    class function YearOf(const DateTime: TTimeStamp): Word; overload; inline; static;
    {$ENDIF}

    class function DayOf(const DateTime: TDateTime): Word; overload; inline; static;
    class function DayOfTheYear(const DateTime: TDateTime):Word; overload; inline; static;
    class function MonthOf(const DateTime: TDateTime): Byte; overload; inline; static;
    class function YearOf(const DateTime: TDateTime): Word; overload; inline; static;
  end;

implementation

uses
  {$IFDEF FPC}Math, DateUtils{$ELSE}System.Math{$ENDIF};

{$IFDEF FASTDATE}

// Optimized from DateUtils.pas
class function TFastDateTime.YearOf(Date: Integer): Word;
var
  D, I: Word;
begin
  if Date <= 0 then
     result:= 0
  else
  begin
    Dec(Date);
    result := 1;

    while Date >= D400 do
    begin
      Dec(Date, D400);
      Inc(result, 400);
    end;

    DivMod(Date, D100, I, D);

    if I>0 then
    begin
      if I = 4 then
      begin
        Dec(I);
        Inc(D, D100);
      end;

      Inc(result, I * 100);
    end;

    DivMod(D, D4, I, D);

    if I>0 then
       Inc(result, I * 4);

    I := D div D1;

    if I>0 then
    begin
      if I = 4 then
         Dec(I);

      Inc(result, I);
    end;
  end;
end;

class function TFastDateTime.YearOf(const DateTime: TTimeStamp): Word;
begin
  result:=YearOf(DateTime.Date);
end;

class function TFastDateTime.CalcDayOfYear(T: Integer; out Y: Word): Word;
var I : Word;
begin
  Dec(T);

  //Y:=400*(T div D400);
  //T:=T mod D400;

  Y := 1;

  while T >= D400 do
  begin
    Dec(T, D400);
    Inc(Y, 400);
  end;

  DivMod(T, D100, I, result);

  if I>0 then
  begin
    if I = 4 then
    begin
      Dec(I);
      Inc(result, D100);
    end;

    Inc(Y, I * 100);
  end;

  DivMod(result, D4, I, result);

  if I>0 then
     Inc(Y, I * 4);

  DivMod(result, D1, I, result);

  if I>0 then
  begin
    if I = 4 then
    begin
      Dec(I);
      Inc(result, D1);
    end;

    Inc(Y, I);
  end;
end;

class function TFastDateTime.CalcLeap(T: Integer; out D:Word): Boolean;
var Y : Word;
begin
  D:=CalcDayOfYear(T,Y);
  result:=IsLeapYear(Y);
end;

{$IFDEF LOOKUP}
type
  TLeapLookup=record
  public
    const Month:Array[0..365] of Byte=
      (
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
        11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
        12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
      );

    const Day:Array[0..365] of Byte=
      (
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
      );
  end;

  TLookup=record
  public
    const Month:Array[0..364] of Byte=
      (
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
        11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
        12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
      );

    const Day:Array[0..364] of Byte=
      (
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
      );
  end;

{$ENDIF}

class function TFastDateTime.DayMonth(const T: Integer; out M:Byte): Word;
{$IFNDEF LOOKUP}
type
  TDayTable = array[1..12] of Byte;

const
  MonthDays: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  LeapMonthDays: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

var I: Byte;
{$ENDIF}
begin
  {$IFNDEF LOOKUP}
  M:=1;
  {$ENDIF}

  if CalcLeap(T,result) then
  begin
    {$IFDEF LOOKUP}
    M:=TLeapLookup.Month[result];
    result:=TLeapLookup.Day[result];
    {$ELSE}
    while True do
    begin
      I := LeapMonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
    {$ENDIF}
  end
  else
  begin
    {$IFDEF LOOKUP}
    M:=TLookup.Month[result];
    result:=TLookup.Day[result];
    {$ELSE}
    while True do
    begin
      I := MonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
    {$ENDIF}
  end;

  {$IFNDEF LOOKUP}
  Inc(result);
  {$ENDIF}
end;

class function TFastDateTime.MonthOf(const Date: Integer): Byte;
begin
  if Date<=0 then
     result:=0
  else
     DayMonth(Date,result);
end;

class function TFastDateTime.MonthOf(const DateTime: TTimeStamp): Byte;
begin
  result:=MonthOf(DateTime.Date);
end;

class function TFastDateTime.DayOf(const Date: Integer): Word;
var M: Byte;
begin
  if Date<=0 then
     result:=0
  else
     result:=DayMonth(Date,M);
end;

class function TFastDateTime.DayOf(const DateTime: TTimeStamp): Word;
begin
  result:=DayOf(DateTime.Date);
end;

class function TFastDateTime.DayOfTheYear(const Date: Integer): Word;
var Y: Word;
begin
  if Date<=0 then
     result:=0
  else
     result:=CalcDayOfYear(Date,Y)+1;
end;

// It is faster to use Round than Trunc in 64 bits cpu,
// and the opposite, it is faster to use Trunc than Round in 32 bits cpu.
{$IFDEF CPUX64}
{$DEFINE USEROUNDSTAMP}
{$ENDIF}

{$IFDEF DATESTAMP}
class function TFastDateTime.DateTimeToDateStamp(const DateTime: TDateTime): Integer;
begin
  Result := DateDelta +
    {$IFDEF USEROUNDSTAMP}
    (Round(DateTime * MSecsPerDay) div MSecsPerDay);
    {$ELSE}
    Trunc(DateTime);
    {$ENDIF}
end;
{$ENDIF}

class function TFastDateTime.DayOfTheYear(const DateTime: TTimeStamp): Word;
begin
  result:=DayOfTheYear(DateTime.Date);
end;
{$ENDIF}

class function TFastDateTime.DayOf(const DateTime: TDateTime): Word;
begin
  {$IFDEF FASTDAYOF}
  {$IFDEF DATESTAMP}
  result:=DayOf(DateTimeToDateStamp(DateTime));
  {$ELSE}
  result:=DayOf(DateTimeToTimeStamp(DateTime).Date);
  {$ENDIF}
  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.DayOf(DateTime);
  {$ENDIF}
end;

class function TFastDateTime.DayOfTheYear(const DateTime: TDateTime): Word;
begin
  {$IFDEF FASTDAYOFYEAR}
  {$IFDEF DATESTAMP}
  result:=DayOfTheYear(DateTimeToDateStamp(DateTime));
  {$ELSE}
  result:=DayOfTheYear(DateTimeToTimeStamp(DateTime).Date);
  {$ENDIF}
  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.DayOfTheYear(DateTime);
  {$ENDIF}
end;

class function TFastDateTime.MonthOf(const DateTime: TDateTime): Byte;
begin
  {$IFDEF FASTMONTHOF}
  {$IFDEF DATESTAMP}
  result:=MonthOf(DateTimeToDateStamp(DateTime));
  {$ELSE}
  result:=MonthOf(DateTimeToTimeStamp(DateTime).Date);
  {$ENDIF}
  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.MonthOf(DateTime);
  {$ENDIF}
end;

class function TFastDateTime.YearOf(const DateTime: TDateTime): Word;
begin
  {$IFDEF FASTYEAROF}
  {$IFDEF DATESTAMP}
  result:=YearOf(DateTimeToDateStamp(DateTime));
  {$ELSE}
  result:=YearOf(DateTimeToTimeStamp(DateTime).Date);
  {$ENDIF}
  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.YearOf(DateTime);
  {$ENDIF}
end;

end.
