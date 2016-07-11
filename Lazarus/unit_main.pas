unit unit_main;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Tee.FastDateTime;

type

  { TFormTest }

  TFormTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    LTest: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }

    procedure TestDay(const D:TDateTime);
    procedure TestDayOfYear(const D:TDateTime);
    procedure TestMonth(const D:TDateTime);
    procedure TestYear(const D:TDateTime);
  public
    { public declarations }
  end;

var
  FormTest: TFormTest;

implementation

{$R *.lfm}

uses
  DateUtils;

type

  { TStopWatch }

  TStopWatch=record
  private
    Old : Int64;
  public
    function ElapsedMilliseconds:Int64;
    class function StartNew:TStopWatch; static;
  end;

{ TStopWatch }

function TStopWatch.ElapsedMilliseconds: Int64;
begin
  result:=GetTickCount64-Old;
end;

class function TStopWatch.StartNew: TStopWatch;
begin
  result.Old:=GetTickCount64;
end;

{ TFormTest }

function Diff(const ASlow,AFast:Int64):String;
var tmp : Single;
begin
  tmp:=100-(AFast*100/ASlow);

  result:='('+FormatFloat('0.##%',tmp)+' faster)';
end;

const
  TestTimes=5000000;

procedure TFormTest.TestYear(const D:TDateTime);
var t1 : TStopwatch;
    t2,t3 : Int64;
    t : Integer;
    y : Word;
begin
  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      y:=YearOf(D);

  t2:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('YearOf: '+IntToStr(t2)+' msec '+IntToStr(y));

  t1:=TStopwatch.StartNew;

  {$O-}
  for t:=1 to TestTimes do
      y:=TFastDateTime.YearOf(D);
  {$O+}

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast YearOf: '+IntToStr(t3)+' msec '+IntToStr(y)+' '+Diff(t2,t3));
end;

procedure TFormTest.TestMonth(const D:TDateTime);
var t1 : TStopwatch;
    t2, t3 : Int64;
    t : Integer;
    day : Word;
begin
  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=MonthOf(D);

  t2:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('MonthOf: '+IntToStr(t2)+' msec '+IntToStr(day));

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.MonthOf(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast MonthOf: '+IntToStr(t3)+' msec '+IntToStr(day)+' '+Diff(t2,t3));
end;

procedure TFormTest.TestDay(const D:TDateTime);
var t1 : TStopwatch;
    t2,t3 : Int64;
    t : Integer;
    day : Word;
begin
  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=DayOf(D);

  t2:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('DayOf: '+IntToStr(t2)+' msec '+IntToStr(day));

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.DayOf(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast DayOf: '+IntToStr(t3)+' msec '+IntToStr(day)+' '+Diff(t2,t3));
end;

procedure TFormTest.TestDayOfYear(const D:TDateTime);
var t1 : TStopwatch;
    t2,t3 : Int64;
    t : Integer;
    day : Word;
begin
  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=DayOfTheYear(D);

  t2:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('DayOfYear: '+IntToStr(t2)+' msec '+IntToStr(day));

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.DayOfTheYear(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast DayOfYear: '+IntToStr(t3)+' msec '+IntToStr(day)+' '+Diff(t2,t3));
end;

procedure TFormTest.Button1Click(Sender: TObject);
var D : TDateTime;
begin
  Memo1.Clear;

  D:=Now;
  //D:=EncodeDate(1994,7,1);

  TestYear(D);

  Memo1.Lines.Add('');

  TestMonth(D);

  Memo1.Lines.Add('');

  TestDay(D);

  Memo1.Lines.Add('');

  TestDayOfYear(D);
end;

procedure TFormTest.Button2Click(Sender: TObject);
var t : Integer;
    y,m,d : Word;
begin
  LTest.Caption:='';

  for t:=1 to Round(EncodeDate(2100,12,31)) do
  begin
    DecodeDate(t,y,m,d);

    if y<>TFastDateTime.YearOf(t) then
       raise Exception.Create('Wrong year: '+DateTimeToStr(t))
    else
    if m<>TFastDateTime.MonthOf(t) then
       raise Exception.Create('Wrong month: '+DateTimeToStr(t))
    else
    if d<>TFastDateTime.DayOf(t) then
       raise Exception.Create('Wrong day: '+DateTimeToStr(t))
    else
    if DayOfTheYear(t)<>TFastDateTime.DayOfTheYear(t) then
       raise Exception.Create('Wrong day of year: '+DateTimeToStr(t))
  end;

  LTest.Caption:='Ok';
end;

procedure TFormTest.FormCreate(Sender: TObject);
begin
  Button1Click(Self);
end;

end.

