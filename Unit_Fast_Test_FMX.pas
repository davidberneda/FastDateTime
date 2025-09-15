unit Unit_Fast_Test_FMX;

{
  https://github.com/davidberneda/FastDateTime
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TFormTest = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Text1: TText;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    procedure TestDay(const D:TDateTime);
    procedure TestDayOfYear(const D:TDateTime);
    procedure TestMonth(const D:TDateTime);
    procedure TestYear(const D:TDateTime);
  public
    { Public declarations }
  end;

var
  FormTest: TFormTest;

implementation

{$R *.fmx}

uses
  System.Diagnostics, Tee.FastDateTime, DateUtils;

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

  Memo1.Lines.Add('YearOf: '+t2.ToString+' msec '+y.ToString);

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      y:=TFastDateTime.YearOf(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast YearOf: '+t3.ToString+' msec '+y.ToString+' '+Diff(t2,t3));
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

  Memo1.Lines.Add('MonthOf: '+t2.ToString+' msec '+day.ToString);

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.MonthOf(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast MonthOf: '+t3.ToString+' msec '+day.ToString+' '+Diff(t2,t3));
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

  Memo1.Lines.Add('DayOf: '+t2.ToString+' msec '+day.ToString);

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.DayOf(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast DayOf: '+t3.ToString+' msec '+day.ToString+' '+Diff(t2,t3));
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

  Memo1.Lines.Add('DayOfYear: '+t2.ToString+' msec '+day.ToString);

  t1:=TStopwatch.StartNew;

  for t:=1 to TestTimes do
      day:=TFastDateTime.DayOfTheYear(D);

  t3:=t1.ElapsedMilliseconds;

  Memo1.Lines.Add('Fast DayOfYear: '+t3.ToString+' msec '+day.ToString+' '+Diff(t2,t3));
end;

procedure TFormTest.Button1Click(Sender: TObject);
var D : TDateTime;
begin
  Memo1.Text:='';

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
  Text1.Text:='';

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

  Text1.Text:='Ok';
end;

procedure TFormTest.FormCreate(Sender: TObject);
begin
  Button1Click(Self);
end;

end.
