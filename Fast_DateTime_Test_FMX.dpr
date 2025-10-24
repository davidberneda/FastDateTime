program Fast_DateTime_Test_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Fast_Test_FMX in 'Unit_Fast_Test_FMX.pas' {FormTest},
  Tee.FastDateTime in 'Tee.FastDateTime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
