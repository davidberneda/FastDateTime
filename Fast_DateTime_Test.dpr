program Fast_DateTime_Test;

uses
  Vcl.Forms,
  Unit_Fast_Test in 'Unit_Fast_Test.pas' {FormTest},
  Tee.FastDateTime in 'Tee.FastDateTime.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
