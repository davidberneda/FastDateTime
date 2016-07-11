object FormTest: TFormTest
  Left = 0
  Top = 0
  Caption = 'FastDateTime Test'
  ClientHeight = 304
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LTest: TLabel
    Left = 112
    Top = 261
    Width = 3
    Height = 13
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 305
    Height = 177
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 256
    Width = 75
    Height = 25
    Caption = '&Verify'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 2
    OnClick = Button2Click
  end
end
