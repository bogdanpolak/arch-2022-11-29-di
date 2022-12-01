object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 418
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 47
    Width = 764
    Height = 10
    Align = alTop
    Shape = bsSpacer
    ExplicitWidth = 554
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 758
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object btnMock: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 133
      Height = 33
      Align = alLeft
      Caption = 'btnMock'
      TabOrder = 0
      OnClick = btnMockClick
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 60
    Width = 654
    Height = 355
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
