object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Spring4D Container Exercises'
  ClientHeight = 355
  ClientWidth = 728
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
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 722
    Height = 33
    Align = alTop
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 125
      Height = 25
      Align = alLeft
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 135
      Top = 4
      Width = 125
      Height = 25
      Align = alLeft
      Caption = 'Button2'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 266
      Top = 4
      Width = 125
      Height = 25
      Align = alLeft
      Caption = 'Button3'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
