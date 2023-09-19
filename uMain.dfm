object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 427
    Width = 622
    Height = 15
    Align = alBottom
    Caption = 'Label1'
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 628
    Height = 383
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 628
    Height = 41
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Click Me!'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
