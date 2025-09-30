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
    Top = 361
    Width = 622
    Height = 15
    Align = alBottom
    Caption = 'Label1'
    WordWrap = True
    ExplicitLeft = 8
    ExplicitTop = 355
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 382
    Width = 622
    Height = 15
    Align = alBottom
    Caption = 'Label2'
    ExplicitLeft = 8
    ExplicitTop = 391
    ExplicitWidth = 628
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 403
    Width = 622
    Height = 15
    Align = alBottom
    Caption = 'Label3'
    ExplicitLeft = 0
    ExplicitTop = 412
    ExplicitWidth = 628
  end
  object Label4: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 424
    Width = 622
    Height = 15
    Align = alBottom
    Caption = 'Label4'
    ExplicitLeft = 40
    ExplicitTop = 344
    ExplicitWidth = 34
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 628
    Height = 317
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 200
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
