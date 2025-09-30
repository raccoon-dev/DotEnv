unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, DotEnv;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Read all environment variables
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
    TDotEnv.Env.All(Memo1.Lines);
  finally
    Memo1.Lines.EndUpdate;
  end;

  // Read only one variable
  Label1.Caption := TDotEnv.Env['VARIABLE1'];
  Label2.Caption := IntToStr(TDotEnv.Env.AsInteger('VARIABLE3', 123));
  Label3.Caption := BoolToStr(TDotEnv.Env.AsBoolean('VARIABLE4', False), True);
  Label4.Caption := FloatToStr(TDotEnv.Env.AsFloat('VARIABLE5', 123.456));
end;

end.
