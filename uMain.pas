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
end;

end.
