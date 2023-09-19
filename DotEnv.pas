unit DotEnv;

interface

uses
  System.Classes, System.SysUtils, System.Types, Winapi.Windows;

type TDotEnv = class(TInterfacedObject, IInterface)
  private
    FEnvList: TStrings;
    procedure AddItem(Strings: TStrings; Name, Value: string); overload;
    procedure AddItem(Strings: TStrings; Item: string); overload;
  protected
    procedure GetSysEnv(Strings: TStrings);
    procedure GetLocEnv(Strings: TStrings; Suffix: string);
    function GetEnv(Name: string): string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure All(Strings: TStrings);
    property  Variable[Name: string]: string read GetEnv; default;
    procedure Refresh;
    class function Env: TDotEnv;
end;

implementation

var
  _env: TDotEnv;

{ TDotEnv }

procedure TDotEnv.AddItem(Strings: TStrings; Name, Value: string);
var
  p: Integer;
  idx: Integer;
begin
  Name  := Name.Trim;
  Value := Value.Trim;

  if (Value.StartsWith('''') or Value.StartsWith('"')) then
    Value := Value.Substring(1);
  if Value.EndsWith('''') or Value.EndsWith('"') then
    Value := Value.Substring(0, Value.Length - 1);

  idx := Strings.IndexOfName(Name);
  if idx >= 0 then
    Strings.ValueFromIndex[idx] := Value
  else
    Strings.AddPair(Name, Value);
end;

procedure TDotEnv.AddItem(Strings: TStrings; Item: string);
var
  p: Integer;
begin
  p := Item.IndexOf('=');
  if p >= 0 then
  begin
    AddItem(Strings, Item.Substring(0, p).Trim, Item.Substring(p + 1).Trim);
  end;
end;

constructor TDotEnv.Create;
begin
  FEnvList := TStringList.Create;
  Refresh;
end;

destructor TDotEnv.Destroy;
begin
  FEnvList.Free;
  inherited;
end;

function TDotEnv.GetEnv(Name: string): string;
begin
  Result := FEnvList.Values[Name];
end;

class function TDotEnv.Env: TDotEnv;
begin
  if not Assigned(_env) then
    _env := TDotEnv.Create;
  Result := _env;
end;

procedure TDotEnv.All(Strings: TStrings);
begin
  Strings.Assign(FEnvList);
end;

procedure TDotEnv.GetLocEnv(Strings: TStrings; Suffix: string);
var
  fileName: string;
  sl: TStringList;
  i: Integer;
begin
  fileName :=  ExtractFilePath(ParamStr(0)) + '.env' + Suffix;
  if FileExists(fileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(fileName);
      for i := 0 to sl.Count - 1 do
      begin
        var s := sl[i].Trim;
        if s.IsEmpty or s.StartsWith('//') or s.StartsWith('#') then
          Continue;
        AddItem(Strings, sl.Names[i], sl.ValueFromIndex[i]);
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TDotEnv.GetSysEnv(Strings: TStrings);
var
  Variable: Boolean;
  Str: PChar;
  Res: string;
begin
  Str := GetEnvironmentStrings;
  Res := '';
  Variable := False;

  while True do begin
    if Str^ = #0 then
    begin
      if Variable then
        AddItem(Strings, Res);
      Variable := True;
      Inc(Str);
      Res := '';
      if Str^ = #0 then
        Break
      else
        Res := Res + str^;
    end else
    if Variable then
      Res := Res + Str^;
    Inc(str);
  end;
end;

procedure TDotEnv.Refresh;
begin
  FEnvList.Clear;
  GetSysEnv(FEnvList);
  GetLocEnv(FEnvList, '');
{$IFDEF DEBUG}
  GetLocEnv(FEnvList, '.development');
{$ELSE}
  GetLocEnv(FEnvList, '.production');
{$ENDIF}
end;

initialization
  _env := nil;

finalization
  if Assigned(_env) then
    FreeAndNil(_env);

end.
