unit DotEnv;

interface

uses
  System.Classes, System.SysUtils, System.Types, Winapi.Windows;

{x$DEFINE USE_APP_ENV}

type TDotEnv = class(TInterfacedObject, IInterface)
  private
    FEnvList: TStrings;
    procedure AddItem(const Strings: TStrings; const Name, Value: string); overload;
    procedure AddItem(const Strings: TStrings; const Item: string); overload;
  protected
    procedure GetSystemEnv(const Strings: TStrings);
    procedure GetLocalEnv(const Strings: TStrings; const Suffix: string);
    function GetEnv(const Name: string): string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure All(const Strings: TStrings);
    procedure Refresh;
    function AsInteger(const Name: string): Integer; overload;
    function AsInteger(const Name: string; DefaultValue: Integer): Integer; overload;
    function AsBoolean(const Name: string): Boolean; overload;
    function AsBoolean(const Name: string; DefaultValue: Boolean): Boolean; overload;
    function AsFloat(const Name: string): Extended; overload;
    function AsFloat(const Name: string; DefaultValue: Extended): Extended; overload;
    class function Env: TDotEnv;
    property Variable[const Name: string]: string read GetEnv; default;
end;

implementation

const
  ENV_FILE_NAME = '.env';
  SUFFIX_DEVELOPMENT = '.development';
  SUFFIX_PRODUCTION = '.production';

var
  _env: TDotEnv;

{ TDotEnv }

procedure TDotEnv.AddItem(const Strings: TStrings; const Name, Value: string);
var
  p: Integer;
  idx: Integer;
  _Name, _Value: string;
begin
  _Name  := Name.Trim;
  _Value := Value.Trim;

  if (_Value.StartsWith('''') or _Value.StartsWith('"')) then
    _Value := _Value.Substring(1);
  if _Value.EndsWith('''') or _Value.EndsWith('"') then
    _Value := _Value.Substring(0, _Value.Length - 1);

  idx := Strings.IndexOfName(_Name);
  if idx >= 0 then
    Strings.ValueFromIndex[idx] := _Value
  else
    Strings.AddPair(_Name, _Value);
{$IFDEF USE_APP_ENV}
  SetEnvironmentVariable(PChar(_Name), PChar(_Value));
{$ENDIF}
end;

procedure TDotEnv.AddItem(const Strings: TStrings; const Item: string);
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

function TDotEnv.GetEnv(const Name: string): string;
begin
  Result := FEnvList.Values[Name];
end;

class function TDotEnv.Env: TDotEnv;
begin
  if not Assigned(_env) then
    _env := TDotEnv.Create;
  Result := _env;
end;

procedure TDotEnv.All(const Strings: TStrings);
begin
  Strings.Assign(FEnvList);
end;

procedure TDotEnv.GetLocalEnv(const Strings: TStrings; const Suffix: string);
var
  fileName: string;
  sl: TStringList;
  i: Integer;
begin
  fileName :=  ExtractFilePath(ParamStr(0)) + ENV_FILE_NAME + Suffix;
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

procedure TDotEnv.GetSystemEnv(const Strings: TStrings);
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
  GetSystemEnv(FEnvList);
  GetLocalEnv(FEnvList, '');
{$IFDEF DEBUG}
  GetLocalEnv(FEnvList, SUFFIX_DEVELOPMENT);
{$ELSE}
  GetLocalEnv(FEnvList, SUFFIX_PRODUCTION);
{$ENDIF}
end;

function TDotEnv.AsInteger(const Name: string; DefaultValue: Integer): Integer;
begin
  Result := StrToIntDef(FEnvList.Values[Name], DefaultValue);
end;

function TDotEnv.AsInteger(const Name: string): Integer;
begin
  Result := StrToInt(FEnvList.Values[Name]);
end;

function TDotEnv.AsBoolean(const Name: string): Boolean;
begin
  Result := StrToBool(FEnvList.Values[Name]);
end;

function TDotEnv.AsBoolean(const Name: string; DefaultValue: Boolean): Boolean;
begin
  Result := StrToBoolDef(FEnvList.Values[Name], DefaultValue);
end;

function TDotEnv.AsFloat(const Name: string): Extended;
begin
  Result := StrToFloat(FEnvList.Values[Name]);
end;

function TDotEnv.AsFloat(const Name: string; DefaultValue: Extended): Extended;
begin
  Result := StrToFloatDef(FEnvList.Values[Name], DefaultValue);
end;

initialization
  _env := nil;

finalization
  if Assigned(_env) then
    FreeAndNil(_env);

end.
