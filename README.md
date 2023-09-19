Simple DotEnv implementation for Delphi
===

This library read all Windows environment variables and add variables from the .env files.
If particulat variable already exist, it is replaced.
*Unfortunately only Windows is supported for now.*

It support `.env`, `.env.production` and `.env.development` files.

Files must be copied to the folder, where is exe file.
I've no clue how to implement this functionality in the compilation time so that env files could be placed in the source tree, i'm not sure it's even possible in Delphi.

Library doesn't support nested variables, for example `VARIABLE2=$(VARIABLE1) value` - this is *NOT* supported.
Just simple variables `NAME=Value` are supported (at least for now).

Values can be surrouded by quotation marks or apostrophes but it isn't necessary.

How to use it?
---

Simple copy `DotEnv.pas` file to your project, add it to the uses list and use it as in the example:

```
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
```

Example application is of course attached. Example env files are also attached, just copy it to the exe folder and rename it from `env*` to `.env*`.
