unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg, Unit2;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure AppExceptionHandler(Sender: TObject; E: Exception);
    procedure InitializeGameForm;
  public
    procedure RestoreMainForm;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  SaveFile = 'novella_save.dat';

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FileExists(SaveFile) then
    DeleteFile(SaveFile);

  Hide;
  InitializeGameForm;

  try
    Form2.StartNewGame;
    Form2.Show;
  except
    on E: Exception do
    begin
      RestoreMainForm;
      ShowMessage('Error starting new game: ' + E.Message);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   Hide;
  InitializeGameForm;

  try
    Form2.ContinueGame;
    // Если игра завершена, Form2 закроется автоматически в ContinueGame
    // и нам нужно показать Form1 снова
    if not Form2.Visible then
      RestoreMainForm;
  except
    on E: Exception do
    begin
      RestoreMainForm;
      ShowMessage('Error loading game: ' + E.Message);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Caption := 'Начать игру';
  Button2.Caption := 'Продолжить';
  Application.OnException := AppExceptionHandler;
  Button2.Enabled := FileExists(SaveFile);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Form2) then
  begin
    Form2.Close;
    Form2 := nil;
  end;
  Action := caFree;
end;

procedure TForm1.AppExceptionHandler(Sender: TObject; E: Exception);
begin
  ShowMessage('Error: ' + E.Message);
end;

procedure TForm1.RestoreMainForm;
begin
  if Assigned(Form2) then
    Form2.Hide;
  Show;
  BringToFront;
  Button2.Enabled := FileExists(SaveFile);
end;

procedure TForm1.InitializeGameForm;
begin
  if not Assigned(Form2) then
  begin
    Form2 := TForm2.Create(Application);
    Form2.Position := poScreenCenter;
  end;
end;

end.
