unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  System.JSON, System.IOUtils;

type
  TGameState = record
    CurrentScene: Integer;
    CurrentText: Integer;
    BackgroundVisible: Boolean;
    CharacterVisible: Boolean;
    CurrentCharacterImage: Integer;
    GameStarted: Boolean;
    NextScene: Integer;
    NextScene1: Integer;
    NextScene2: Integer;
  end;

  TForm2 = class(TForm)
    imgBackground: TImage;
    imgCharacter: TImage;
    pnlTextContainer: TPanel;
    lblText: TLabel;
    Button1: TButton;
    Button2: TButton;




    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FGameState: TGameState;
    FAutoSaveTimer: TTimer;
    ScenesArr: TJSONArray;
    DialogueArr: TJSONArray;
    procedure InitializeGame;
    procedure SetScene;
    procedure ShowSceneElements(ShowBackground, ShowCharacter: Boolean);
    procedure SwitchCharacterImage(Index: Integer);
    procedure NextText;
    procedure SaveGame;
    procedure LoadGameState;
    procedure UpdateGameVisuals;
    procedure AutoSaveTimerEvent(Sender: TObject);
    procedure EnsureVisible;
    procedure SafeStartNewGame;
    procedure CheckGameStarted;

  public
    procedure StartNewGame;
    procedure ContinueGame;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  Unit1;

const
  SaveFileName = 'game_save.dat';
  AutoSaveInterval = 30000;

procedure TForm2.FormCreate(Sender: TObject);
var
  JSONStr: string;
  JSONObj, Item: TJSONObject;
  //DialogueArr, ChoicesArr: TJSONArray;
begin
  // Íàñòðîéêà ýëåìåíòîâ èíòåðôåéñà
  lblText.WordWrap := True;
  lblText.AutoSize := False;
  lblText.Align := alClient;
  lblText.Alignment := taCenter;
  lblText.Layout := tlCenter;

  pnlTextContainer.BevelOuter := bvNone;
  pnlTextContainer.DoubleBuffered := True;

  // Èçíà÷àëüíî ñêðûâàåì âñå ýëåìåíòû
  imgBackground.Visible := False;
  imgCharacter.Visible := False;

  // Íàñòðîéêà òàéìåðà àâòîñîõðàíåíèÿ
  FAutoSaveTimer := TTimer.Create(Self);
  FAutoSaveTimer.Interval := AutoSaveInterval;
  FAutoSaveTimer.OnTimer := AutoSaveTimerEvent;
  FAutoSaveTimer.Enabled := True;



  JSONStr := TFile.ReadAllText('..\..\dialogs.json', TEncoding.UTF8);
  JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  ScenesArr := JSONObj.GetValue<TJSONArray>('scenes');
  //SetScene;
end;

function HexToColor(const Hex: string): TColor;
var
  R, G, B: Byte;
begin
  R := StrToInt('$' + Copy(Hex, 2, 2));
  G := StrToInt('$' + Copy(Hex, 4, 2));
  B := StrToInt('$' + Copy(Hex, 6, 2));
  Result := RGB(R, G, B);
end;


procedure TForm2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: NextText;      // Enter - ñëåäóþùèé òåêñò
    #27: Close;         // Escape - çàêðûòü èãðó
    's', 'S': SaveGame; // Ðó÷íîå ñîõðàíåíèå
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FAutoSaveTimer) then
  begin
    FAutoSaveTimer.Enabled := False;
    FAutoSaveTimer.Free;
  end;

  Action := caFree;
  Form2 := nil;
  if Assigned(Form1) then
    Form1.Show;
end;

procedure TForm2.InitializeGame;
begin
  try
    if FileExists(SaveFileName) then
    begin
      var F: File of TGameState;
      AssignFile(F, SaveFileName);
      try
        Reset(F);
        Read(F, FGameState);
      finally
        CloseFile(F);
      end;
      LoadGameState;
    end
    else
    begin
      SafeStartNewGame;
    end;
  except
    on E: Exception do
    begin
      SafeStartNewGame;
    end;
  end;
end;




procedure TForm2.SetScene;
var
  Item: TJSONObject;
  background_filename, sprite_filename: string;
  choices: TJSONArray;
begin
  //if (FGameState.CurrentScene >= 0) and (FGameState.CurrentScene < ScenesArr.Count) then
  //begin
  //  Item := ScenesArr.Items[FGameState.CurrentScene] as TJSONObject;
  //end;
  //else
  //begin
  //  ShowMessage('Scene index is out of range!');
  //end;

  for var i := 0 to ScenesArr.Count - 1 do
    begin
      item := ScenesArr.Items[i] as TJSONObject;
      if item.GetValue<integer>('id') = FGameState.CurrentScene then
      begin
        Item := item;
        Break;
      end;
    end;


  // Loading background image
  background_filename := Item.GetValue<string>('background');
  if FileExists(background_filename) then
    begin
      imgBackground.Picture.LoadFromFile(background_filename);
      imgBackground.Stretch := True; // Optional: scales image to fit form
      imgBackground.Visible := True;
    end;

  // Loading sprite image
  sprite_filename := Item.GetValue<string>('sprite');
  if FileExists(sprite_filename) then
    begin
      imgCharacter.Picture.LoadFromFile(sprite_filename);
      imgCharacter.Stretch := True; // Optional: scales image to fit form
      imgCharacter.Visible := True;
    end
  else if sprite_filename = '' then
    begin
      imgCharacter.Visible := False;
    end;

  // Add buttons if needed
  DialogueArr := Item.GetValue<TJSONArray>('choices');
  if DialogueArr.Count = 2 then
    begin
      Button1.Caption := DialogueArr.Items[0].GetValue<string>('text');
      Button2.Caption := DialogueArr.Items[1].GetValue<string>('text');

      Button1.Visible := True;
      Button2.Visible := True;

      FGameState.NextScene1 := DialogueArr.Items[0].GetValue<integer>('next');
      FGameState.NextScene2 := DialogueArr.Items[1].GetValue<integer>('next');

      FGameState.NextScene := FGameState.CurrentScene;
    end
  else if DialogueArr.Count = 1 then
    begin
      Button1.Visible := False;
      Button2.Visible := False;
      FGameState.NextScene := DialogueArr.Items[0].GetValue<integer>('next');
    end;

  // Setting dialogue text
  lblText.Caption := Item.GetValue<string>('text');
  lblText.Font.Color := HexToColor(Item.GetValue<string>('font_color'));;
  //lblText.Caption := 'привет';

  EnsureVisible;

end;


procedure TForm2.ShowSceneElements(ShowBackground, ShowCharacter: Boolean);
begin
  imgBackground.Visible := ShowBackground;

  if ShowCharacter then
    SwitchCharacterImage(FGameState.CurrentCharacterImage)
  else
  begin
    imgCharacter.Visible := False;
  end;
end;

procedure TForm2.SwitchCharacterImage(Index: Integer);
begin
  imgCharacter.Visible := True;

  // add image read

  FGameState.CurrentCharacterImage := Index;
end;

procedure TForm2.SaveGame;
var
  F: File of TGameState;
begin
  try
    AssignFile(F, SaveFileName);
    try
      Rewrite(F);
      Write(F, FGameState);
    finally
      CloseFile(F);
    end;
  except
    on E: Exception do
      ShowMessage('Îøèáêà ñîõðàíåíèÿ èãðû: ' + E.Message);
  end;
end;

procedure TForm2.LoadGameState;
begin

end;

procedure TForm2.UpdateGameVisuals;
begin
  ShowSceneElements(FGameState.BackgroundVisible, FGameState.CharacterVisible);
end;

procedure TForm2.AutoSaveTimerEvent(Sender: TObject);
begin
  SaveGame;
end;

procedure TForm2.EnsureVisible;
begin
  if not Visible then Show;
  if WindowState = wsMinimized then WindowState := wsNormal;
  BringToFront;
  SetFocus;
  Update;
  Refresh;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  //ShowMessage(IntToStr(FGameState.NextScene1));
  FGameState.CurrentScene := FGameState.NextScene1;
  SetScene;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  //ShowMessage(IntToStr(FGameState.NextScene2));
  FGameState.CurrentScene := FGameState.NextScene2;
  SetScene;
end;

procedure TForm2.CheckGameStarted;
begin
  if not FGameState.GameStarted then
  begin
    ShowMessage('Âû íå íà÷èíàëè íîâóþ èãðó');
    Abort;
  end;
end;

procedure TForm2.SafeStartNewGame;
begin
  FGameState.CurrentScene := 0;
  SetScene;
end;

procedure TForm2.StartNewGame;
begin
  if FileExists(SaveFileName) then
    DeleteFile(SaveFileName);

  SafeStartNewGame;
end;

procedure TForm2.ContinueGame;
begin
  
end;

procedure TForm2.NextText;
begin
  try
    //CheckGameStarted;
    // FGameState.CurrentScene := FGameState.CurrentScene + 1;
    if FGameState.NextScene <> FGameState.CurrentScene then
      FGameState.CurrentScene := FGameState.NextScene;
      SetScene;
//    SaveGame;
  except
    on E: Exception do
    begin
      // Ïîäàâëÿåì îøèáêó, íî ïðîäîëæàåì ðàáîòó
    end;
  end;
end;


end.
