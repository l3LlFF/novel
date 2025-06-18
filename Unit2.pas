unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  System.JSON, System.IOUtils, Math, Vcl.Imaging.GIFImg;

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
    MadHealth: Integer;
    NoaHealth: Integer;
    is_delievered: Boolean;
    PetName: array[1..50] of Char;
  end;

  TForm2 = class(TForm)
    imgBackground: TImage;
    imgCharacter: TImage;
    pnlTextContainer: TPanel;
    lblText: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;




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
    GIF: TGIFImage;
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

  Edit1.Visible := False;

  // Íàñòðîéêà òàéìåðà àâòîñîõðàíåíèÿ
  FAutoSaveTimer := TTimer.Create(Self);
  FAutoSaveTimer.Interval := AutoSaveInterval;
  FAutoSaveTimer.OnTimer := AutoSaveTimerEvent;
  FAutoSaveTimer.Enabled := True;


  GIF := TGIFImage.Create;
  GIF.LoadFromFile('..\\..\\assets\\background\\background_2.gif');
  GIF.Animate := True; // Enable animation

  JSONStr := TFile.ReadAllText('..\..\dialogs.json', TEncoding.UTF8);
  JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  ScenesArr := JSONObj.GetValue<TJSONArray>('scenes');
  //SetScene;

  FGameState.is_delievered := False;

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
  background_filename, sprite_filename, sprite_position: string;
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

      if LowerCase(ExtractFileExt(background_filename)) = '.gif' then
        begin


          //imgBackground.Picture.Graphic := nil;
          imgBackground.Picture.Graphic := GIF;
          imgBackground.Parent.DoubleBuffered := True;
        end
      else
        begin
          imgBackground.Picture.LoadFromFile(background_filename);
          imgBackground.Stretch := True; // Optional: scales image to fit form
          imgBackground.Visible := True;
        end;




    end;

  // Loading sprite image
  sprite_filename := Item.GetValue<string>('sprite');
  sprite_position := Item.GetValue<string>('sprite_position');
  if FileExists(sprite_filename) then
    begin
      imgCharacter.Picture.LoadFromFile(sprite_filename);
      imgCharacter.Stretch := True; // Optional: scales image to fit form
      imgCharacter.Visible := True;

      if sprite_position = 'center' then
        imgCharacter.Left := (Form1.Width - imgCharacter.Width) div 2
      else if sprite_position = 'left' then
        imgCharacter.Left := 0;
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
  lblText.Font.Color := HexToColor(Item.GetValue<string>('font_color'));



  // Fight
  if FGameState.CurrentScene = 1190 then
    begin
      FGameState.MadHealth := 75;
      FGameState.NoaHealth := 40;
    end
  else if FGameState.CurrentScene = 118 then
    begin
      FGameState.MadHealth := 75;
      FGameState.NoaHealth := 50;
    end
  else if FGameState.CurrentScene = 122 then
    begin
      FGameState.NoaHealth := Min(FGameState.NoaHealth + 15, 50);
      lblText.Caption := lblText.Caption + IntToStr(FGameState.NoaHealth) + '/50';
    end
  else if FGameState.CurrentScene = 121 then
    begin
      FGameState.MadHealth := FGameState.MadHealth - Min(20, FGameState.MadHealth div 2);
      lblText.Caption := lblText.Caption + IntToStr(FGameState.MadHealth) + '/75';
    end
  else if FGameState.CurrentScene = 123 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - Min(10, FGameState.NoaHealth div 2);
      lblText.Caption := lblText.Caption + IntToStr(FGameState.NoaHealth) + '/50';
    end
  // Fight 2
  else if FGameState.CurrentScene = 328 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 20;
      lblText.Caption := lblText.Caption + IntToStr(FGameState.NoaHealth) + '/50';
    end
  else if FGameState.CurrentScene = 331 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 10;
      lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.';
    end
  else if FGameState.CurrentScene = 338 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 20;
      lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50.';
    end
  else if FGameState.CurrentScene = 340 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 10;
      lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.';
    end;


  // delievery
  if FGameState.CurrentScene = 14 then
    begin
      FGameState.is_delievered := True;
    end
  else if FGameState.CurrentScene = 15 then
    begin
      FGameState.is_delievered := False;
    end;

  // pet name
  if FGameState.CurrentScene = 230 then
    begin
      Edit1.Visible := True;
    end
  else
    begin
      Edit1.Visible := False;
    end;

  // Fight 2, set health
  if FGameState.CurrentScene = 314 then
    begin
      FGameState.MadHealth := 75;
      FGameState.NoaHealth := 50;
    end;

  if FGameState.CurrentScene = 318 then
    begin
      FGameState.MadHealth := 75;
      FGameState.NoaHealth := 30;
    end;

  if FGameState.CurrentScene = 321 then
    begin
      FGameState.MadHealth := 75;
      FGameState.NoaHealth := 40;
    end;


  // show pet name
  if FGameState.CurrentScene = 231 then
      lblText.Caption := 'Да! ' + string(PChar(@FGameState.PetName[1])) + ' неплохой вариант !';



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
  FGameState.CurrentScene := 311;
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
    // save pet name
    if FGameState.CurrentScene = 230 then
      StrPLCopy(@FGameState.PetName[1], Edit1.Text, SizeOf(FGameState.PetName));
    // fight 1
    if (FGameState.CurrentScene = 123) and (FGameState.MadHealth <= 5) then
      begin
        FGameState.CurrentScene := 124;
        SetScene;
      end
    else if (FGameState.CurrentScene = 123) and (FGameState.NoaHealth <= 10) then
      begin
        FGameState.CurrentScene := 127;
        FGameState.MadHealth := 75;
        FGameState.NoaHealth := 50;
        SetScene;
      end
    // Delievery
    else if (FGameState.CurrentScene = 158) and (FGameState.is_delievered = True) then
      begin
        FGameState.CurrentScene := 159;
        SetScene;
      end
    else if (FGameState.CurrentScene = 158) and (FGameState.is_delievered = False) then
      begin
        FGameState.CurrentScene := 174;
        SetScene;
      end
    else if (FGameState.CurrentScene = 338) and (FGameState.NoaHealth <= 0) then
      begin
        FGameState.CurrentScene := 342;
        SetScene;
      end
    else if (FGameState.CurrentScene = 340) and (FGameState.NoaHealth <= 0) then
      begin
        FGameState.CurrentScene := 342;
        SetScene;
      end
    else if FGameState.NextScene <> FGameState.CurrentScene then
      begin
        FGameState.CurrentScene := FGameState.NextScene;
        SetScene;
      end

//    SaveGame;
  except
    on E: Exception do
    begin
      // Ïîäàâëÿåì îøèáêó, íî ïðîäîëæàåì ðàáîòó
    end;
  end;
end;


end.
