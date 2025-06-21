unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  System.JSON, System.IOUtils, Math, Vcl.Imaging.GIFImg, MMSystem,
  SoundManager;

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
    Edit1: TEdit;
    TextTimer: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Image1: TImage;
    LabelImage1: TLabel;
    Image2: TImage;
    LabelImage2: TLabel;
    Image3: TImage;
    Image4: TImage;
    LabelImage3: TLabel;
    LabelImage4: TLabel;




    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TextTimerTimer(Sender: TObject);
    procedure StartTextAnimation(const Text: string);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
  private
    FGameState: TGameState;
    FAutoSaveTimer: TTimer;
    ScenesArr: TJSONArray;
    DialogueArr: TJSONArray;
    GIF: TGIFImage;
    previous_background: string;
    previous_audio: string;
    FullText: string;
    Words: TArray<string>;
    CurrentWordIndex: Integer;

    procedure SetScene;
    procedure NextText;
    procedure Menu;
    procedure SaveGame;
    procedure LoadGame;
    procedure EnsureVisible;
    procedure CheckGameStarted;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

const
  SaveFileName = 'game_save.dat';
  AutoSaveInterval = 30000;


procedure LoadCustomFont(const FontPath: string);
begin
  AddFontResourceEx(PChar(FontPath), FR_PRIVATE, nil);
  //SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
end;

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


  Label1.WordWrap := True;
  Label1.AutoSize := False;
  Label1.Align := alClient;
  Label1.Alignment := taCenter;
  Label1.Layout := tlCenter;

  LabelImage1.Enabled := False;
  LabelImage1.Font.Color := clWhite;

  LabelImage2.Enabled := False;
  LabelImage2.Font.Color := clWhite;

  LabelImage3.Enabled := False;
  LabelImage4.Enabled := False;

  LabelImage3.Font.Color := clWhite;
  LabelImage4.Font.Color := clWhite;


  LoadCustomFont('..\\..\\assets\\fonts\\Minecraftia-Regular.ttf');
  lblText.Font.Name := 'Minecraftia';
  lblText.Font.Size := 10;

  Label1.Font.Name := 'Minecraftia';
  Label1.Font.Size := 10;

  LabelImage1.Font.Name := 'Minecraftia';
  LabelImage1.Font.Size := 10;

  LabelImage2.Font.Name := 'Minecraftia';
  LabelImage2.Font.Size := 10;

  LabelImage3.Font.Name := 'Minecraftia';
  LabelImage3.Font.Size := 10;

  LabelImage4.Font.Name := 'Minecraftia';
  LabelImage4.Font.Size := 10;


  //pnlTextContainer.BevelOuter := bvNone;
  pnlTextContainer.DoubleBuffered := True;
  pnlTextContainer.Visible := False;

  //Panel1.BevelOuter := bvNone;
  Panel1.DoubleBuffered := True;
  Panel1.Visible := False;


  // Èçíà÷àëüíî ñêðûâàåì âñå ýëåìåíòû
  imgBackground.Visible := True;
  imgCharacter.Visible := False;

  Edit1.Visible := False;

  LabelImage1.Visible := False;
  LabelImage2.Visible := False;
  Image1.Visible := False;
  Image2.Visible := False;

  LabelImage3.Visible := True;
  LabelImage4.Visible := True;
  Image3.Visible := True;
  Image4.Visible := True;

  LabelImage1.Font.Name := 'Minecraftia';
  LabelImage2.Font.Name := 'Minecraftia';
  LabelImage3.Font.Name := 'Minecraftia';
  LabelImage4.Font.Name := 'Minecraftia';
  Edit1.Font.Name := 'Minecraftia';

  // Íàñòðîéêà òàéìåðà àâòîñîõðàíåíèÿ
  FAutoSaveTimer := TTimer.Create(Self);
  FAutoSaveTimer.Interval := AutoSaveInterval;
  FAutoSaveTimer.Enabled := True;


  GIF := TGIFImage.Create;
  GIF.LoadFromFile('..\\..\\assets\\background\\background_2.gif');
  GIF.Animate := True; // Enable animation

  JSONStr := TFile.ReadAllText('..\..\dialogs.json', TEncoding.UTF8);
  JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  ScenesArr := JSONObj.GetValue<TJSONArray>('scenes');

  PlaySound('..\\..\\assets\\audio\\menu.wav', 0, SND_FILENAME or SND_ASYNC or SND_LOOP);

  Form2.KeyPreview := True;

  FGameState.is_delievered := False;
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


procedure TForm2.TextTimerTimer(Sender: TObject);
begin
  if CurrentWordIndex < Length(FullText) + 1 then
  begin
    lblText.Caption := lblText.Caption + FullText[CurrentWordIndex];
    Inc(CurrentWordIndex);
  end
  else
    TextTimer.Enabled := False;
end;


procedure TForm2.StartTextAnimation(const Text: string);
begin
  FullText := Text;
  //Words := FullText.Split([' ']);
  CurrentWordIndex := 1;
  lblText.Caption := '';
  TextTimer.Enabled := True;
end;




procedure TForm2.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: NextText;      // Enter - ñëåäóþùèé òåêñò
    #27: Menu;         // Escape - çàêðûòü èãðó
    's', 'S': SaveGame; // Ðó÷íîå ñîõðàíåíèå
  end;
end;

procedure TForm2.Image1Click(Sender: TObject);
begin
  FGameState.CurrentScene := FGameState.NextScene1;
  SetScene;
end;

procedure TForm2.Image2Click(Sender: TObject);
begin
  FGameState.CurrentScene := FGameState.NextScene2;
  SetScene;
end;

procedure TForm2.Image3Click(Sender: TObject);
begin
  FGameState.CurrentScene := 0;
  LabelImage3.Visible := False;
  LabelImage4.Visible := False;
  Image3.Visible := False;
  Image4.Visible := False;
  LabelImage1.Visible := True;
  LabelImage2.Visible := True;
  Image1.Visible := True;
  Image2.Visible := True;
  SetScene;
end;

procedure TForm2.Image4Click(Sender: TObject);
begin
  LoadGame;
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
end;



procedure TForm2.SetScene;
var
  Item: TJSONObject;
  background_filename, sprite_filename, sprite_position, audio_filename, character_name: string;
  choices: TJSONArray;
  audio_looped: boolean;

begin
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
  if FileExists(background_filename) and (previous_background <> background_filename) then
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
      previous_background := background_filename;
    end;


  audio_filename := Item.GetValue<string>('audio');
  audio_looped := Item.GetValue<boolean>('audio_looped');


  if (audio_filename <> previous_audio) and (audio_filename <> '') then
    if audio_looped then
      //PlaySound(PWideChar(audio_filename), 0, SND_FILENAME or SND_ASYNC or SND_LOOP)
      PlayLoopedSound(audio_filename)
    else
      PlaySound(PWideChar(audio_filename), 0, SND_FILENAME or SND_ASYNC)
  else if audio_filename = '' then
    PlaySound(nil, 0, 0);

  previous_audio := audio_filename;

  // Load a character name
  character_name := Item.GetValue<string>('character_name');
  if character_name <> '' then
    begin
      Label1.Visible := true;
      Panel1.Visible := true;
      Label1.Caption := character_name;
    end
  else
    begin
      Label1.Visible := false;
      Panel1.Visible := false;
      Label1.Caption := '';
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
        imgCharacter.Left := (Form2.Width - imgCharacter.Width) div 2
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
      //Button1.Caption := DialogueArr.Items[0].GetValue<string>('text');
      //Button2.Caption := DialogueArr.Items[1].GetValue<string>('text');
      LabelImage1.Caption := DialogueArr.Items[0].GetValue<string>('text');
      LabelImage2.Caption := DialogueArr.Items[1].GetValue<string>('text');

      //Button1.Visible := True;
      //Button2.Visible := True;

      LabelImage1.Visible := True;
      LabelImage2.Visible := True;
      Image1.Visible := True;
      Image2.Visible := True;

      FGameState.NextScene1 := DialogueArr.Items[0].GetValue<integer>('next');
      FGameState.NextScene2 := DialogueArr.Items[1].GetValue<integer>('next');

      FGameState.NextScene := FGameState.CurrentScene;
    end
  else if DialogueArr.Count = 1 then
    begin
      //Button1.Visible := False;
      //Button2.Visible := False;

      Image1.Visible := False;
      Image2.Visible := False;
      LabelImage1.Visible := False;
      LabelImage2.Visible := False;

      FGameState.NextScene := DialogueArr.Items[0].GetValue<integer>('next');
    end;

  // Setting dialogue text
  // pnlTextContainer.Visible := True;
  //lblText.Caption := Item.GetValue<string>('text');
  StartTextAnimation(Item.GetValue<string>('text'));

  lblText.Font.Color := HexToColor(Item.GetValue<string>('font_color'));
  Label1.Font.Color := HexToColor(Item.GetValue<string>('font_color'));
  LabelImage1.Font.Color := HexToColor(Item.GetValue<string>('font_color'));
  LabelImage2.Font.Color := HexToColor(Item.GetValue<string>('font_color'));

  // Fight
  if (FGameState.CurrentScene = 101) or (FGameState.CurrentScene = 103) or (FGameState.CurrentScene = 114) or (FGameState.CurrentScene = 119) or (FGameState.CurrentScene = 312) or (FGameState.CurrentScene = 314) or (FGameState.CurrentScene = 324) or (FGameState.CurrentScene = 335) then
    begin

      Image1.Parent := pnlTextContainer;
      Image2.Parent := pnlTextContainer;

      LabelImage1.Parent := pnlTextContainer;
      LabelImage2.Parent := pnlTextContainer;


      Image1.Left := 40;
      Image1.Top := 50;
      Image2.Left := 368;
      Image2.Top := 50;

      LabelImage1.Left := 40;
      LabelImage1.Top := 60;
      LabelImage2.Left := 368;
      LabelImage2.Top := 60;


    end
  else
    begin

      Image1.Parent := Form2;
      Image2.Parent := Form2;

      LabelImage1.Parent := Form2;
      LabelImage2.Parent := Form2;

      Image1.Left := 438;
      Image1.Top := 135;
      Image2.Left := 438;
      Image2.Top := 200;

      LabelImage1.Left := 438;
      LabelImage1.Top := 145;
      LabelImage2.Left := 438;
      LabelImage2.Top := 210;
    end;

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
      //lblText.Caption := lblText.Caption + IntToStr(FGameState.NoaHealth) + '/50';
      StartTextAnimation('Мэдисон швыряет шарик с водой. Он пролетает мимо. Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50');
    end
  else if FGameState.CurrentScene = 121 then
    begin
      FGameState.MadHealth := FGameState.MadHealth - Min(20, FGameState.MadHealth div 2);
      //lblText.Caption := lblText.Caption + IntToStr(FGameState.MadHealth) + '/75';
      StartTextAnimation('Вы толкаете Мэдисона. Он невольно покачивается.. Оставшихся очков Мэдисона ' + IntToStr(FGameState.MadHealth) + '/75');
    end
  else if FGameState.CurrentScene = 123 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - Min(10, FGameState.NoaHealth div 2);
      //lblText.Caption := 'Мэдисон швыряет шарик с водой. Меткое попадание. Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50';
      StartTextAnimation('Мэдисон швыряет шарик с водой. Меткое попадание. Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50');
    end
  // Fight 2
  else if FGameState.CurrentScene = 328 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 20;
      //lblText.Caption := lblText.Caption + IntToStr(FGameState.NoaHealth) + '/50';
      StartTextAnimation('Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50');
    end
  else if FGameState.CurrentScene = 331 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 10;
      //lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.';
      StartTextAnimation('Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.');
    end
  else if FGameState.CurrentScene = 338 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 20;
      //lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50.';
      StartTextAnimation('Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50.');
    end
  else if FGameState.CurrentScene = 340 then
    begin
      FGameState.NoaHealth := FGameState.NoaHealth - 10;
      //lblText.Caption := 'Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.';
      StartTextAnimation('Оставшихся очков ' + IntToStr(FGameState.NoaHealth) + '/50. Могло быть и хуже.');
    end;

  if FGameState.CurrentScene = -1 then
    begin


      Image3.Visible := true;
      Image4.Visible := true;
      LabelImage3.Visible := true;
      LabelImage4.Visible := true;
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
      //lblText.Caption := 'Да! ' + string(PChar(@FGameState.PetName[1])) + ' неплохой вариант !';
      StartTextAnimation('Да! ' + string(PChar(@FGameState.PetName[1])) + ' неплохой вариант !');

  if FGameState.CurrentScene = -1 then
    begin
      pnlTextContainer.Visible := False;

    end
  else
    begin
      pnlTextContainer.Visible := True;


      Image3.Visible := False;
      Image4.Visible := False;
      LabelImage3.Visible := False;
      LabelImage4.Visible := False;
    end;

  EnsureVisible;

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

procedure TForm2.LoadGame;
begin
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
    end;
    SetScene;
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



// Start new game button
procedure TForm2.Menu;
begin
  FGameState.CurrentScene := -1;

  LabelImage1.Visible := False;
  LabelImage2.Visible := False;
  Image1.Visible := False;
  Image2.Visible := False;
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
      end;

    SaveGame;
  except
    on E: Exception do
    begin
      // Ïîäàâëÿåì îøèáêó, íî ïðîäîëæàåì ðàáîòó
    end;
  end;
end;


end.
