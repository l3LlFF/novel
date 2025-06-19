unit SoundManager;

interface

procedure PlayLoopedSound(const FileName: string);
procedure StopSound;

implementation

uses MMSystem, Windows;

procedure PlayLoopedSound(const FileName: string);
begin
  PlaySound(PChar(FileName), 0, SND_FILENAME or SND_ASYNC or SND_LOOP);
end;

procedure StopSound;
begin
  PlaySound(nil, 0, 0);
end;

end.

