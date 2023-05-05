unit AiInfoJson;

interface

uses uai_types, SysUtils, fpjson, jsonparser;

procedure AiInfoToJson(const Player : Cardinal; const AiInfo: TAiInfo; var JsonStringBuilder: TStringBuilder);
procedure NewRoundToJson(const Strength: Byte; var JsonStringBuilder: TStringBuilder);

implementation

const
  JsonFormatSettings: TFormatSettings = (
    CurrencyFormat: 0;
    NegCurrFormat: 0;
    ThousandSeparator: #0;
    DecimalSeparator: '.';
    CurrencyDecimals: 0;
    DateSeparator: #0;
    TimeSeparator: #0;
    ListSeparator: #0;
    CurrencyString: '';
    ShortDateFormat: '';
    LongDateFormat: '';
    TimeAMString: '';
    TimePMString: '';
    ShortTimeFormat: '';
    LongTimeFormat: '';
    ShortMonthNames: ('', '', '', '', '', '', '', '', '', '', '', '');
    LongMonthNames: ('', '', '', '', '', '', '', '', '', '', '', '');
    ShortDayNames: ('', '', '', '', '', '', '');
    LongDayNames: ('', '', '', '', '', '', '');
    TwoDigitYearCenturyWindow: 0;
  );


function BoolToJsonBoolean(const b : Boolean) : String;
begin
  result := BoolToStr(b, 'true', 'false');
end;

var
  CommandJsonObj: TJSONObject;

function JsonToAiCommand(const jsonStr: string): TAiCommand;
begin
  Result.Action := apNone;
  Result.MoveState := amNone;

end;

procedure NewRoundToJson(const Strength: Byte; var JsonStringBuilder: TStringBuilder);
begin
  JsonStringBuilder.Clear;

  // Start des JSON-Objekts
  JsonStringBuilder.Append('{');
  JsonStringBuilder.Append('"type":"newround",');
  JsonStringBuilder.Append('"Strength":');
  JsonStringBuilder.Append(IntToStr(Strength));
  JsonStringBuilder.Append('}');
end;

procedure AiInfoToJson(const Player : Cardinal; const AiInfo: TAiInfo; var JsonStringBuilder: TStringBuilder);
var
  i, j: integer;
begin
  JsonStringBuilder.Clear;

  // Start des JSON-Objekts
  JsonStringBuilder.Append('{');
  JsonStringBuilder.Append('"type":"info",');
  JsonStringBuilder.Append('"player":');
  JsonStringBuilder.Append(IntToStr(Player));
  JsonStringBuilder.Append(',');

  // Teamplay-Flag
  JsonStringBuilder.Append('"Teamplay":');
  JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.Teamplay));
  JsonStringBuilder.Append(',');

  // PlayerInfos-Array
  JsonStringBuilder.Append('"PlayerInfos":[');
  for i := 0 to High(AiInfo.PlayerInfos) do
  begin
    // PlayerInfo-Objekt
    JsonStringBuilder.Append('{');
    JsonStringBuilder.Append('"Team":');
    JsonStringBuilder.Append(IntToStr(AiInfo.PlayerInfos[i].Team));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Position":{"x":');
    JsonStringBuilder.Append(FloatToStr(AiInfo.PlayerInfos[i].Position.x, JsonFormatSettings));
    JsonStringBuilder.Append(',"y":');
    JsonStringBuilder.Append(FloatToStr(AiInfo.PlayerInfos[i].Position.y, JsonFormatSettings));
    JsonStringBuilder.Append('},');
    JsonStringBuilder.Append('"Alive":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.PlayerInfos[i].Alive));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Flying":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.PlayerInfos[i].Flying));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"FlameLength":');
    JsonStringBuilder.Append(IntToStr(AiInfo.PlayerInfos[i].FlameLength));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"AvailableBombs":');
    JsonStringBuilder.Append(IntToStr(AiInfo.PlayerInfos[i].AvailableBombs));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Speed":');
    JsonStringBuilder.Append(FloatToStr(AiInfo.PlayerInfos[i].Speed, JsonFormatSettings));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Abilities":');
    JsonStringBuilder.Append(IntToStr(AiInfo.PlayerInfos[i].Abilities));
    JsonStringBuilder.Append('}');

    if i < High(AiInfo.PlayerInfos) then
      JsonStringBuilder.Append(',');
  end;
  JsonStringBuilder.Append('],');

  // Feld-Array
  JsonStringBuilder.Append('"Field":[');
  for i := 0 to High(AiInfo.Field) do
  begin
    JsonStringBuilder.Append('[');
    for j := 0 to High(AiInfo.Field[i]) do
    begin
      JsonStringBuilder.Append(IntToStr(Ord(AiInfo.Field[i][j])));
      if j < High(AiInfo.Field[i]) then
        JsonStringBuilder.Append(',');
    end;
    JsonStringBuilder.Append(']');

    if i < High(AiInfo.Field) then
      JsonStringBuilder.Append(',');
  end;
  JsonStringBuilder.Append('],');

  // BombsCount-Flag
  JsonStringBuilder.Append('"BombsCount":');
  JsonStringBuilder.Append(IntToStr(AiInfo.BombsCount));
  JsonStringBuilder.Append(',');

  // Bombs-Array
  JsonStringBuilder.Append('"Bombs":[');
  for i := 0 to High(AiInfo.Bombs) do
  begin
    JsonStringBuilder.Append('{');
    JsonStringBuilder.Append('"Position":{"x":');
    JsonStringBuilder.Append(FloatToStr(AiInfo.Bombs[i].Position.x, JsonFormatSettings));
    JsonStringBuilder.Append(',"y":');
    JsonStringBuilder.Append(FloatToStr(AiInfo.Bombs[i].Position.y, JsonFormatSettings));
    JsonStringBuilder.Append('},');
    JsonStringBuilder.Append('"FlameLength":');
    JsonStringBuilder.Append(IntToStr(AiInfo.Bombs[i].FlameLength));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Flying":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.Bombs[i].Flying));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Owner":');
    JsonStringBuilder.Append(IntToStr(AiInfo.Bombs[i].Owner));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"ManualTrigger":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.Bombs[i].ManualTrigger));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"Jelly":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.Bombs[i].Jelly));
    JsonStringBuilder.Append(',');
    JsonStringBuilder.Append('"DudBomb":');
    JsonStringBuilder.Append(BoolToJsonBoolean(AiInfo.Bombs[i].DudBomb));
    JsonStringBuilder.Append('}');

    if i < High(AiInfo.Bombs) then
      JsonStringBuilder.Append(',');
  end;
  JsonStringBuilder.Append(']');

  // Ende des JSON-Objekts
  JsonStringBuilder.Append('}');
end;

(*
{
   "Teamplay":true,
   "PlayerInfos":[
      {
         "Team":1,
         "Position":{
            "x":10.0,
            "y":20.0
         },
         "Alive":true,
         "Flying":false,
         "FlameLength":3,
         "AvailableBombs":2,
         "Speed":1.0,
         "Abilities":11
      },
      {
         "Team":2,
         "Position":{
            "x":30.0,
            "y":40.0
         },
         "Alive":false,
         "Flying":true,
         "FlameLength":2,
         "AvailableBombs":1,
         "Speed":0.5,
         "Abilities":21
      }
   ],
     "Field": [
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 1],
    [1, 0, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 0, 1, 1, 1, 0, 0, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 0, 1, 0, 1, 0, 0, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 0, 1, 1, 1, 0, 0, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 1],
    [1, 0, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 0, 0, 1],
    [1, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,]
   "BombsCount":2,
   "Bombs":[
      {
         "Position":{
            "x":50.0,
            "y":60.0
         },
         "FlameLength":3,
         "Flying":true,
         "Owner":1,
         "ManualTrigger":false,
         "Jelly":true,
         "DudBomb":false
      },
      {
         "Position":{
            "x":70.0,
            "y":80.0
         },
         "FlameLength":2,
         "Flying":false,
         "Owner":2,
         "ManualTrigger":true,
         "Jelly":false,
         "DudBomb":true
      }
   ]
}

*)



end.
