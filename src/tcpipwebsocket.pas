(*
  Useful classes for TCP/IP communication.
  Copyright (c) 2013 by Silvio Clecio, Gilson Nunes Rodrigues and Waldir Paim
  Copyright (c) 2017 by Marco Caselli

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit tcpipwebsocket;

{$mode objfpc}{$H+}

interface

uses
  classes, TcpIpBase, TcpIpUtils, TcpIpClient, sysutils, URIParser;

type

  { TTcpIpWebSocket }
  TTcpIpWebSocket = class;

  { TcpipListenThread }

  TcpipListenThread = class(TThread)
  private
    FWebSocket: TTcpIpWebSocket;
  public
    constructor Create(websocket: TTcpIpWebSocket);
    procedure Execute; override;
    Destructor Destroy; override;
  end;

  TTcpIpWebSocket = class
  private
    FHandShakeDone: boolean;
    fResourceName: string;
    ssl:boolean;
    key: string;
    fURI: TURI;
    fOrigin: string;
    fListener : TcpipListenThread;
    IntSocket: TTcpIpClientSocket;
    procedure Output(b: Byte; const data; len: Int64; Mask: boolean=false);
    function ReadHandShake: boolean;
    procedure SendHandShake;
    procedure SendSwitchHeader;

  protected
    function Write(const ABuffer; ACount: LongInt): LongInt;
    Procedure ReadFrame;
  public
    function Listen: boolean;
    constructor Create(Const URL:string; origin:string);
    constructor Create(const ASocket: LongInt);
  end;

const
    {:Constants section defining close codes}
  {:Normal valid closure, connection purpose was fulfilled}
  wsCloseNormal = 1000;
  {:Endpoint is going away (like server shutdown) }
  wsCloseShutdown = 1001;
  {:Protocol error }
  wsCloseErrorProtocol = 1002;
  {:Unknown frame data type or data type application cannot handle }
  wsCloseErrorData = 1003;
  {:Reserved }
  wsCloseReserved1 = 1004;
  {:Close received by peer but without any close code. This close code MUST NOT be sent by application. }
  wsCloseNoStatus = 1005;
  {:Abnotmal connection shutdown close code. This close code MUST NOT be sent by application. }
  wsCloseErrorClose = 1006;
  {:Received text data are not valid UTF-8. }
  wsCloseErrorUTF8 = 1007;
  {:Endpoint is terminating the connection because it has received a message that violates its policy. Generic error. }
  wsCloseErrorPolicy = 1008;
  {:Too large message received }
  wsCloseTooLargeMessage = 1009;
  {:Client is terminating the connection because it has expected the server to negotiate one or more extension, but the server didn't return them in the response message of the WebSocket handshake }
  wsCloseClientExtensionError = 1010;
  {:Server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request }
  wsCloseErrorServerRequest = 1011;
  {:Connection was closed due to a failure to perform a TLS handshake. This close code MUST NOT be sent by application. }
  wsCloseErrorTLS = 1015;


  function EncodeBufferBase64(const s: String):String;

implementation
uses
  strutils, base64,  sha1;

const
  {:Constants section defining what kind of data are sent from one pont to another}
  {:Continuation frame }
  wsCodeContinuation = $0;
  {:Text frame }
  wsCodeText = $1;
  {:Binary frame }
  wsCodeBinary = $2;
  {:Close frame }
  wsCodeClose = $8;
  {:Ping frame }
  wsCodePing = $9;
  {:Frame frame }
  wsCodePong = $A;

{ TTcpIpWebSocket }

function EncodeBufferBase64(const s: string):String;

var
  Digest: TSHA1Digest;
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Digest := SHA1String(s);
      Encoder.Write(digest,sizeof(Digest));
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
    end;
end;

{ TcpipListenThread }

constructor TcpipListenThread.Create(websocket: TTcpIpWebSocket);
begin
  inherited Create(False);
  FWebSocket:= websocket;
  FreeOnTerminate := True;
end;

procedure TcpipListenThread.Execute;
begin
  repeat
     if Terminated then
        break;
     if FWebSocket.IntSocket.CanRead(60000) then
        begin
          Synchronize(@FWebSocket.ReadFrame);
        end;
  until Terminated;
end;

destructor TcpipListenThread.Destroy;
begin
  inherited Destroy;
end;


procedure TTcpIpWebSocket.SendSwitchHeader;
var
  HttpResponse: TStringList;
  wrkstr: string;
begin
  HttpResponse:= TStringList.Create;
  try
    HttpResponse.LineBreak:=#13#10;
    HttpResponse.NameValueSeparator:=':';
    HttpResponse.Add('HTTP/1.1 101 Switching Protocols');
    HttpResponse.Values['Upgrade'] := ' websocket';
    HttpResponse.Values['Connection'] := ' upgrade';
    wrkstr := EncodeBufferBase64(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');

    HttpResponse.Values['Sec-WebSocket-Accept'] := wrkstr;
    HttpResponse.Add('');
    wrkstr := HttpResponse.Text;
    IntSocket.WriteStr(wrkstr);
    FHandShakeDone:=true;
  finally
    HttpResponse.free;
  end;
end;

procedure TTcpIpWebSocket.SendHandShake;
var
  HttpResponse: TStringList;
  wrkstr: string;
  g:TGUID;
begin
  HttpResponse:= TStringList.Create;
  try
    HttpResponse.LineBreak:=#13#10;
    HttpResponse.NameValueSeparator:=':';
    HttpResponse.Add('GET '+fUri.Document+' HTTP/1.1');
    HttpResponse.Values['Upgrade'] := ' websocket';
    HttpResponse.Values['origin'] := fOrigin;
    HttpResponse.Values['Connection'] := ' upgrade';
    CreateGUID(G);
    key:= EncodeBufferBase64(GUIDToString(g));
    HttpResponse.Values['Sec-WebSocket-Key'] := key;
    HttpResponse.Values['Sec-WebSocket-Vesrion'] := '13';
    HttpResponse.Add('');
    wrkstr := HttpResponse.Text;
    IntSocket.WriteStr(wrkstr);
  finally
    HttpResponse.free;
  end;
end;

function TTcpIpWebSocket.ReadHandShake:boolean;
var
  Buf: Array [0..8192-1] of Char;
  cnt: integer;
  HttpRequest: TStringList;
  wrkstr: string;

begin
  Result:= false;
  cnt:= IntSocket.Read(Buf, 8192);
  if cnt = 0 then
    exit;
  HttpRequest:= TStringList.Create;
  try
    HttpRequest.LineBreak:=#13#10;
    HttpRequest.NameValueSeparator:=':';
    HttpRequest.SetText(Buf);
    if HttpRequest.Count < 5 then
      exit;
    wrkstr := HttpRequest[0];
    if ((Pos('GET ', Uppercase(wrkstr)) = 0) or
        (Pos(' HTTP/1.1', Uppercase(wrkstr)) = 0)) then
      exit;

    Copy2SpaceDel(wrkstr);
    fResourceName:=Copy2Space(wrkstr);

    wrkstr:= HttpRequest.Values['sec-websocket-key'];
    if wrkstr = '' then
      exit;

    if (Length(DecodeStringBase64(wrkstr)) = 16) then
      key := trim(wrkstr)
    else
      exit;

    if (LowerCase(trim(HttpRequest.Values['Upgrade'])) <> LowerCase('websocket')) or
      (pos('upgrade', LowerCase(trim(HttpRequest.Values['Connection']))) = 0) then
      exit;

    result:= true;
  finally
    HttpRequest.Free;
  end;


end;

procedure TTcpIpWebSocket.Output(b: Byte; const data; len: Int64; Mask: boolean=false);
var
  lenarray: array[0..7] of Byte absolute len;
  d: Cardinal;
  p: Pointer;
  g: TGUID;
  bitMask: byte;
begin
    Intsocket.Write(b, 1);
    if mask then
      bitMask:=$80
    else
      bitMask:=$00;

    if len < 126 then
    begin
      b := len or bitMask;
      Intsocket.Write(b, 1);
    end else
      if len < High(Word) then
      begin
        b := 126 or bitMask;
        Intsocket.Write(b, 1);
        Intsocket.Write(lenarray[1], 1);
        Intsocket.Write(lenarray[0], 1);
      end else
      begin
        b := 127 or bitMask;
        Intsocket.Write(b, 1);
        Intsocket.Write(lenarray[7], 1);
        Intsocket.Write(lenarray[6], 1);
        Intsocket.Write(lenarray[5], 1);
        Intsocket.Write(lenarray[4], 1);
        Intsocket.Write(lenarray[3], 1);
        Intsocket.Write(lenarray[2], 1);
        Intsocket.Write(lenarray[1], 1);
        Intsocket.Write(lenarray[0], 1);
      end;

    if Mask then
      begin
        CreateGUID(g); // entropy
        Intsocket.Write(g.D1, SizeOf(g.D1));
        p := @data;
        while len >= 4 do
          begin
            d := Cardinal(p^) xor g.D1;
            Intsocket.Write(d, SizeOf(d));
            Inc(NativeInt(p), 4);
            Dec(len, 4);
          end;
        if len > 0 then
          begin
            Move(p^, d, len);
            d := d xor g.D1;
            Intsocket.Write(d, len);
          end;
      end
    else
      Intsocket.Write(data, len);

  end;


function TTcpIpWebSocket.Write(const ABuffer; ACount: LongInt): LongInt;
begin
  Output($80 or wsCodeText, ABuffer, ACount);
end;


function TTcpIpWebSocket.Listen:boolean;
begin
  if not FHandShakeDone then
    begin
      if ReadHandShake then
        SendSwitchHeader
      else
        exit;
    end;
  fListener := TcpipListenThread.Create(Self);
  fListener.Start;
end;


Procedure TTcpIpWebSocket.ReadFrame;
var
  Buf: Array [0..8192-1] of Char;
begin
  // only to empty input buffer ......
  IntSocket.Read(Buf, 8192);

  Write('I am ALIVE!',11);
end;

constructor TTcpIpWebSocket.Create(Const URL:string; origin:string);
begin
  fURI := URIParser.ParseURI(URL);
  if fUri.protocol = 'ws' then
   begin
     ssl := False;
     if fUri.port = 0 then
       fUri.port := 80;
   end else
     if fUri.protocol = 'wss' then
     begin
       ssl := True;
       if fUri.port = 0 then
         fUri.port := 443;
     end;
  fOrigin:=origin;
  IntSocket.Create(fURI.Host, fUri.Port);
  SendHandShake;
end;

constructor TTcpIpWebSocket.Create(const ASocket: LongInt);
begin
  IntSocket:= TTcpIpClientSocket.Create(ASocket);
end;

end.

