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
  classes, TcpIpBase, TcpIpUtils, TcpIpClient, sysutils;

type

  { TTcpIpWebSocket }

  TTcpIpWebSocket = class(TTcpIpClientSocket)
  private
    FHandShakeDone: boolean;
    fResourceName: string;
    key: string;
    function CheckForHandShake: boolean;
    procedure SendSwitchHeader;
  public
    function Write(const ABuffer; ACount: LongInt): LongInt; override;
    function Read(var ABuffer; ACount: LongInt): LongInt; override;

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


Procedure TTcpIpWebSocket.SendSwitchHeader;
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
    WriteStr(wrkstr);
    FHandShakeDone:=true;
  finally
    HttpResponse.free;
  end;

end;

function TTcpIpWebSocket.CheckForHandShake:boolean;
var
  Buf: Array [0..8192-1] of Char;
  cnt: integer;
  HttpRequest: TStringList;
  wrkstr: string;

begin
  Result:= false;
  cnt:=inherited Read(Buf, 8192);
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

function TTcpIpWebSocket.Write(const ABuffer; ACount: LongInt): LongInt;
begin
  Result:=inherited Write(ABuffer, ACount);
end;

function TTcpIpWebSocket.Read(var ABuffer; ACount: LongInt): LongInt;
begin
  if not FHandShakeDone then
    begin
      if CheckForHandShake then
        SendSwitchHeader
      else
        exit;
    end;
  Result:=inherited Read(ABuffer, ACount);
end;

end.

