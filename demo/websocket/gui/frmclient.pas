unit frmclient;

{$mode objfpc}{$H+}

interface

uses
  TcpIpClient, tcpipwebsocket, Classes, SysUtils, Forms, StdCtrls, Controls;

type

  { TfrClient }

  TfrClient = class(TForm)
    btSend: TButton;
    edText: TEdit;
    edMsg: TMemo;
    procedure btSendClick(Sender: TObject);
    procedure edTextKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    FClient: TTcpIpWebSocket;
    procedure OnText(ws: TTcpIpWebSocket; const Data: string);
  end;

var
  frClient: TfrClient;
  NickName: string;

implementation

{$R *.lfm}


{ TfrClient }

procedure TfrClient.FormCreate(Sender: TObject);
begin
  FClient := TTcpIpWebSocket.Create('ws://localhost:4100/chat','localhost');
  FClient.OnText:= @OnText;
  FClient.Connect;
end;

procedure TfrClient.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

procedure TfrClient.OnText(ws: TTcpIpWebSocket; const Data: string);
begin
  edMsg.Lines.Add(Data);
end;

procedure TfrClient.btSendClick(Sender: TObject);
var
  VData: string;
begin
  if Trim(edText.Text) = '' then
    Exit;
  VData := NickName + ': ' + edText.Text;
  FClient.WriteString(VData);
  edText.Clear;
  if edText.CanFocus then
    edText.SetFocus;
end;

procedure TfrClient.edTextKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btSendClick(Sender);
  end;
end;

end.

