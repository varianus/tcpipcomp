unit frmserver;

{$mode objfpc}{$H+}

interface

uses
  TcpIpServer, TcpIpClient, tcpipwebsocket, Classes, SysUtils, Forms, StdCtrls;

type
  TServerThread = class;

  { TfrServer }

  TfrServer = class(TForm)
    edMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure DoClientReceive(Sender: TObject; const AData: string);
    procedure DoClientConnect(Sender: TObject; const ASocket: LongInt);
    procedure DoClientDisconnect(Sender: TObject; const ASocket: LongInt);
  private
    FServer: TServerThread;
  end;

  TClientReceiveEvent = procedure(Sender: TObject;
    const AData: string) of object;
  TClientConnectEvent = procedure(Sender: TObject;
    const ASocket: LongInt) of object;

  { TServerThread }

  TServerThread = class(TThread)
  private
    FClients: TThreadList;
    FOnClientConnect: TClientConnectEvent;
    FOnClientDisconnect: TClientConnectEvent;
    FOnClientReceive: TClientReceiveEvent;
    FServer: TTcpIpServerSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure TerminateClients;
    property Clients: TThreadList read FClients;
    property OnClientConnect: TClientConnectEvent read FOnClientConnect
      write FOnClientConnect;
    property OnClientDisconnect: TClientConnectEvent read FOnClientDisconnect
      write FOnClientDisconnect;
    property OnClientReceive: TClientReceiveEvent read FOnClientReceive
      write FOnClientReceive;
  end;

  { TClientObject }

  TClientObject = class(TObject)
  private
    FData: string;
    FOnConnect: TClientConnectEvent;
    FOnDisconnect: TClientConnectEvent;
    FOnReceive: TClientReceiveEvent;
    FSocket: TTcpIpwebsocket;
    FOwner: TServerThread;
  protected
    procedure DoClientConnect;
    procedure DoClientDisconnect;
    procedure DoClientReceive(Const data:string);
    procedure GetData (ws: TTcpIpWebSocket; const Data: string);
  public
    constructor Create(AOwner: TServerThread; const ASocket: LongInt);
    destructor Destroy; override;
    procedure Accept;
    property OnConnect: TClientConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TClientConnectEvent read FOnDisconnect write FOnDisconnect;
    property OnReceive: TClientReceiveEvent read FOnReceive write FOnReceive;
    property Socket: TTcpIpwebSocket read FSocket;
  end;

var
  frServer: TfrServer;

implementation

{$R *.lfm}

{ TServerThread }

constructor TServerThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClients := TThreadList.Create;
  FServer := TTcpIpServerSocket.Create(4100);
end;

destructor TServerThread.Destroy;
begin
  FClients.Free;
  FServer.Free;
  inherited Destroy;
end;

procedure TServerThread.Execute;
var
  VSocket: LongInt;
  VClient: TClientObject;
begin
  FServer.Bind;
  FServer.Listen;
  while (not Terminated) or (FServer.LastError = 0) do
  begin
    VSocket := FServer.Accept;
    if VSocket = -1 then
      Break;
    VClient := TClientObject.Create(Self, VSocket);
  end;
end;

procedure TServerThread.TerminateClients;
var
  VList: TList;
  VItem: Pointer;
  VClient: TClientObject absolute VItem;
begin
  VList := FClients.LockList;
  try
    for VItem in VList do
      if Assigned(VClient) then
      begin
        VClient.free;
      end;
  finally
    FClients.UnlockList;
  end;
end;

{ TClientObject }

constructor TClientObject.Create(AOwner: TServerThread; const ASocket: LongInt);
begin
  FOwner := AOwner;
  FSocket := TTcpIpWebSocket.Create(ASocket);
  if Assigned(FOwner) then
  begin
    FOwner.Clients.Add(Self);
    FOnConnect := FOwner.OnClientConnect;
    FOnDisconnect := FOwner.OnClientDisconnect;
    FOnReceive := FOwner.OnClientReceive;
  end;

 Accept;

end;

destructor TClientObject.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Clients.Remove(Self);

  inherited Destroy;
end;

procedure TClientObject.Accept;
var
  VDataSize: Integer;
begin
  DoClientConnect;
  try
     FSocket.OnText:= @GetData;
    if not FSocket.Listen then
       exit;

  finally
{$IFDEF UNIX}
    Sleep(100);
{$ENDIF}
    DoClientDisconnect;
  end;
end;

procedure TClientObject.DoClientConnect;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self, ptruint(FSocket));
end;

procedure TClientObject.DoClientDisconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, ptruint(FSocket));
end;

procedure TClientObject.DoClientReceive(Const Data:string);
var
  VList: TList;
  VItem: Pointer;
  VClient: TClientObject absolute VItem;
begin
  if not Assigned(FOwner) then
    Exit;
  if not Assigned(FOwner.FServer) then
    Exit;
  if not FOwner.FServer.IsConnected then
    Exit;
  VList := FOwner.Clients.LockList;
  try
    for VItem in VList do
    begin
      if not Assigned(VClient.Socket) then
        Continue;
      FData := DateTimeToStr(Now) + '- ' + Data;
      VClient.Socket.WriteString(FData);
      if Assigned(FOnReceive) and (VClient <> Self) then
        FOnReceive(Self, FData);
    end;
  finally
    FOwner.Clients.UnlockList;
  end;
end;

procedure TClientObject.GetData(ws: TTcpIpWebSocket; const Data: string);
begin
  DoClientReceive(Data);
end;

{ TfrServer }

procedure TfrServer.FormCreate(Sender: TObject);
begin
  FServer := TServerThread.Create;
  FServer.Start;
  FServer.OnClientReceive := @DoClientReceive;
  FServer.OnClientConnect := @DoClientConnect;
  FServer.OnClientDisconnect := @DoClientDisconnect;
end;

procedure TfrServer.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) and not FServer.Finished then
  begin
    FServer.FreeOnTerminate := False;
    FServer.Terminate;
    FServer.TerminateClients;
    FreeAndNil(FServer);
  end;
end;

procedure TfrServer.DoClientReceive(Sender: TObject; const AData: string);
begin
  edMsg.Lines.Add(AData);
end;

procedure TfrServer.DoClientConnect(Sender: TObject; const ASocket: LongInt);
begin
  edMsg.Lines.Add('Connected: ' + IntToStr(ASocket));
end;

procedure TfrServer.DoClientDisconnect(Sender: TObject; const ASocket: LongInt);
begin
  edMsg.Lines.Add('Disconnected: ' + IntToStr(ASocket));
end;

end.
