{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tcpipcomp;

{$warn 5023 off : no warning about unused units}
interface

uses
  TcpIpBase, TcpIpClient, TcpIpServer, TcpIpUtils, tcpipwebsocket, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('tcpipcomp', @Register);
end.
