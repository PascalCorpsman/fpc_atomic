(******************************************************************************)
(* uip.pas                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Unit to get all Adapternames and their assosiated            *)
(*               IP-Addresses                                                 *)
(*                                                                            *)
(* Inspired by : https://forum.lazarus.freepascal.org/index.php?topic=24506.0 *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Add Darwin support by Pavel Zverina                   *)
(*               0.03 - Read the adapters from the OS (getifaddrs /           *)
(*                      GetAdaptersAddresses) instead of parsing the output   *)
(*                      of ifconfig / ipconfig, add GetLANIPs                 *)
(*                                                                            *)
(******************************************************************************)
Unit uip;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TIPAddress = Array[0..3] Of Byte;

  TNetworkAdapter = Record
    IpAddress: String; // xxx.xxx.xxx.xxx -> IPV4
    SubnetMask: String; // xxx.xxx.xxx.xxx -> IPV4
    AdapterName: String;
    Description: String; // Windows: name of the hardware / driver, e.g. "VirtualBox Host-Only Ethernet Adapter", Unix: ''
  End;

  TNetworkAdapterList = Array Of TNetworkAdapter;

  (*
   * Call this to get all local Adapternames and their IPV-4 addresses
   * (only adapters that are up, without loopback)
   *)
Function GetLocalIPs(): TNetworkAdapterList;

(*
 * Like GetLocalIPs, but without the adapters of containers and virtual machines
 * (Docker, WSL, Hyper-V, libvirt, VirtualBox, VMware), they can not be reached from the local network.
 * Use this to show the own address to other players.
 *)
Function GetLANIPs(IncludeVirtual: Boolean = false): TNetworkAdapterList;

(*
 * True if Adapter is the adapter of a container or a virtual machine
 *)
Function IsVirtualAdapter(Const Adapter: TNetworkAdapter): Boolean;

(*
 * Windows: A UDP packet to a port where nobody listens (e.g. the search ping to 127.0.0.1 without
 * a local server) makes the next receive on the socket fail with WSAECONNRESET (10054), lNet then
 * closes the socket. This switches that off for the socket (SIO_UDP_CONNRESET). Elsewhere it does nothing.
 *)
Procedure IgnoreUdpConnReset(Handle: PtrInt);

(*
 * Calculates from a given Address and Subnet Mask the Broadcast address for the net
 * e.g. 192.168.168.1, 255.255.255.0 -> 192.168.168.255
 * !! Attention !!
 * this works only with IPV-4 addresses
 *)
Function CalculateBroadCastAddressFromAddress(Address, SubnetMask: String): String;

(*
 * Converts a IPV-4 String into TIPAddress, also doing some checks!
 *)
Function StrToIPAddress(Value: String): TIPAddress;

Function IsNetworkPresent(): Boolean;

Implementation

Uses
  StrUtils
{$IFDEF Unix}
  , ctypes, Sockets
{$ENDIF}
  ;

{$IFDEF Unix}
Const
  IFF_UP = $1;
  IFF_LOOPBACK = $8;
  IFF_RUNNING = $40;

Type
  PIfAddrs = ^TIfAddrs;
  TIfAddrs = Record // struct ifaddrs, same layout on Linux and Darwin
    ifa_next: PIfAddrs;
    ifa_name: PChar;
    ifa_flags: cuint;
    ifa_addr: psockaddr;
    ifa_netmask: psockaddr;
    ifa_dstaddr: psockaddr;
    ifa_data: Pointer;
  End;

Function getifaddrs(Var ifap: PIfAddrs): cint; cdecl; external 'c';
Procedure freeifaddrs(ifa: PIfAddrs); cdecl; external 'c';
{$ENDIF}

{$IFDEF Windows}
{$PACKRECORDS C}
Const
  AF_INET = 2;
  GAA_FLAG_SKIP_ANYCAST = $2;
  GAA_FLAG_SKIP_MULTICAST = $4;
  GAA_FLAG_SKIP_DNS_SERVER = $8;
  ERROR_BUFFER_OVERFLOW = 111;
  IF_TYPE_SOFTWARE_LOOPBACK = 24;
  IfOperStatusUp = 1;

Type
  TSocketAddress = Record // SOCKET_ADDRESS, has to be a record of its own because of the padding behind it
    lpSockaddr: Pointer;
    iSockaddrLength: Integer;
  End;

  PIpAdapterUnicastAddress = ^TIpAdapterUnicastAddress;
  TIpAdapterUnicastAddress = Record // IP_ADAPTER_UNICAST_ADDRESS, only the fields up to OnLinkPrefixLength (Vista+)
    Length: Cardinal;
    Flags: Cardinal;
    Next: PIpAdapterUnicastAddress;
    Address: TSocketAddress;
    PrefixOrigin: Integer;
    SuffixOrigin: Integer;
    DadState: Integer;
    ValidLifetime: Cardinal;
    PreferredLifetime: Cardinal;
    LeaseLifetime: Cardinal;
    OnLinkPrefixLength: Byte;
  End;

  PIpAdapterAddresses = ^TIpAdapterAddresses;
  TIpAdapterAddresses = Record // IP_ADAPTER_ADDRESSES, only the fields up to OperStatus
    Length: Cardinal;
    IfIndex: Cardinal;
    Next: PIpAdapterAddresses;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIpAdapterUnicastAddress;
    FirstAnycastAddress: Pointer;
    FirstMulticastAddress: Pointer;
    FirstDnsServerAddress: Pointer;
    DnsSuffix: PWideChar;
    Description: PWideChar;
    FriendlyName: PWideChar;
    PhysicalAddress: Array[0..7] Of Byte;
    PhysicalAddressLength: Cardinal;
    Flags: Cardinal;
    Mtu: Cardinal;
    IfType: Cardinal;
    OperStatus: Integer;
  End;
{$PACKRECORDS DEFAULT}

Function GetAdaptersAddresses(Family, Flags: Cardinal; Reserved: Pointer;
  AdapterAddresses: PIpAdapterAddresses; SizePointer: PCardinal): Cardinal; stdcall; external 'iphlpapi.dll';
{$ENDIF}

Function StrToIPAddress(Value: String): TIPAddress;
Var
  t: TStringArray;
  i: Integer;
Begin
  t := value.Split('.');
  If length(t) <> 4 Then Raise Exception.Create('Invalid ip:' + Value);
  For i := 0 To 3 Do Begin
    If Not (strtointdef(t[i], -1) In [0..255]) Then Raise Exception.Create('Invalid ip:' + Value);
    result[i] := strtointdef(t[i], -1);
  End;
End;

(*
 * sockaddr_in has the IPV-4 address at offset 4 on all platforms
 * (Linux / Windows: 2 byte family + 2 byte port, Darwin: 1 byte len + 1 byte family + 2 byte port)
 *)
Function SockAddrToStr(Addr: PByte): String;
Begin
  result := format('%d.%d.%d.%d', [Addr[4], Addr[5], Addr[6], Addr[7]]);
End;

Function IsNetworkPresent: Boolean;
Var
  blub: TNetworkAdapterList;
Begin
  blub := GetLocalIPs();
  result := assigned(blub);
  setlength(blub, 0);
End;

Function IsVirtualAdapter(Const Adapter: TNetworkAdapter): Boolean;
Const
  // Linux: Docker (docker0, br-<id>, veth<id>), libvirt (virbr0), VirtualBox (vboxnet0), VMware (vmnet1, vmnet8)
  // Windows: Docker (vEthernet (DockerNAT)), WSL (vEthernet (WSL), vEthernet (WSL (Hyper-V firewall))), Hyper-V NAT (vEthernet (Default Switch)),
  //          VMware (VMware Network Adapter VMnet1)
  Prefixes: Array[0..5] Of String = ('br-', 'veth', 'virbr', 'vboxnet', 'vmnet', 'vethernet (wsl');
  Parts: Array[0..2] Of String = ('docker', 'vethernet (default switch)', 'vmware network adapter');
  // Windows: the name of a VirtualBox adapter is only "Ethernet 2", so check the description
  DescriptionParts: Array[0..1] Of String = ('virtualbox host-only', 'vmware virtual ethernet');
Var
  s: String;
  i: Integer;
Begin
  result := true;
  s := lowercase(Adapter.AdapterName);
  For i := 0 To high(Prefixes) Do
    If pos(Prefixes[i], s) = 1 Then exit;
  For i := 0 To high(Parts) Do
    If pos(Parts[i], s) <> 0 Then exit;
  s := lowercase(Adapter.Description);
  For i := 0 To high(DescriptionParts) Do
    If pos(DescriptionParts[i], s) <> 0 Then exit;
  result := false;
End;

Function GetLANIPs(IncludeVirtual: Boolean): TNetworkAdapterList;
Var
  all: TNetworkAdapterList;
  i: Integer;
Begin
  result := Nil;
  all := GetLocalIPs();
  For i := 0 To high(all) Do Begin
    If (Not IncludeVirtual) And IsVirtualAdapter(all[i]) Then Continue;
    setlength(result, high(result) + 2);
    result[high(result)] := all[i];
  End;
End;

{$IFDEF Unix}

Function GetLocalIPs: TNetworkAdapterList;
Var
  list, p: PIfAddrs;
Begin
  result := Nil;
  list := Nil;
  If getifaddrs(list) <> 0 Then exit;
  Try
    p := list;
    While assigned(p) Do Begin
      If assigned(p^.ifa_addr) And assigned(p^.ifa_netmask)
        And (p^.ifa_addr^.sa_family = AF_INET)
        And ((p^.ifa_flags And IFF_UP) <> 0)
        And ((p^.ifa_flags And IFF_RUNNING) <> 0)
        And ((p^.ifa_flags And IFF_LOOPBACK) = 0) Then Begin
        setlength(result, high(result) + 2);
        result[high(result)].IpAddress := SockAddrToStr(PByte(p^.ifa_addr));
        result[high(result)].SubnetMask := SockAddrToStr(PByte(p^.ifa_netmask));
        result[high(result)].AdapterName := p^.ifa_name;
        result[high(result)].Description := '';
      End;
      p := p^.ifa_next;
    End;
  Finally
    freeifaddrs(list);
  End;
End;
{$ENDIF}

{$IFDEF Windows}

Function PrefixLengthToMask(PrefixLength: Byte): String;
Var
  m: Cardinal;
Begin
  If PrefixLength = 0 Then
    m := 0
  Else If PrefixLength >= 32 Then
    m := $FFFFFFFF
  Else
    m := Cardinal($FFFFFFFF) Shl (32 - PrefixLength);
  result := format('%d.%d.%d.%d', [(m Shr 24) And $FF, (m Shr 16) And $FF, (m Shr 8) And $FF, m And $FF]);
End;

Function GetLocalIPs: TNetworkAdapterList;
Var
  buffer: Array Of Byte;
  size, res: Cardinal;
  p: PIpAdapterAddresses;
  u: PIpAdapterUnicastAddress;
  i: Integer;
Begin
  result := Nil;
  buffer := Nil;
  res := ERROR_BUFFER_OVERFLOW;
  size := 16 * 1024;
  // The adapter list can grow between two calls, so retry a few times
  For i := 0 To 2 Do Begin
    setlength(buffer, size);
    res := GetAdaptersAddresses(AF_INET, GAA_FLAG_SKIP_ANYCAST Or GAA_FLAG_SKIP_MULTICAST Or GAA_FLAG_SKIP_DNS_SERVER,
      Nil, PIpAdapterAddresses(@buffer[0]), @size);
    If res <> ERROR_BUFFER_OVERFLOW Then break;
  End;
  If res <> 0 Then exit;
  p := PIpAdapterAddresses(@buffer[0]);
  While assigned(p) Do Begin
    If (p^.OperStatus = IfOperStatusUp) And (p^.IfType <> IF_TYPE_SOFTWARE_LOOPBACK) Then Begin
      u := p^.FirstUnicastAddress;
      While assigned(u) Do Begin
        If assigned(u^.Address.lpSockaddr) And (PWord(u^.Address.lpSockaddr)^ = AF_INET) Then Begin
          setlength(result, high(result) + 2);
          result[high(result)].IpAddress := SockAddrToStr(PByte(u^.Address.lpSockaddr));
          result[high(result)].SubnetMask := PrefixLengthToMask(u^.OnLinkPrefixLength);
          result[high(result)].AdapterName := UTF8Encode(WideString(p^.FriendlyName));
          result[high(result)].Description := UTF8Encode(WideString(p^.Description));
        End;
        u := u^.Next;
      End;
    End;
    p := p^.Next;
  End;
End;
{$ENDIF}

Function CalculateBroadCastAddressFromAddress(Address, SubnetMask: String): String;
Var
  BroadcastAddr, Addr, Mask: TIPAddress;
  M: String;
  b: Boolean;
  i: Integer;
Begin
  Addr := StrToIPAddress(Address);
  Mask := StrToIPAddress(SubnetMask);
  // Check if subnet mask is valid
  m := inttobin(mask[0], 8) + inttobin(mask[1], 8) + inttobin(mask[2], 8) + inttobin(mask[3], 8);
  If m[1] <> '1' Then Raise Exception.Create('Invalid subnet mask:' + SubnetMask);
  b := true;
  For i := 1 To length(m) Do Begin
    If b Then Begin
      If m[i] <> '1' Then b := false;
    End
    Else Begin
      If m[i] <> '0' Then Begin
        Raise Exception.Create('Invalid subnet mask:' + SubnetMask);
      End;
    End;
  End;
  // Calculate the Broadcast address
  For i := 0 To 3 Do Begin
    BroadcastAddr[i] := Addr[i] Or (Not Mask[i]);
  End;
  result := format('%d.%d.%d.%d', [BroadcastAddr[0], BroadcastAddr[1], BroadcastAddr[2], BroadcastAddr[3]]);
End;

{$IFDEF Windows}
Function WSAIoctl(s: PtrInt; dwIoControlCode: Cardinal; lpvInBuffer: Pointer; cbInBuffer: Cardinal;
  lpvOutBuffer: Pointer; cbOutBuffer: Cardinal; lpcbBytesReturned: PCardinal; lpOverlapped: Pointer;
  lpCompletionRoutine: Pointer): Integer; stdcall; external 'ws2_32.dll';

Procedure IgnoreUdpConnReset(Handle: PtrInt);
Const
  SIO_UDP_CONNRESET = $9800000C;
Var
  Enable: LongBool;
  Returned: Cardinal;
Begin
  Enable := false;
  Returned := 0;
  WSAIoctl(Handle, SIO_UDP_CONNRESET, @Enable, SizeOf(Enable), Nil, 0, @Returned, Nil, Nil);
End;
{$ELSE}

Procedure IgnoreUdpConnReset(Handle: PtrInt);
Begin
End;
{$ENDIF}

End.

