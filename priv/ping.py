import socket

from struct import pack, unpack, calcsize

StructFormat = "!I"
PrefixLength = calcsize(StructFormat)

def prepare_ping():
    from pub_pb2 import ABSMessage
    from proto_pb2 import PingSend
    pingmsg = PingSend()
    pingmsg.msg = 'ping'

    res = ABSMessage()
    res.action = 1
    res.msg = pingmsg.SerializeToString()
    res_buf = res.SerializeToString()
    sendbuf = pack(StructFormat, len(res_buf)) + res_buf

    return sendbuf

def main():
    host = 'localhost'
    port = 9102
    addr = (host,port)
    client = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    client.connect(addr)

    sendbuf = '\x00\x00\x00\n\x08\x01\x12\x06\n\x04ping'
    client.send(sendbuf)
    client.close()

if __name__ == '__main__':
    main()
