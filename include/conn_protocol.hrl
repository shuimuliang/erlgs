%% -*- coding: utf-8 -*-

-record(connstate, {
          socket,
          host,                     % 接入IP
          port,
          transport
         }).
