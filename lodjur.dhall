{ cfgHttp =
    { httpPort = 4000
    -- , httpStaticDir = Some "/home/shaun/devel/mpowered/lodjur/static"
    , httpStaticDir = None Text
    }
, cfgGithub =
    { githubWebhookSecret = "secret"
    , githubAppId = 25289
    , githubAppPrivateKey = ''
        -----BEGIN RSA PRIVATE KEY-----
        MIIEpQIBAAKCAQEArcPz5g8+GAbJwj8Ai9RwFAHU5PftvSa1+JvtRBq4fx7gVqHU
        76mELs7WC7tuf7thvmvtzKcSlwOg+G2Crhp3mLyAAXPa7wU4qtzESI81hMe2Vyfw
        Gwlp8kWh5Q29lhRSIiFj+VwJzmvZis6wasvXSoMFr0Ih4pV3JjeuLUVeWXA33jPy
        Z6Dl8OI0K0dRZwWg6lBD9yZZqMxRyNdk+9Zc2ljDbFnjlXTaKIVp0Dcr5R5yXyel
        g5YZhdKRU0bAKxrxjsP85oJEW9kHqQU2sMpMcO3f2Py7PS7dMgH4GiIxMO2SyEIw
        TpVSnLIcESV74eMooIVkO8k7/fPySFvlHyaQvwIDAQABAoIBAGYsutWkUNa/5xMZ
        CLEwEVcyXITmZBy9DnNI/JL3Q1U8Sp2mOxdRjjAX3zS22ZqyDB90Rzf3Za72q0ho
        3FaTqY9eB8bS4QF7CH8voRspWkbhSA6npT/AoVOcLj4JflkpScJX7/lIbVu7fwdn
        0cR6Z/WgZU9Qi6w4Rv9p7jkgeQINSgMtx1/XUiDEQcUQ4+BkDR5tFwYtPdEtKDhV
        1ag5j3Alp71nT0futTYU9r3SWiZoPtNfGNUSwciLPYG9muiolG631aV9CXXyR71n
        IqPQZ7xegeUwlGyP6rCLsKPUbLxMr+53TAkTNPiphd2JOjDhgvaHKB37zlqpxo+T
        0wCU74ECgYEA3JHy2hknAk0nDNDUk0ooV4e/wXhysY6KR9cSJDCfhOC46SZKQcn8
        tC2EJ8Qxp4pDfYZs3yZsFHdEyOIoW+7+o74VPgVW5LSzObCxEe9FXvdeuRNhQGk8
        VDm+aUAbFMT6uq1QzXY3GyDejvyMM58ST/dCQ+e84QdWdPpT8SiOYicCgYEAya1a
        DmJcbwni/uauNKvFp1RA9+yGPoOEx6LPT5S+5bp0Rj6FUTtaZ/R0ICzOCKDHQgrb
        9m67OERNple/cYhZwlBcqRuceTImgsWVI8ecV07lNHVytO88MvpY6hriYk18rKos
        krn5XDwCVB164pSU34RCVo4ptzvBkR5zSfnWM6kCgYEAvj8Id8OAGQUQ7IXnBI/P
        EghdhfNtAecPMH26AQsLXCI0e6zyGaKt237y4RXhOkAQjGF44FSUnmZtJtOiugD/
        E56tVcxnMWcGu3jCkdSOQiYdRocHt/XM+ly/9qo7cYOhO4ioDD+tsjF8pVoBV7kZ
        o22Cc22bRYD9unH8GyK/BLsCgYEAgBe7gDFXFiuIYm/vq4KCOoIX9Z/jr+bmucmd
        tKT8Dns7iLYDBRIWnLo12425mt9LT5YIGWBBRz6Stxk6fgilQfa6s6nW219P+HU4
        AY6xrP5uM8B5B5R/fr1lJePHb7pZem1nfBXk3IE81WIX/7txRvoBDGqk8j1g9yQJ
        A65ML2kCgYEAiZ3TluDemONIsnSMMpI8DeWLyGkyXOQGWJt1KOY5sX/yvR9D86kV
        QUSvysHCeImmaSux5N9Yhr2qz5kJWXLDUm1YlRXYsuAMD3OSBmCeIsaMHF2LlIL/
        IisjFYF1wgy3PiEmrUSwTQVAfFd/65Fd3oDDlcLb8rAf3odggz2AV84=
        -----END RSA PRIVATE KEY-----
        ''
    , githubInstId = 684377
    }
, cfgLogDir = "/home/shaun/devel/mpowered/lodjur/logs"
, cfgDatabase =
    { dbHost = "localhost"
    , dbPort = 5432
    , dbName = "lodjur"
    , dbUser = "lodjur"
    , dbPassword = "lodjur"
    }
}
