# smsaero

[![Hackage](https://img.shields.io/hackage/v/smsaero.svg)](http://hackage.haskell.org/package/smsaero)
[![Build Status](https://travis-ci.org/GetShopTV/smsaero.svg?branch=master)](https://travis-ci.org/GetShopTV/smsaero)

SMSAero API and HTTP client based on servant library.

## Documentation

Library documentation is available [on Hackage](http://hackage.haskell.org/package/smsaero).

The original SMSAero API documentation (in Russian) is available [here](http://smsaero.ru/api/description).

API documentation in English can be generated using [`pandoc`](http://pandoc.org):

```
$ stack exec pandoc --filter=docs/api-filter.hs -o docs/api.md api-intro.md
```

Note that you can generate this documentation in any format that `pandoc` supports (e.g. HTML, LaTeX, Markdown, etc.).

## Usage

Import `SMSAero` and `Control.Monad.Trans.Either` module to interact with SMSAero:

```
>>> :s -XOverloadedStrings
>>> import SMSAero
>>> import Control.Monad.Trans.Either
>>> let credentials = SMSAeroAuth "user@example.com" "md5-password-hash"
>>> runEitherT $ smsAeroBalance credentials
Right (ResponseOK (BalanceResponse 10.0))
```

## Contributing

Contributions and bug reports are welcome!

*GetShopTV Team*
