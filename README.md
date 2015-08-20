# smsaero

SMSAero API and HTTP client based on servant library.

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
