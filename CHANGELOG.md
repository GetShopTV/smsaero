0.4
---
* Rename `StatusResponse` to `MessageStatus` and add instance Read
* Change `MessageId` representation to `Int64`
* Change `Phone` representation to `Int64`

0.3
---
* Add `MessageBody` newtype for `"text"` query parameter
* Add instances for automatic API documentation via servant-docs
* Add filter for Pandoc to generate documentation in many formats
* Fix some `ToJSON` instances to match `FromJSON`
* Add `ToJSON`/`FromJSON` instances for `SMSAeroAuth`

0.2
---
* Structure haddock documentation
* Add missing `ToJSON` and `FromText` instances

0.1.1
-----
* Add support for GHC 7.8
