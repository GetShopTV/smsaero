0.7.1
---
* Restrict `base` to `4.9.*` (see [#15](https://github.com/GetShopTV/smsaero/issues/15)).

0.7
---
* Add support for `aeson-1`;
* Switch to `servant-0.9`.

0.6.2
---
* Get rid of `SmsAeroJson` (they have finally fixed their `Content-Type`!). See [5a7539e](https://github.com/GetShopTV/smsaero/commit/5a7539edff7f274d39c58ce02f19bcb14bbc05b5).

0.6.1
---
* Fix `SmsAeroBalance` `ToJSON`/`FromJSON` instances.

0.6
---
* Change client function types (see [#11](https://github.com/GetShopTV/smsaero/pull/11)).

0.5
---
* Switch to `servant-0.7` (see [#5](https://github.com/GetShopTV/smsaero/pull/5));
* Update to the latest SMSAero API (see [#7](https://github.com/GetShopTV/smsaero/pull/7) and [#9](https://github.com/GetShopTV/smsaero/pull/9) and [#10](https://github.com/GetShopTV/smsaero/pull/10)):
  * Add `type` parameter;
  * Add groups, contacts and blacklist API;
  * Add API for tarif and message status checking.

0.4.1
---
* Add `Eq` instances

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
