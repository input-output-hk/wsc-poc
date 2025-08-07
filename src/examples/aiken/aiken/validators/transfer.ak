use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use cardano/assets.{PolicyId}

validator placeholder {
  withdraw(_redeemer: Data, _account: Credential, _self: Transaction) {
    trace @"withdraw"
    True
  }
  mint(_redeemer: Data, _policy_id: PolicyId, _self: Transaction) {
    trace @"mint"
    True
  }
  else(_) {
    trace @"fallback"
    True
  }
}
