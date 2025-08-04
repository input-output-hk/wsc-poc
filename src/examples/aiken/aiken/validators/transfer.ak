use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use cardano/assets.{PolicyId}

validator placeholder {
  withdraw(_redeemer: Data, _account: Credential, _self: Transaction) {
    todo @"withdraw logic goes here"
  }
  mint(_redeemer: Data, _policy_id: PolicyId, _self: Transaction) {
    todo @"mint logic goes here"
  }
}
