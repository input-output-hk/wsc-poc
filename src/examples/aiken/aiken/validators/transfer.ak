use cardano/address.{Credential}
use cardano/transaction.{Transaction}

validator issue {
  withdraw(redeemer: Int, _account: Credential, _self: Transaction) {
    trace @"withdraw"
    redeemer == 100
  }
  else(_) {
    trace @"fallback"
    False
  }
}

validator transfer {
  withdraw(redeemer: Int, _account: Credential, _self: Transaction) {
    trace @"withdraw"
    redeemer == 200
  }
  else(_) {
    trace @"fallback"
    False
  }
}
