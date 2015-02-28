package frdomain.ch5

import scalaz.Free

package object service {
  type AccountAction[A] = Free[AccountActionF, A]
}

