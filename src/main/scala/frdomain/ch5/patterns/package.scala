package frdomain.ch5

import scalaz._
import Scalaz._

package object patterns {
  type AccountRepo[A] = Free[AccountRepoF, A]
  type AccountRepoC[A] = Free.FreeC[AccountRepoF, A]
}
