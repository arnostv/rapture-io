/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2011 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture.io

import scala.collection.mutable._

/** Provides a simple class mixin for creating a list of items from which items can be lookup up.
  *
  * @tparam Index The type of the key by which items are indexed */
trait Lookup[Index] {

  type Item <: AutoAppend

  trait AutoAppend { thisItem : Item =>
    def index : Index
    items(index) = thisItem
  }

  private val items = new HashMap[Index, Item]
  def elements = items.valuesIterator
  def lookup(idx : Index) : Option[Item] = items.get(idx)
}

trait Cell[T] {
  def apply() : T
  def update(t : T) : Unit
}

class Counter {
  private var n = 0
  def apply() = synchronized { n += 1; n }
}
