/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture.io

object Generator {

  def nonNull[T](f : => T) = non[T](null.asInstanceOf[T])(f)

  def non[T](halt : T)(f : => T) = new Iterator[T] {
    private var n : T = _
    private var h = false
    
    def hasNext = {
      if(!h) {
        n = f
        h = true
      }
      n != halt
    }

    def next = {
      if(!h) n = f
      h = false
      n
    }
  }

}