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

package rapture

import java.net._

package object io extends Io {

  type JavaFile = java.io.File

  implicit def urlCodec(s : String) = new {
    def urlEncode(implicit encoding : Encodings.Encoding = Encodings.`UTF-8`) =
      URLEncoder.encode(s, encoding.name)
    def urlDecode(implicit encoding : Encodings.Encoding = Encodings.`UTF-8`) =
      URLDecoder.decode(s, encoding.name)
  }

}
