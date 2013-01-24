/**************************************************************************************************
Rapture I/O Library
Version 0.7.2

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2013 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture

trait Mail { this: Io =>

  object Mailto extends Scheme[MailtoUri] {
    def schemeName = "mailto"
    def /(email: String): MailtoUri = new MailtoUri(email)
  }

  class MailtoUri(val email: String) extends Uri {
    def absolute = true
    def scheme = Mailto
    def schemeName = scheme.schemeName
    def schemeSpecificPart = email
  }

}
