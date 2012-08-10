/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture.io

import java.security._

abstract class Digester {
  
  def digest(msg: Array[Byte]): Array[Byte]

  /** Digests the UTF-8 representation of the given string. */
  def digest(msg: String): Array[Byte] = digest(msg.getBytes("UTF-8"))

  /** Digests the UTF-8 representation of the given string, and returns the
   * result in hexadecimal form. */
  def digestHex(msg: String): String = digestHex(msg.getBytes("UTF-8"))

  /** Digests the given bytes, and returns the result in hexadecimal form. */
  def digestHex(msg: Array[Byte]): String = {
    
    val bytes = digest(msg)
    val out = new Array[Char](bytes.length * 2)

    var i = 0
    val len = bytes.length
    
    while(i < len) {
      
      val i2 = i << 1
      
      out(i2) = ((bytes(i) & 0xF0) >>> 4).toHexString.toUpperCase.head
      out(i2 + 1) = (bytes(i) & 0x0F).toHexString.toUpperCase.head
      
      i += 1
    }

    new String(out)
  }

  /** Digests the UTF-8 representation of the given string, and returns the
    * result base-64 encoded. Note that this is not strictly RFC2045 compliant
    * as the result is not padded. Append "==" to comply. */
  def digestBase64(msg: String): String = digestBase64(msg.getBytes("UTF-8"))

  /** Digests the given bytes, and returns the result base-64 encoded. Note
    * that this is not strictly RFC2045 compliant as the result is not padded.
    * Append "==" to comply. */
  def digestBase64(msg: Array[Byte]): String =
    new String(Base64.encode(digest(msg), false, false))
}

/** SHA-256 digester, with additional methods for secure password encoding. */
object Sha256 extends Digester {
  
  private val random = new SecureRandom

  /** Digests the given bytes. */
  def digest(msg: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-256")
    md.digest(msg)
  }

  /** Applies the hash function after combining the supplied key with a
    * random 64-bit salt, and returns the result base-64 encoded. */
  def makePassword(key: Array[Char]): String = {
    val salt = new Array[Byte](8)
    synchronized { random.nextBytes(salt) }
    buildPass(key, salt)
  }

  /** Checks that the given key matches the salted hash. */
  def checkPassword(key: Array[Char], hash: String): Boolean = {
    val salt = Base64.decode(hash.toCharArray)
    val newCode = buildPass(key, salt)
    hash == newCode
  }

  private def buildPass(key: Array[Char], salt: Array[Byte]): String = {
    
    val md = MessageDigest.getInstance("SHA-256")
    md.update(salt, 0, 8)
    
    val kLen = key.length
    val keyBytes = new Array[Byte](kLen << 1)
    var i = 0
    
    while(i < kLen) {
      val i2 = i << 1
      keyBytes(i2) = (key(i) >>> 8).asInstanceOf[Byte]
      keyBytes(i2 + 1) = key(i).asInstanceOf[Byte]
      i = i + 1
    }
    
    val digest = md.digest(keyBytes)
    
    java.util.Arrays.fill(keyBytes, 0.toByte) // Don't leave sensitive data lying around
    
    val code = new Array[Byte](digest.length + 8)
    
    Array.copy(salt, 0, code, 0, 8)
    Array.copy(digest, 0, code, 8, digest.length)
    
    new String(Base64.encode(code, false, false))
  }
}

/** MD5 Digester. This is included for backwards compatibility. MD5 is no
  * longer considered future-proof and new designs should prefer SHA-256. */
object Md5 extends Digester {
  
  /** Digests the given bytes. */
  def digest(msg: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("MD5")
    md.digest(msg)
  }
}
