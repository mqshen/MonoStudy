package mono

import akka.util.ByteString
import mono.crypto.hash.Keccak256
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.asn1.x9.X9ECParameters
import org.spongycastle.crypto.params.ECDomainParameters

package object crypto {
  val curveParams: X9ECParameters = SECNamedCurves.getByName("secp256k1")
  val curve: ECDomainParameters = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  def kec256(input: Array[Byte]*): Array[Byte] = {
    val digest: Keccak256 = new Keccak256()
    input.foreach(i => digest.update(i))
    digest.digest
  }

  def kec256(input: ByteString): Array[Byte] = kec256(input.toArray)
}
