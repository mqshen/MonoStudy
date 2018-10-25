package mono.crypto

import java.math.BigInteger

import akka.util.ByteString
import mono.util.{BlockchainConfig, Config}
import org.spongycastle.asn1.x9.X9IntegerConverter
import org.spongycastle.math.ec.{ECAlgorithms, ECCurve, ECPoint}

object ECDSASignature {
  val NegativePointSign: Byte = 27
  val PositivePointSign: Byte = 28
  val EIP155NegativePointSign: Byte = 35
  val EIP155PositivePointSign: Byte = 36

  val allowedPointSigns = Set(NegativePointSign, PositivePointSign)

  // TODO, ETH return Int, ETC should return Byte (for example of chainId 61, the v (2*61+35) is int 157 but byte -99)
  //def getEncodeV(v: Byte, chainId: Option[Int]): Int =
  def getEncodeV(v: Byte, chainId: Option[Int]): Either[Byte, Int] =
    if (Config.isEth) Right(getEncodeV_eth(v, chainId)) else Left(getEncodeV_etc(v, chainId))


  // --- choose eth or etc
  def extractChainIdFromV(v: Int): Option[Int] =
    if (Config.isEth) extractChainIdFromV_eth(v) else extractChainIdFromV_etc(v)

  def getRealV(v: Int): Byte =
    if (Config.isEth) getRealV_eth(v) else getRealV_etc(v)

  def recoverPublicKey(sig: ECDSASignature, message: ByteString): Option[ByteString] = recoverPublicKey(sig, message.toArray, None).map(ByteString(_))
  /**
    * returns ECC point encoded with on compression and without leading byte indicating compression
    * @param message message to be signed
    * @param chainId optional value if you want new signing schema with recovery id calculated with chain id
    * @return
    */
  def recoverPublicKey(sig: ECDSASignature, message: Array[Byte], chainId: Option[Int] = None): Option[Array[Byte]] =
    if (Config.isEth) recoverPubBytes_eth(sig.r, sig.s, sig.v, message, chainId) else recoverPubBytes_etc(sig.r, sig.s, sig.v, message, chainId)

  private def extractChainIdFromV_etc(v: Int): Option[Int] = {
    Some(BlockchainConfig(Config.config).chainId.toByte)
  }

  private def getEncodeV_etc(v: Byte, chainId: Option[Int]): Byte = {
    v
  }

  private def getEncodeV_eth(v: Byte, chainId: Option[Int]): Int = {
    chainId match {
      case Some(id) =>
        // v - NegativePointSign ------------ 0 or 1
        // + id * 2 + NewNegativePointSign
        v match {
          case NegativePointSign => id * 2 + EIP155NegativePointSign
          case PositivePointSign => id * 2 + EIP155PositivePointSign
          case _                 => id * 2 + EIP155NegativePointSign + (v - NegativePointSign)
        }
      case None => v
    }
  }

  private def getRealV_etc(v: Int): Byte = {
    v.toByte
  }

  /**
    * @return 27 or 28
    */
  private def getRealV_eth(v: Int): Byte = {
    //if (v.bitLength() > 31) return 0; // chainId is limited to 31 bits, longer are not valid for now
    v match {
      case NegativePointSign | PositivePointSign => v.toByte
      case _                                     => if (v % 2 == 0) PositivePointSign else NegativePointSign
    }
  }

  /**
    * <p>Given the components of a signature and a selector value, recover and return the public key
    * that generated the signature according to the algorithm in SEC1v2 section 4.1.6.</p>
    *
    * <p>The recoverId is an index from 0 to 3 which indicates which of the 4 possible keys is the correct one. Because
    * the key recovery operation yields multiple potential keys, the correct key must either be stored alongside the
    * signature, or you must be willing to try each recId in turn until you find one that outputs the key you are
    * expecting.</p>
    *
    * <p>If this method returns null it means recovery was not possible and recId should be iterated.</p>
    *
    * <p>Given the above two points, a correct usage of this method is inside a for loop from 0 to 3, and if the
    * output is null OR a key that is not the one you expect, you try again with the next recId.</p>
    *
    * @param s the R and S components of the signature, wrapped.
    * @param r Which possible key to recover.
    * @param chainId
    * @param messageHash Hash of the data that was signed.
    * @return 65-byte encoded public key
    */
  private def recoverPubBytes_etc(r: BigInteger, s: BigInteger, v: Byte, messageHash: Array[Byte], chainId: Option[Int]): Option[Array[Byte]] = {
    getRecoveredPointSign_etc(v, chainId) flatMap { recoverPointSign =>
      val order = curve.getCurve.getOrder
      // ignore case when x = r + order because it is negligibly improbable
      // says: https://github.com/paritytech/rust-secp256k1/blob/f998f9a8c18227af200f0f7fdadf8a6560d391ff/depend/secp256k1/src/ecdsa_impl.h#L282
      val xCoordinate = r
      val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
      val prime = curveFp.getQ

      if (xCoordinate.compareTo(prime) < 0) {
        val R = constructPoint(xCoordinate, recoverPointSign == PositivePointSign)
        if (R.multiply(order).isInfinity) {
          val e = new BigInteger(1, messageHash)
          val rInv = r.modInverse(order)
          // Q = r^(-1)(sR - eG)
          val q = R.multiply(s).subtract(curve.getG.multiply(e)).multiply(rInv)
          // byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
          Some(q.getEncoded(false).tail)
        } else None
      } else None
    }
  }

  private def recoverPubBytes_eth(r: BigInteger, s: BigInteger, v: Int, message: Array[Byte], chainId: Option[Int]): Option[Array[Byte]] = {
    getRecoverIdx_eth(v) flatMap { recoverIdx =>
      recoverPubBytesForRecoverId_eth(r, s, recoverIdx, message)
    }
  }

  /**
    * Since EIP-155, we could encode chainId in V
    */
  private def extractChainIdFromV_eth(v: Int): Option[Int] = {
    //if (v.bitLength() > 31) return Integer.MAX_VALUE; // chainId is limited to 31 bits, longer are not valid for now
    v match {
      case NegativePointSign | PositivePointSign => None
      case _                                     => Some((v - EIP155NegativePointSign) / 2)
    }
  }

  /**
    * <p>Given the components of a signature and a selector value, recover and return the public key
    * that generated the signature according to the algorithm in SEC1v2 section 4.1.6.</p>
    *
    * <p>The recId is an index from 0 to 3 which indicates which of the 4 possible keys is the correct one. Because
    * the key recovery operation yields multiple potential keys, the correct key must either be stored alongside the
    * signature, or you must be willing to try each recId in turn until you find one that outputs the key you are
    * expecting.</p>
    *
    * <p>If this method returns null it means recovery was not possible and recId should be iterated.</p>
    *
    * <p>Given the above two points, a correct usage of this method is inside a for loop from 0 to 3, and if the
    * output is null OR a key that is not the one you expect, you try again with the next recId.</p>
    *
    * @param r Which possible key to recover.
    * @param s the R and S components of the signature, wrapped.
    * @param messageHash Hash of the data that was signed.
    * @return 65-byte encoded public key
    */
  private def recoverPubBytesForRecoverId_eth(r: BigInteger, s: BigInteger, recoverIdx: Int, messageHash: Array[Byte]): Option[Array[Byte]] = {
    //check(recId >= 0, "recId must be positive");
    //check(sig.r.signum() >= 0, "r must be positive");
    //check(sig.s.signum() >= 0, "s must be positive");
    //check(messageHash != null, "messageHash must not be null");
    // 1.0 For j from 0 to h   (h == recIdx here and the loop is outside this function)
    //   1.1 Let x = r + jn
    val n = curve.getN // Curve order.
    val i = BigInteger.valueOf(recoverIdx / 2)
    val x = r.add(i.multiply(n))
    //   1.2. Convert the integer x to an octet string X of length mlen using the conversion routine
    //        specified in Section 2.3.7, where mlen = ⌈(log2 p)/8⌉ or mlen = ⌈m/8⌉.
    //   1.3. Convert the octet string (16 set binary digits)||X to an elliptic curve point R using the
    //        conversion routine specified in Section 2.3.4. If this conversion routine outputs “invalid”, then
    //        do another iteration of Step 1.
    //
    // More concisely, what these points mean is to use X as a compressed public key.
    val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
    val prime = curveFp.getQ // Bouncy Castle is not consistent about the letter it uses for the prime.
    if (x.compareTo(prime) < 0) {
      // Compressed keys require you to know an extra bit of data about the y-coord as there are two possibilities.
      // So it's encoded in the recId.
      val R = constructPoint(x, (recoverIdx & 1) == 1)
      //   1.4. If nR != point at infinity, then do another iteration of Step 1 (callers responsibility).
      if (R.multiply(n).isInfinity) {
        //   1.5. Compute e from M using Steps 2 and 3 of ECDSA signature verification.
        val e = new BigInteger(1, messageHash)
        //   1.6. For k from 1 to 2 do the following.   (loop is outside this function via iterating recId)
        //   1.6.1. Compute a candidate public key as:
        //               Q = mi(r) * (sR - eG)
        //
        // Where mi(x) is the modular multiplicative inverse. We transform this into the following:
        //               Q = (mi(r) * s ** R) + (mi(r) * -e ** G)
        // Where -e is the modular additive inverse of e, that is z such that z + e = 0 (mod n). In the above equation
        // ** is point multiplication and + is point addition (the EC group operator).
        //
        // We can find the additive inverse by subtracting e from zero then taking the mod. For example the additive
        // inverse of 3 modulo 11 is 8 because 3 + 8 mod 11 = 0, and -3 mod 11 = 8.
        val eInv = BigInteger.ZERO.subtract(e).mod(n)
        val rInv = r.modInverse(n)
        val srInv = rInv.multiply(s).mod(n)
        val eInvrInv = rInv.multiply(eInv).mod(n)
        val q = ECAlgorithms.sumOfTwoMultiplies(curve.getG, eInvrInv, R, srInv).asInstanceOf[ECPoint.Fp]
        // byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
        Some(q.getEncoded(false).tail)
      } else {
        None
      }
    } else {
      // Cannot have point co-ordinates larger than this as everything takes place modulo Q.
      None
    }
  }

  /**
    * New formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  private def getRecoveredPointSign_etc(pointSign: Byte, chainId: Option[Int]): Option[Byte] = {
    (chainId match {
      case Some(id) =>
        if (pointSign == NegativePointSign || pointSign == (id * 2 + EIP155NegativePointSign).toByte) {
          Some(NegativePointSign)
        } else if (pointSign == PositivePointSign || pointSign == (id * 2 + EIP155PositivePointSign).toByte) {
          Some(PositivePointSign)
        } else {
          None
        }
      case None => Some(pointSign)
    }).filter(allowedPointSigns.contains)
  }

  private def getRecoverIdx_eth(v: Int): Option[Int] = {
    // The header byte: 0x1B = 1st key with even y, 0x1C = 1st key with odd y,
    //                  0x1D = 2nd key with even y, 0x1E = 2nd key with odd y
    if (v >= 27 && v <= 34) { // 27,28,29,30,31,32,33,34
      if (v >= 31) { // 31,32,33,34
        Some(v - 31) // 0,1,2,3
      } else { // 27,28,29,30
        Some(v - 27) // 0,1,2,3
      }
    } else {
      None // "Header byte out of range: "
    }
  }

  /**
    * Decompress a compressed public key (x co-ord and low-bit of y-coord).
    *
    * @param xBN -
    * @param yBit_PositivePointSign -
    * @return -
    */
  private def constructPoint(xBN: BigInteger, yBit_PositivePointSign: Boolean): ECPoint = {
    val x9 = new X9IntegerConverter()
    val compEnc = x9.integerToBytes(xBN, 1 + x9.getByteLength(curve.getCurve))
    compEnc(0) = if (yBit_PositivePointSign) 0x03 else 0x02
    curve.getCurve.decodePoint(compEnc)
  }

}
final case class ECDSASignature(r: BigInteger, s: BigInteger, v: Byte)
