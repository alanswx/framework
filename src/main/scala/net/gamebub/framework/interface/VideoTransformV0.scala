package net.gamebub.framework.interface

import chisel3._

/** Optional game-layer transform. The board's panel mounting and menu are independent.
  * Rotation is clockwise: 0 = none, 1 = 90, 2 = 180, 3 = 270 degrees.
  * The framework synchronizes requests and applies them during output blanking.
  */
class VideoTransformV0 extends Bundle {
  val rotation = Output(UInt(2.W))
}
