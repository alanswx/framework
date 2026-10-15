package lib.video

import chisel3._
import chisel3.util._

/** Changes orientation only between output frames. Input must already be synchronized. */
class FrameRotationLatch extends Module {
  val io = IO(new Bundle {
    val requested = Input(UInt(2.W))
    val boundary = Input(Bool())
    val active = Output(UInt(2.W))
  })
  val active = RegInit(0.U(2.W))
  when (io.boundary) { active := io.requested }
  io.active := active
}

/** Read-address transform for an existing row-major framebuffer. No image storage.
  * Quarter turns fit the screen with a rational scale; 0/180 preserve integer scaling.
  * Read lookahead is measured along the output's physical scan axis, before rotation.
  */
class FramebufferTransform(
  width: Int, height: Int, screenWidth: Int, screenHeight: Int,
  offsetX: Int = 0, delayX: Int = 0, delayY: Int = 0,
  xBits: Int = 10, yBits: Int = 10,
) extends Module {
  require(width > 0 && height > 0 && screenWidth > 0 && screenHeight > 0)
  val io = IO(new Bundle {
    val x = Input(UInt(xBits.W))
    val y = Input(UInt(yBits.W))
    val rotation = Input(UInt(2.W))
    val address = Output(UInt(log2Ceil(width * height).W))
    val inBounds = Output(Bool())
  })

  def translate(value: UInt, origin: Int): UInt =
    if (origin >= 0) value - origin.U else value + (-origin).U

  def inRange(value: UInt, start: Int, count: Int): Bool =
    (if (start > 0) value >= start.U else true.B) && value < (start + count).U

  val quarter = io.rotation(0)
  val normalScale = (screenWidth / width).min(screenHeight / height).max(1)
  val normalWidth = width * normalScale
  val normalHeight = height * normalScale
  val fitWidthLimited = screenWidth.toLong * width <= screenHeight.toLong * height
  val portraitWidth = if (fitWidthLimited) screenWidth else height * screenHeight / width
  val portraitHeight = if (fitWidthLimited) width * screenWidth / height else screenHeight
  val normalX = (screenWidth - normalWidth) / 2 + offsetX
  val normalY = (screenHeight - normalHeight) / 2
  val portraitX = (screenWidth - portraitWidth) / 2 + offsetX
  val portraitY = (screenHeight - portraitHeight) / 2

  // Preserve the scan driver's modular coordinate width during lookahead.
  // Blanking before coordinate zero is encoded as unsigned wraparound.
  val lookX = Wire(UInt(xBits.W))
  val lookY = Wire(UInt(yBits.W))
  lookX := io.x + delayX.U
  lookY := io.y + delayY.U

  // Use the rounded output dimensions in both centering and inverse scaling.
  val nx = translate(lookX, normalX) / normalScale.U
  val ny = translate(lookY, normalY) / normalScale.U
  def rescale(value: UInt, source: Int, output: Int): UInt = {
    val common = BigInt(source).gcd(BigInt(output)).toInt
    (value * (source / common).U) / (output / common).U
  }
  val qx = rescale(translate(lookX, portraitX), height, portraitWidth)
  val qy = rescale(translate(lookY, portraitY), width, portraitHeight)
  val sx = Wire(UInt(log2Ceil(width).W))
  val sy = Wire(UInt(log2Ceil(height).W))
  sx := nx
  sy := ny
  switch (io.rotation) {
    is (1.U) { sx := qy; sy := (height - 1).U - qx }
    is (2.U) { sx := (width - 1).U - nx; sy := (height - 1).U - ny }
    is (3.U) { sx := (width - 1).U - qy; sy := qx }
  }
  io.address := sy * width.U + sx
  io.inBounds := Mux(quarter,
    inRange(io.x, portraitX, portraitWidth) && inRange(io.y, portraitY, portraitHeight),
    inRange(io.x, normalX, normalWidth) && inRange(io.y, normalY, normalHeight))
}
