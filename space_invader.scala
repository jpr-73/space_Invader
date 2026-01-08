import Global.display
import hevs.graphics.FunGraphics

import java.awt.Color
import java.awt.event._
import javax.swing.Timer
import scala.collection.mutable.ArrayBuffer

object Global {
  var display: FunGraphics = new FunGraphics(1920, 1080)
}

class Spaceship(var x: Int, var y: Int, var size: Int, var c: Color){

  private var up = false
  private var down = false
  private var left = false
  private var right = false
  var shot = false
  var speed = 3
  var lastDx = 1
  var lastDy = 0



  def computeDirection(): (Int, Int) = {
    val dx =
      (if (right) 1 else 0) +
        (if (left) -1 else 0)

    val dy =
      (if (down) 1 else 0) +
        (if (up) -1 else 0)

    if (dx != 0 || dy != 0) {
      lastDx = dx
      lastDy = dy
    }

    (dx, dy)
  }

  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }


  display.setKeyManager(new KeyAdapter() {
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_LEFT | KeyEvent.VK_A => left = true
        case KeyEvent.VK_RIGHT | KeyEvent.VK_D => right = true
        case KeyEvent.VK_UP | KeyEvent.VK_W => up = true
        case KeyEvent.VK_DOWN | KeyEvent.VK_S => down = true
        case KeyEvent.VK_E => shot = true
        case KeyEvent.VK_ESCAPE => sys.exit()
        case _ =>
      }
    }

    override def keyReleased(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_LEFT | KeyEvent.VK_A => left = false
        case KeyEvent.VK_RIGHT | KeyEvent.VK_D => right = false
        case KeyEvent.VK_UP | KeyEvent.VK_W => up = false
        case KeyEvent.VK_DOWN | KeyEvent.VK_S => down = false
        case KeyEvent.VK_E => shot = false
        case _ =>
      }
    }
  })
}

class Invader(var x: Int, var y:Int, var size: Int, var c: Color){

}


class Projectile(var x: Int, var y: Int, var size: Int, var speed: Int, var dx: Int, var dy: Int) {
  def update(): Unit = {
    x += dx * speed
    y += dy * speed
  }

  def draw(): Unit = {
    display.setColor(Color.RED)
    display.drawFillRect(x, y, size, size)
  }

  def isOutOfBounds(width: Int, height: Int): Boolean = {
    x < 0 || x > width || y < 0 || y > height
  }
}


class Game1{
  private var grid : Array[Array[Int]] = Array.fill(1980, 1080)(0)
  private var numLives = 3

  private var score = 0

  var highscore = 0



  val s: Spaceship = new Spaceship(
    960,
    540,
    30,
    Color.CYAN
  )

  private var prevX = s.x
  private var prevY = s.y

  private val projectiles: ArrayBuffer[Projectile] = ArrayBuffer()


  private var shotCooldown = 0
  private val SHOT_COOLDOWN_FRAMES = 15

  private var firstDraw = true

  private val timer = new Timer(16, (_: ActionEvent)=>{
    if(firstDraw){
      for(row <- grid.indices; col <- grid(row).indices){
        Global.display.setColor(Color.BLACK)
      }
      firstDraw = false
    }

    val (dx, dy) = s.computeDirection()
    s.x = Math.max(0, Math.min(1920 - s.size, s.x + dx * s.speed))
    s.y = Math.max(0, Math.min(1080 - s.size, s.y + dy * s.speed))


    if(shotCooldown > 0){
      shotCooldown -= 1
    }


    if(s.shot && shotCooldown <= 0){
      val projX = s.x + s.size / 2
      val projY = s.y + s.size / 2

      val dx = 0
      val dy = -1

      projectiles += new Projectile(projX, projY, 8, 8, dx, dy)
      shotCooldown = SHOT_COOLDOWN_FRAMES
    }


    if(!s.shot){
      shotCooldown = 0
    }

    projectiles.foreach(_.update())
    projectiles.filterInPlace(!_.isOutOfBounds(1920, 1080))

    display.setColor(Color.BLACK)
    display.drawFillRect(0, 0, 1920, 1080)

    s.draw()
    projectiles.foreach(_.draw())

    display.setColor(Color.WHITE)
    display.drawString(20, 30, s"Score : $score")

  })
  timer.start()

}

object Spacegame extends App{
  new Game1()
}