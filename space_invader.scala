import Global.display
import hevs.graphics.FunGraphics

import java.awt._
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
  private var lastDx = 1
  private var lastDy = 0

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

class Invader(var x: Int, var y: Int, val offsetX: Int, val offsetY: Int, var size: Int, var c: Color) {
  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }
}

class Projectile(var x: Int, var y: Int, var size: Int, var speed: Int, var dx: Int, var dy: Int, c: Color) {
  def update(): Unit = {
    x += dx * speed
    y += dy * speed
  }

  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }

  def isOutOfBounds(width: Int, height: Int): Boolean = {
    x < 0 || x > width || y < 0 || y > height
  }
}

class Game1 {

  private val grid: Array[Array[Int]] = Array.fill(1980, 1080)(0)

  private var numLives = 3
  private var score = 0
  private var gameStarted = false
  private var end = false

  private var invaderDir = 1
  private val invaderSpeed = 3
  private val RESPAWN_DELAY_MS = 2500

  // Formation base position
  private var formationX = 0
  private val formationY = 0

  // Dead invaders store offsets, not absolute positions
  private val deadInvaders: ArrayBuffer[(Int, Int, Int, Color, Long)] = ArrayBuffer()

  private val ship_proj: ArrayBuffer[Projectile] = ArrayBuffer()
  private val inv_proj: ArrayBuffer[Projectile] = ArrayBuffer()

  private var shotCooldown = 0
  private val SHOT_CD_FRAMES = 25
  private val invaderShotPeriod = 60
  private val invaderShotOffset = 20

  private var frameCount = 0
  private var firstDraw = true
  private val scoreFont = new Font("Lithograph", Font.BOLD, 36)

  private val buttonX = 810
  private val buttonY = 500
  private val buttonWidth = 300
  private val buttonHeight = 80

  private val starX = Array.fill(300)(scala.util.Random.nextInt(1920))
  private val starY = Array.fill(300)(scala.util.Random.nextInt(1080))
  private val starSize = Array.fill(300)(scala.util.Random.nextInt(3) + 1)
  private val starType = Array.fill(300)(scala.util.Random.nextInt(2)) // 0 = dot, 1 = cross
  private val starColor = Array.fill(300)(
    if (scala.util.Random.nextBoolean())
      new Color(20, 20, 80)
    else
      Color.YELLOW
  )


  private val s: Spaceship = new Spaceship(
    960,
    540,
    30,
    Color.CYAN
  )

  private val i: ArrayBuffer[Invader] = ArrayBuffer(


    new Invader(0, 0, 165, 180, 60, Color.RED),
    new Invader(0, 0, 515, 180, 60, Color.RED),
    new Invader(0, 0, 815, 180, 60, Color.RED),

    new Invader(0, 0, 80, 260, 60, Color.RED),
    new Invader(0, 0, 255, 260, 60, Color.RED),
    new Invader(0, 0, 595, 260, 60, Color.RED),
    new Invader(0, 0, 445, 260, 60, Color.RED),
    new Invader(0, 0, 735, 260, 60, Color.RED),
    new Invader(0, 0, 905, 260, 60, Color.RED)
  )


  private def drawHeart(x: Int, y: Int, size: Int, c: Color): Unit = {
    display.setColor(c)
    val half = size / 2


    display.drawFilledOval(x, y, half, half)
    display.drawFilledOval(x + half, y, half, half)


    val triangle = new Polygon()
    triangle.addPoint(x, y + half)
    triangle.addPoint(x + size, y + half)
    triangle.addPoint(x + half, y + size)

    display.drawFilledPolygon(triangle, c)
  }

  private def drawBackgroundStars(): Unit = {
    for (i <- starX.indices) {
      display.setColor(starColor(i))
      val x = starX(i)
      val y = starY(i)
      val s = starSize(i)

      if (starType(i) == 0) {
        display.drawFillRect(x, y, s, s)
      } else {
        display.drawFillRect(x - s, y, s * 2, 1)
        display.drawFillRect(x, y - s, 1, s * 2)
      }
    }
  }




  private def drawStartScreen(): Unit = {
    display.setColor(Color.BLACK)
    display.drawFillRect(0, 0, 1920, 1080)

    display.setColor(Color.WHITE)
    val titleFont = new Font("Lithograph", Font.BOLD, 72)
    display.drawString(650, 300, "SPACE INVADERS", titleFont, Color.WHITE)

    display.setColor(Color.GREEN)
    display.drawFillRect(buttonX, buttonY, buttonWidth, buttonHeight)

    val buttonFont = new Font("Lithograph", Font.BOLD, 36)
    display.drawString(buttonX + 80, buttonY + 50, "START", buttonFont, Color.BLACK)

    val instructFont = new Font("Arial", Font.PLAIN, 24)
    display.drawString(800, 700, "WASD or Arrow Keys to Move", instructFont, Color.WHITE)
    display.drawString(900, 750, "E to Shoot", instructFont, Color.WHITE)
  }

  private def checkStartButtonClick(mouseX: Int, mouseY: Int): Unit = {
    if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth &&
      mouseY >= buttonY && mouseY <= buttonY + buttonHeight) {
      gameStarted = true
    }
  }

  private def resetGame():Unit ={
    numLives = 3
    score = 0
    formationX = 0
    end = false
    gameStarted = true
    deadInvaders.clear()
    ship_proj.clear()
    inv_proj.clear()
    i.clear()

    i ++= ArrayBuffer(
      new Invader(0, 0, 165, 180, 60, Color.RED),
      new Invader(0, 0, 515, 180, 60, Color.RED),
      new Invader(0, 0, 815, 180, 60, Color.RED),
      new Invader(0, 0, 80, 260, 60, Color.RED),
      new Invader(0, 0, 255, 260, 60, Color.RED),
      new Invader(0, 0, 595, 260, 60, Color.RED),
      new Invader(0, 0, 445, 260, 60, Color.RED),
      new Invader(0, 0, 735, 260, 60, Color.RED),
      new Invader(0, 0, 905, 260, 60, Color.RED)
    )
    timer.start()
  }

  display.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (!gameStarted) {
        checkStartButtonClick(e.getX, e.getY)
      }
    }
  })

  private def respawnInvaders(): Unit = {
    val currentTime = System.currentTimeMillis()
    val toRemove = ArrayBuffer[Int]()

    for (idx <- deadInvaders.indices) {
      val (offsetX, offsetY, size, color, deathTime) = deadInvaders(idx)
      if (currentTime - deathTime >= RESPAWN_DELAY_MS) {
        // Respawn with offsets relative to formation
        i += new Invader(0, 0, offsetX, offsetY, size, color)
        toRemove += idx
      }
    }

    toRemove.reverse.foreach(deadInvaders.remove)
  }

  private def checkGameOver():Unit ={
    if(numLives == 0 && !end){
      end = true
      timer.stop()

      display.setColor(Color.black)
      display.drawFillRect(0, 0, 1920, 1080)

      val gameOverFont = new Font("Lithograph", Font.BOLD, 96)
      display.drawString(650, 400, "GAME OVER", gameOverFont, Color.RED)
      display.drawString(840, 650, s"Final Score: $score", scoreFont, Color.WHITE)

      display.setColor(Color.BLUE)
      display.drawFillRect(buttonX,buttonY,buttonWidth,buttonHeight)
      val btnFont = new Font("Lithograph", Font.BOLD, 36)
      display.drawString(buttonX + 60, buttonY+50, "RESTART", btnFont, Color.BLACK)

    }
  }

  private def checkRestartBtn(mouseX: Int, mouseY: Int): Unit = {
    if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth &&
      mouseY >= buttonY && mouseY <= buttonY + buttonHeight) {
      resetGame()
    }
  }

  display.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent): Unit = {
      if(!gameStarted){
        checkStartButtonClick(e.getX, e.getY)
      } else if(end){
        checkRestartBtn(e.getX, e.getY)
      }
    }
  })


  private val timer = new Timer(16, (_: ActionEvent) => {
    if (!gameStarted) {
      drawStartScreen()
    } else if (!end) {
      // --- existing game update logic ---
      frameCount += 1

      val (dx, dy) = s.computeDirection()
      s.x = Math.max(0, Math.min(1920 - s.size, s.x + dx * s.speed))
      s.y = Math.max(0, Math.min(1080 - s.size, s.y + dy * s.speed))

      formationX += invaderDir * invaderSpeed
      val leftEdge = i.map(_.offsetX + formationX).min
      val rightEdge = i.map(iv => iv.offsetX + formationX + iv.size).max
      if (rightEdge >= 1920) invaderDir = -1
      if (leftEdge <= 0) invaderDir = 1

      i.foreach(iv => {
        iv.x = formationX + iv.offsetX
        iv.y = formationY + iv.offsetY
      })

      // projectiles, collisions, respawn...
      if (shotCooldown > 0) shotCooldown -= 1
      if (s.shot && shotCooldown == 0) {
        ship_proj += new Projectile(
          s.x + s.size / 2,
          s.y,
          8,
          8,
          0,
          -1,
          Color.BLUE
        )
        shotCooldown = SHOT_CD_FRAMES
      }
      if (!s.shot) shotCooldown = 0

      for ((invader, idx) <- i.zipWithIndex)
        if ((frameCount + idx * invaderShotOffset) % invaderShotPeriod == 0)
          inv_proj += new Projectile(
            invader.x + invader.size/2,
            invader.y + invader.size,
            9,
            6,
            0,
            1,
            Color.GREEN
          )

      val invadersToRemove = ArrayBuffer[Int]()
      val projectilesToRemove = ArrayBuffer[Int]()
      for ((proj, pIdx) <- ship_proj.zipWithIndex)
        for ((invader, iIdx) <- i.zipWithIndex)
          if (proj.x < invader.x + invader.size && proj.x + proj.size > invader.x &&
            proj.y < invader.y + invader.size && proj.y + proj.size > invader.y) {
            deadInvaders += ((invader.offsetX, invader.offsetY, invader.size, invader.c, System.currentTimeMillis()))
            invadersToRemove += iIdx
            projectilesToRemove += pIdx
          }

      score += invadersToRemove.distinct.size
      invadersToRemove.distinct.sorted.reverse.foreach(i.remove)
      projectilesToRemove.distinct.sorted.reverse.foreach(ship_proj.remove)

      val invProjToRM = ArrayBuffer[Int]()
      for ((proj, pIdx) <- inv_proj.zipWithIndex)
        if (proj.x < s.x + s.size && proj.x + proj.size > s.x &&
          proj.y < s.y + s.size && proj.y + proj.size > s.y) {
          numLives -= 1
          invProjToRM += pIdx
        }
      invProjToRM.distinct.sorted.reverse.foreach(inv_proj.remove)

      checkGameOver()
      if (!end) {
        respawnInvaders()
        ship_proj.foreach(_.update())
        ship_proj.filterInPlace(!_.isOutOfBounds(1920,1080))
        inv_proj.foreach(_.update())
        inv_proj.filterInPlace(!_.isOutOfBounds(1920,1080))

        display.setColor(Color.BLACK)
        display.drawFillRect(0, 0, 1920, 1080)
        drawBackgroundStars()

        s.draw()
        ship_proj.foreach(_.draw())
        i.foreach(_.draw())
        inv_proj.foreach(_.draw())
        display.setColor(Color.WHITE)
        display.drawString(20, 30, s"Score : $score", scoreFont, Color.WHITE)
        for (i <- 0 until numLives) {
          drawHeart(100 + i * 40, 50, 30, Color.RED)
        }
      }
    }
  })

  timer.start()
}

object Spacegame extends App {
  new Game1()
}
