import Global.display
import hevs.graphics.FunGraphics

import java.awt._
import java.awt.event._
import javax.swing.Timer

// main interface, made as a global object to make functions with
// the interface in several classes (draw)
object Global {
  var display: FunGraphics = new FunGraphics(1920, 1080)
}

//class of the spaceship which the user will be able to control
class Spaceship(var x: Int, var y: Int, var size: Int, var c: Color){

  // values to verify the direction of the spaceship and if its shooting
  private var up = false
  private var down = false
  private var left = false
  private var right = false
  var shot = false

  // values for the speed of the spaceship and previous direction
  var speed = 3



  def computeDirection(): (Int, Int) = {
   //horizontal movement: +1 for right, -1 for left
    val horizon = (if(right)1 else 0) - (if(left) 1 else 0)
    //same for vertical: +1 down, -1 up
    val vertical= (if(down)1 else 0) - (if(up)1 else 0)
    (horizon, vertical)
  }

  //draw a rectangle for the spaceship
  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }


  //checks which key has been pressed for the direction and movement
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

    //checks if the key has been released to stop the action
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


//class of the invader which is the enemy
class Invader(var x: Int, var y: Int, val offsetX: Int, val offsetY: Int, var size: Int, var c: Color) {
  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }
}

//class for the projectile coming from both the spaceship and the invader
//it has a update to change of position over time
// a draw, and a check to see if the projectile is outside the interface
class Projectile(var x: Int, var y: Int, var size: Int, var speed: Int, var dx: Int, var dy: Int, c: Color) {
  def update(): Unit = {
    x += dx * speed
    y += dy * speed
  }

  def draw(): Unit = {
    display.setColor(c)
    display.drawFillRect(x, y, size, size)
  }

  def bounds(width: Int, height: Int): Boolean = {
    val coteDroit = x > width
    val coteGauche = x < 0
    val coteHaut = y < 0
    val coteBas = y > height

    coteDroit ||coteGauche ||coteHaut ||coteBas
  }
}



class Game1 {

  // player stats
  private var lives = 3
  private var score = 0

  // game state
  private var gameStarted = false
  private var gameOver = false

  // invader formation
  private var formationX = 0
  private val formationY = 0
  private var invaderDir = 1
  private val invaderSpeed = 3
  private val respawnDelay = 2500

  // projectile delay
  private var shotCooldown = 0
  private val shotFrames = 25
  private val invaderShotPeriod = 60
  private val invaderShotOffset = 20
  private var frameCount = 0

  // the spaceship
  private val spaceship = new Spaceship(960, 540, 30, Color.PINK)

  // max values for invader, projectiles and marked for dead
  private val maxInvaders = 20
  private val maxShipProj = 50
  private val maxInvProj = 50
  private val maxDead = 50

  // the invaders in a certain formation
  private val invaders: Array[Invader] = new Array(maxInvaders)
  private var invaderCount = 9
  invaders(0) = new Invader(0, 0, 80, 260, 60, Color.RED)
  invaders(1) = new Invader(0, 0, 165, 180, 60, Color.RED)
  invaders(2) = new Invader(0, 0, 255, 260, 60, Color.RED)
  invaders(3) = new Invader(0, 0, 445, 260, 60, Color.RED)
  invaders(4) = new Invader(0, 0, 515, 180, 60, Color.RED)
  invaders(5) = new Invader(0, 0, 595, 260, 60, Color.RED)
  invaders(6) = new Invader(0, 0, 735, 260, 60, Color.RED)
  invaders(7) = new Invader(0, 0, 815, 180, 60, Color.RED)
  invaders(8) = new Invader(0, 0, 905, 260, 60, Color.RED)

  // Dead invaders: (offsetX, offsetY, size, Color, deathTime)
  private val deadInvaders: Array[(Int, Int, Int, Color, Long)] = new Array(maxDead)
  private var deadCount = 0

  // the projectiles
  private val shipProj: Array[Projectile] = new Array(maxShipProj)
  private var shipProjCount = 0

  private val invProj: Array[Projectile] = new Array(maxInvProj)
  private var invProjCount = 0

  // background stars for style
  private val starCount = 300
  private val starX = Array.fill(starCount)(scala.util.Random.nextInt(1920))
  private val starY = Array.fill(starCount)(scala.util.Random.nextInt(1080))
  private val starSize = Array.fill(starCount)(scala.util.Random.nextInt(3) + 1)
  private val starColor = Array.fill(starCount)(
    if(scala.util.Random.nextBoolean()) new Color(20, 20, 80) else Color.YELLOW
  )


  // dimensions for the button start/ restart
  private val scoreFont = new Font("Lithograph", Font.BOLD, 36)
  private val buttonX = 810
  private val buttonY = 500
  private val buttonWidth = 300
  private val buttonHeight = 80



  // move, shoot with the spaceship
  private def moveship(): Unit = {
    val (dx, dy) = spaceship.computeDirection()

    // compute next position
    val nextX = spaceship.x + dx * spaceship.speed
    val nextY = spaceship.y + dy * spaceship.speed

    spaceship.x = Math.max(0, Math.min(1920 - spaceship.size, nextX))
    spaceship.y = Math.max(0, Math.min(1080 - spaceship.size, nextY))

    // Shooting
    if(spaceship.shot && shotCooldown == 0 && shipProjCount < maxShipProj){
      shipProj(shipProjCount) = new Projectile(spaceship.x + spaceship.size/2, spaceship.y, 8, 8, 0, -1, Color.CYAN)
      shipProjCount += 1
      shotCooldown = shotFrames
    }

    if(!spaceship.shot) shotCooldown = 0
    if(shotCooldown > 0) shotCooldown -= 1
  }

  // function so that the invader automatically moves and shot
  private def moveInvader(): Unit = {
    // move the formation
    formationX += invaderDir * invaderSpeed

    // check edges to make sure they don't go out of the screen
    var leftMost = Int.MaxValue
    var rightMost = Int.MinValue
    for(i <- 0 until invaderCount){
      val iv = invaders(i)
      leftMost = Math.min(leftMost, iv.offsetX + formationX)
      rightMost = Math.max(rightMost, iv.offsetX + formationX + iv.size)
    }
    if(rightMost >= 1920) invaderDir = -1
    if(leftMost <= 0) invaderDir = 1

    // update positions
    for(i <- 0 until invaderCount){
      val iv = invaders(i)
      iv.x = formationX + iv.offsetX
      iv.y = formationY + iv.offsetY
    }

    // automatic shooting
    for(i <- 0 until invaderCount){
      val iv = invaders(i)
      if((frameCount + i * invaderShotOffset) % invaderShotPeriod == 0 && invProjCount < maxInvProj){
        invProj(invProjCount) = new Projectile(iv.x + iv.size/2, iv.y + iv.size, 9, 6, 0, 1, Color.GREEN)
        invProjCount += 1
      }
    }
  }

  // update spaceship projectiles, so they are moving
  private def projectiltrajectory(): Unit = {

    var i = 0
    while(i < shipProjCount){
      val p = shipProj(i)
      p.update()
      if(p.bounds(1920,1080)){
        // remove projectile
        for(j <- i until shipProjCount-1) shipProj(j) = shipProj(j+1)
        shipProjCount -= 1
      } else i += 1
    }

    // update invader projectiles
    i = 0
    while(i < invProjCount){
      val p = invProj(i)
      p.update()
      if(p.bounds(1920,1080)){
        for(j <- i until invProjCount-1) invProj(j) = invProj(j+1)
        invProjCount -= 1
      } else i += 1
    }
  }

  // if a projectile hits something, invader dies or spaceship loses a life
  private def collisionsCheck(): Unit = {
    // spaceship hits invaders
    var i = 0
    while(i < shipProjCount){
      val proj = shipProj(i)
      var j = 0
      var hit = false
      while(j < invaderCount && !hit){
        val iv = invaders(j)
        if(proj.x < iv.x + iv.size && proj.x + proj.size > iv.x &&
          proj.y < iv.y + iv.size && proj.y + proj.size > iv.y){

          // mark invader dead
          if(deadCount < maxDead){
            deadInvaders(deadCount) = (iv.offsetX, iv.offsetY, iv.size, iv.c, System.currentTimeMillis())
            deadCount += 1
          }

          // remove invader
          for(k <- j until invaderCount-1) invaders(k) = invaders(k+1)
          invaderCount -= 1

          // remove projectile
          for(k <- i until shipProjCount-1) shipProj(k) = shipProj(k+1)
          shipProjCount -= 1

          score += 1
          hit = true
        } else j += 1
      }
      if(!hit) i += 1
    }

    // invader projectiles hit spaceship
    i = 0
    while(i < invProjCount){
      val proj = invProj(i)
      if(proj.x < spaceship.x + spaceship.size && proj.x + proj.size > spaceship.x &&
        proj.y < spaceship.y + spaceship.size && proj.y + proj.size > spaceship.y){
        lives -= 1
        for(j <- i until invProjCount-1) invProj(j) = invProj(j+1)
        invProjCount -= 1
      } else i += 1
    }

    if(lives <= 0) gameOver = true
  }

  //respawn invader function after a certain amount of time
  private def respawninvaders(): Unit = {
    val currentTime = System.currentTimeMillis()
    var i = 0
    while(i < deadCount){
      val (ox, oy, size, color, deathTime) = deadInvaders(i)
      if(currentTime - deathTime >= respawnDelay && invaderCount < maxInvaders){
        invaders(invaderCount) = new Invader(0,0,ox,oy,size,color)
        invaderCount += 1

        // remove from dead
        for(j <- i until deadCount-1) deadInvaders(j) = deadInvaders(j+1)
        deadCount -= 1
      } else i += 1
    }
  }

  // render the spaceship, invaders, background, projectiles, player stats
  private def renderGame(): Unit = {
    display.setColor(Color.BLACK)
    display.drawFillRect(0,0,1920,1080)

    drawStars()

    spaceship.draw()
    for(i <- 0 until shipProjCount) shipProj(i).draw()
    for(i <- 0 until invaderCount) invaders(i).draw()
    for(i <- 0 until invProjCount) invProj(i).draw()

    display.setColor(Color.WHITE)
    display.drawString(20,30,s"Score: $score",scoreFont,Color.WHITE)
    for(i <- 0 until lives) drawHeart(100+i*40,50,30,Color.RED)
  }

  private def drawStars(): Unit = {
    for(i <- 0 until starCount){
      display.setColor(starColor(i))
      display.drawFillRect(starX(i), starY(i), starSize(i), starSize(i))
    }
  }


  // draws 2 circle and a triangle below them to make a heart
  private def drawHeart(x: Int, y: Int, size: Int, c: Color): Unit = {
    display.setColor(c)
    val r = size/2
    display.drawFilledOval(x,y,r,r)
    display.drawFilledOval(x+r,y,r,r)
    val t = new Polygon()
    t.addPoint(x,y+r)
    t.addPoint(x+size,y+r)
    t.addPoint(x+r,y+size)
    display.drawFilledPolygon(t,c)
  }

  // resets the initial values to match the game start
  private def restartGame(): Unit ={
    lives = 3
    score = 0
    gameOver = false
    gameStarted = true
    frameCount = 0

    invaderCount = 9
    shipProjCount = 0
    invProjCount = 0
    formationX = 0

  }

  // checks if the mouse is in the rectangle's dimension
  private def checkStartbtn(mx:Int, my:Int): Boolean ={
    mx >= buttonX && my <= buttonX + buttonWidth &&
      my >= buttonY && my <= buttonY + buttonHeight
  }

  // listens to check if the mouse has been clicked
  display.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent): Unit = {
      if(!gameStarted && checkStartbtn(e.getX, e.getY)){
        gameStarted = true
      }
      else if (gameOver && checkStartbtn(e.getX, e.getY)){
        restartGame()
      }
    }
  })


  // initial game state with title, instruction, start button, when code executed
  private def drawStartScreen(): Unit = {
    display.setColor(Color.BLACK)
    display.drawFillRect(0,0,1920,1080)
    display.setColor(Color.WHITE)
    display.drawString(650,300,"SPACE INVADERS",new Font("Lithograph",Font.BOLD,72),Color.WHITE)

    display.setColor(Color.GREEN)
    display.drawFillRect(buttonX,buttonY,buttonWidth,buttonHeight)
    display.setColor(Color.BLACK)
    display.drawString(buttonX+80,buttonY+50,"START",new Font("Lithograph",Font.BOLD,36),Color.BLACK)

    val instructFont = new Font("Arial", Font.PLAIN, 24)
    display.drawString(800, 700, "WASD or Arrow Keys to Move", instructFont, Color.WHITE)
    display.drawString(900, 750, "E to Shoot", instructFont, Color.WHITE)
  }

  // final game state with title and restart button, when game lost
  private def drawGameOverScreen(): Unit = {
    display.setColor(Color.BLACK)
    display.drawFillRect(0,0,1920,1080)
    display.setColor(Color.RED)
    display.drawString(650,400,"GAME OVER",new Font("Lithograph",Font.BOLD,96),Color.RED)
    display.setColor(Color.WHITE)
    display.drawString(840,650,s"Final Score: $score",scoreFont,Color.WHITE)

    display.setColor(Color.BLUE)
    display.drawFillRect(buttonX,buttonY,buttonWidth,buttonHeight)
    display.setColor(Color.BLACK)
    display.drawString(buttonX+60,buttonY+50,"RESTART",new Font("Lithograph",Font.BOLD,36),Color.BLACK)
  }



  // main loop where everything runs from here
  private val timer = new Timer(16, (_: ActionEvent) => {
    if(!gameStarted) drawStartScreen()
    else if(gameOver) drawGameOverScreen()
    else {
      frameCount += 1
      moveship()
      moveInvader()
      projectiltrajectory()
      collisionsCheck()
      respawninvaders()
      renderGame()
    }
  })

  timer.start()





}


object Spacegame extends App {
  new Game1()
}
