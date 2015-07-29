import swing._
import event._
import BorderPanel.Position._
import java.awt.{Color}
import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon

//libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2" - error

// FYI
/*Classes in Scala are static templates that can be instantiated into many objects at runtime. 
Case classes can't be extended via subclassing.
Abstract classes cannot be instanciated.
Sealed classes must have all classes on same file
Case class to be able to pattern match */

/*2 unimplemented members errors, prompted to make class absract.
You must initialize variables. If you don't, Scala assumes you're writing an abstract class and 
a subclass will fill in the initialization. 
(The compiler will tell you so if you have just a single uninitialized variable.)*/
/*var x: Int = xc
  var y: Int = yc
  def move(dx: Int, dy: Int) {
    x = x + dx
    y = y + dy
  }
override def toString(): String = "(" + x + ", " + y + ")"; */
     
/*Companion Objects; avoid the new in main
object SumMaker( x : Var, y : Var) {
  def apply() = new Sum(x,y)
  }*/

// difference between val and def:
    // val evaluates when defined
    // def evaluates on call and creates new function every time (new instance of Function1)
    // val function1 = def functiontest( z: Var , c : Var ) =>   z + c 
    // example function
    // val sum  =  (  x : Int,  y : Int  )  =>  ( x  +  y )

/* After you compile your code, you end up with .class files for each class in your program. 
These binary files are the bytecode that Java interprets to execute your program. 
The NoClassDefFoundError indicates that the classloader, which is responsible for dynamically 
loading classes, cannot find the .class file for the class that you're trying to use. */

/*A code block delimited by {} evaluates to the last expression inside it. Unit is java's void.*/

///////////////////////////////////////////////////////////////////////////////////////////
// Main Elements Classes //
///////////////////////////////////////////////////////////////////////////////////////////

// The main elements in Halide are:
     // Vars
     // Exprs
     // Funcs
     // Image
     // class Image The java.awt.Image class is the superclass that represents graphical images as rectangular arrays of pixels.
// These build the pipeline in memory.

/* Halide - Funcs are defined at any integer coordinate of its variables as
 an Expr in terms of those variables and other functions. */
sealed abstract class Expr ()

/* Halide - Var objects are names to use as variables in the definition of
 a Func. They have no meaning by themselves. */
case class Var( nm : String, va : Int ) extends Expr {
    // attributes
    /* private */ val name : String = nm //x, y domain axis; c color channel
    /* private */ val value : Int = va

    // methods
    // name
    def getVarName() : String = name
    /* def setName( nm : String ) { 
        name = nm } //val not var */

    // value
    def getVarValue() : Int = value
    /* def setValue( va : Int ) { 
        value = va } //val not var */

} // end of Class Var

// Expression operators
case class Sum( x : Expr, y : Expr ) extends Expr
case class Subst( x : Expr, y : Expr ) extends Expr
case class Mult(  x : Expr, y : Expr ) extends Expr

// Evaluate the expressions to interger results
// an evaluator object?
class Evaluate ( e : Expr ) extends Expr{ 
    val eval : Expr => Int
    = _ match {
        case Var( nm, va ) => va
        case Sum( left, right ) => eval( left ) + eval( right )
        case Subst( left, right ) => eval( left ) - eval( right )
        case Mult( left, right) => eval( left ) * eval( right ) 
    } 

} // end of Class Evaluate

class PullVars ( e : Expr ) extends Expr { 
    // var param : String = ""
    val parameters : Expr => String
    = _ match {
        case Var( nm, va ) => nm + " : " + " Var " 
        case Sum( left, right ) => parameters( left ) + ", " + parameters( right )
        case Subst( left, right ) => parameters( left ) + ", " + parameters( right )
        case Mult( left, right) => parameters( left ) + ", " + parameters( right ) 
    } 

} // end of Class PullVars

class ExprPrinter ( e : Expr ) extends Expr {
    val print : Expr => String
        = _ match {
            case Var( nm, va  ) => nm.toString
            case Sum( left, right ) => "( " + print( left ) + " + " + print( right ) + " )"
            case Subst( left, right ) => "( " + print( left ) + " - " + print( right) + " )"
            case Mult( left, right ) => "( " + print( left ) + " * " + print( right ) + " )" 
        }

} // end of Class ExprPrinter

/* Halide - A 'Func' object represents a pipeline stage. It's a pure
 function that defines what value each pixel should have. You
 can think of it as a computed image. */
class Func( nm : String, e : Expr ) {
    // attributes
    val name : String = nm 

    /////////////////////////////////////////////////////////////////////////////////////////// 
    val inst2: PullVars = new PullVars( e )
    println( inst2.parameters( e ) )
    //val params : String = inst2 // type mismatch

    val inst4: ExprPrinter = new ExprPrinter( e )
    println( "Test; Printer of Evaluation of Expr: " )
    println( inst4.print( e ) )

    // type inference; not good with recursive functions
    val func = "( " + inst2.parameters( e ) + " )" + " => "  + inst4.print( e )
    //val func : String = "def " + name + "( " + inst2.parameters( e ) + " )" + " => "  + inst4.print( e )
    /////////////////////////////////////////////////////////////////////////////////////////// 

    // methods
    def getFuncName() : String = name
    def getFuncDefinition() : String = func

} // end of Class Func

/* Halide::Image<int32_t> output = gradient.realize(800, 600); */

//color channel - scala
/*32-bit color is often called ARGB because it has alpha, red, green, and blue values all
packed into 32 bits. */
class Image ( img : BufferedImage ) { // Halide::Image<int32_t> ?
    // attributes
    val image = img
    //var bufferedImage : BufferedImage = null

    /* list of types 
    https://docs.oracle.com/javase/7/docs/api/java/awt/image/BufferedImage.html */

    // BufferedImage.TYPE_INT_ARGB 
    // Represents an image with 8-bit RGBA color components packed into integer pixels.

} // end of Class Image

//realize has this parameters not image. Image just receives an image
/*class Image ( w : Int, h : Int , tp : String ) {
    // attributes
    val width : Int = w //x
    val height : Int = h //y
    val _type : String = tp

    /* list of types 
    https://docs.oracle.com/javase/7/docs/api/java/awt/image/BufferedImage.html */

    var image = new BufferedImage (width, height, BufferedImage.TYPE_INT_ARGB)
    //var bufferedImage : BufferedImage = null

    // BufferedImage.TYPE_INT_ARGB 
    // Represents an image with 8-bit RGBA color components packed into integer pixels.

    // methods
    // width, height, type
    def getImgWidth() : Int = width
    def getImgHeight() : Int = height
    //def getImgType() : String = _type

} // end of Class Image*/

// Main 
///////////////////////////////////////////////////////////////////////////////////////////	
object main {
    def main( args: Array[String] ) {

        val x = new Var( "x", 2 )
        val y = new Var( "y", 3 )
        val expr = new Sum( x , y )
        //var expr =  new Sum( new Var( "x", 2 ), new Var( "y", 3 ) )  

        val inst: Evaluate = new Evaluate( expr )
        println( "Test; Evaluation of Expr: " )
        println( inst.eval(expr) )
        
        //val inst3: Func = new Func( "gradient", expr )
        val inst3 = new Func( "gradient", expr )
        println( "Func Name: " )
        println( inst3.name )
        // name not private
        //println( inst3.getFuncName() )
        //println( inst3.getFuncDefinition() )
        val pipelineStage = inst3.getFuncDefinition()
        println(pipelineStage)
        //println( inst.eval( "Evaluation of Expr: " expr ) )

        ///////////////////////////////////////////////////////////////////////////////////////////
        // Image practice

        /*val label = new Label {
            icon = new ImageIcon("C:\\Users\\LilliamI\\Pictures\\Wallpapers\\02.jpg")
        }*/
        //var bufferedImage : BufferedImage = null 
        //val buffered = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)  
        //bufferedImage = ImageIO.read(new File("C:\\Users\\LilliamI\\Pictures\\Wallpapers\\02.jpg")) 

        //val image: Image = new Image( buffered )

        //val image = ImageIO.read(new File("02.jpg"))
        //BufferedImage image = ImageIO.read();


        ///////////////////////////////////////////////////////////////////////////////////////////
        //Realize a single stage gradient pipeline
        def realize ( w : Int , h : Int , func : Func ) : Unit = { 
            //Unit -> change to BufferedImage after returning the value
            //generate the function from the string
            val funcString : String = func.getFuncDefinition()
            //println( funcString )

            //map string to function
            def realizeFunction( x : Int ) : String = null
            val m = Map[ String , ( Int ) => String ]( funcString -> realizeFunction )
            println( m )

            val functionTesting = m(funcString)
            //functionTesting(2,3)
            
            //val realizeFunction = func
            val width : Int = w //x axis
            val height : Int = h //y axis

            val outImage = new BufferedImage( w , h , BufferedImage.TYPE_INT_RGB )
    
            /*//loops to process pipeline stage
            for ( x <- 1 until width )
            for ( y <- 1 until height ) {
                // setRGB(int x, int y, int rgb) 
                // Sets a pixel in this BufferedImage to the specified RGB value.
                //outImage.setRGB( x , y , /*FUNC*/ )
            }*/
        
            //outImage //gets assigned to Image Object
        } //end of def realize

        realize( 800 , 600 , inst3 ) //test with Unit return type
        //assign the realize to an image object with BufferedImage return type
        //val imageTest = new Image( realize( 800 , 600 , inst3 ) )

        ///////////////////////////////////////////////////////////////////////////////////////////
        //Panels

        //val img = new BufferedImage (200,200,BufferedImage.TYPE_INT_ARGB)
        val panel = new Panel {
            override def paint( g : Graphics2D ) {
                //g.drawImage( (realize( 800 , 600 , inst3 )) , 0 , 0 , null ) //test with BufferedImage type
                //g.drawImage(image,0,0,null) //cant be applied to my class
            }
            preferredSize = new Dimension( 500 , 500 )
        }

        val frame = new MainFrame {
            title = "Halide"
            contents = panel
            centerOnScreen
        }

        frame.open

    } // end of def main
} // end of object main
///////////////////////////////////////////////////////////////////////////////////////////
