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
import scala.math._

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

 //operands
case class Var( val nm : String ) extends Expr 
/*case class Pixel ( var color : Int )  extends Expr {
    // attributes
     var axis1 : Var 
     var axis2 : Var 
     //var color : Int 

    // methods
    def getColor() : Int = { color }

    def setAxis1( value : Var ) { 
        axis1 = value } 

    def setAxis2( value : Var ) { 
        axis1 = value } 

    def setColor( value : Int ) { 
        color = value } 

} // end of class Pixel */
case class Operand( val number : Double ) extends Expr 

// operators
case class Sum( x : Expr, y : Expr ) extends Expr
case class Subst( x : Expr, y : Expr ) extends Expr
case class Mult(  x : Expr, y : Expr ) extends Expr

// Evaluate the expressions to interger results
class Evaluate ( e : Expr , env : String => Double ) extends Expr { 
    val eval : Expr => Double
    = _ match {
        case Var( nm ) => env( nm )
        case Operand ( number ) => number
        //case Pixel( color ) => color
        case Sum( left , right ) => eval( left ) + eval( right )
        case Subst( left , right ) => eval( left ) - eval( right )
        case Mult( left , right ) => eval( left ) * eval( right ) 
    } 

} // end of Class Evaluate

class ExprPrinter ( e : Expr ) extends Expr {
    val print : Expr => String
        = _ match {
            case Var( nm ) => nm.toString
            case Operand ( value ) => value.toString
            case Sum( left, right ) => "( " + print( left ) + " + " + print( right ) + " )"
            case Subst( left, right ) => "( " + print( left ) + " - " + print( right) + " )"
            case Mult( left, right ) => "( " + print( left ) + " * " + print( right ) + " )" 
        }

} // end of Class ExprPrinter

/* Halide - A 'Func' object represents a pipeline stage. It's a pure
 function that defines what value each pixel should have. You
 can think of it as a computed image. */

case class Func( val nm : String , val args : List[ Var ] , val e : List[ Expr ] ) {
  
    if ( e.length == 1 ) {

        //no input image
        val inst : ExprPrinter = new ExprPrinter( e( 0 ) )

        println( "Evaluation of Expr " )
        println( inst.print( e(0) ) )

    } else if ( e.length == 3 ) { 

        for ( x <- 0 until 3 ) { 

            // fixed; only three channels (List has no size method)
            val inst : ExprPrinter = new ExprPrinter( e( x ) )

            println( "Evaluation of Expr " )
            println( inst.print( e( x ) ) ) 
        }
    }

} // end of Class Func

/* Halide::Image<int32_t> output = gradient.realize(800, 600); */
/*32-bit color is often called ARGB because it has alpha, red, green, and blue values all
packed into 32 bits. */

class Image ( img : BufferedImage ) { 
    
    val image = img

} // end of Class Image

// Main 
///////////////////////////////////////////////////////////////////////////////////////////	
object main {
    def main( args: Array[ String ] ) {

    ////////////////////////////////////////////////////////////////////////////////////////////
    //Gradient Function
    ////////////////////////////////////////////////////////////////////////////////////////////

        val x = new Var( "x" )
        println( x.nm )
        val y = new Var( "y")
        println( y.nm )
        val expr = new Sum( x , y )
        //var expr =  new Sum( new Var( "x" , 2 ) , new Var( "y" , 3 ) )  

        //fixed to test the evaluation
        val evaluateExpr: Evaluate = new Evaluate( expr  ,  Map( "x" -> 2 , "y" -> 3 ) ) 
        println( "Testing Evaluation of Expr: " )
        println( evaluateExpr.eval( expr ) )
        
        //val inst3: Func = new Func( "gradient", expr )
        val gradient = new Func( "gradient" , List( x , y ) , List( expr ) ) 
        println( "Func Name: " + gradient.nm )

    ////////////////////////////////////////////////////////////////////////////////////////////
    //Brighter Function
    ////////////////////////////////////////////////////////////////////////////////////////////

        // color packed into a single int
        //val c = new Var( "color channel")

        val r = new Var( "red channel" )
        val g = new Var( "green channel" )
        val b = new Var( "blue channel" )

        //val expr2 = new Mult( new Pixel( "x" , "y" , null ) , 1.5) 
        // outside I dont have a func yet; dont know arguments

        //val brighter = new Func( "brighter" , List( x, y ) , new Mult( new Pixel( brighter.args(0).nm , brighter.args(1) , null ) , 1.5) )
        //val px = new Pixel ( 0 )

        // color packed into a single int
        // val exprBrighter = new Mult( c , new Operand( 2 ) )
        // individual R G B channels
        val exprR = new Mult( r , new Operand( 1.2 ) )
        val exprG = new Mult( g , new Operand( 1.2 ) )
        val exprB = new Mult( b , new Operand( 1.2 ) )
        
        //val brighter = new Func( "brighter" , List( x , y , c ) , exprBrighter )
        // like this how to write the Expr object?
        val brighter = new Func( "brighter" , List( x , y , r , g , b ) , List( exprR , exprG , exprB ) )

        // variable number of arguments, def not case classes
        ///////////////////////////////////////////////////////////////////////////////////////////

        // Realize a single stage gradient pipeline
        def realize ( w : Int , h : Int , func : Func , inImg : BufferedImage ) : BufferedImage = { 

            println( "Realizing " + func.nm + " Func" )

            val width : Int = w // x axis
            val height : Int = h // y axis
            val outImg = new BufferedImage( w , h , BufferedImage.TYPE_INT_ARGB )

            if ( inImg == null ) {
            
                for ( x <- 0 until width ; y <- 0 until height) {
                    // setRGB(int x, int y, int rgb) 
                    // sets a pixel in this BufferedImage to the specified RGB value.

                    val inst = new Evaluate( func.e( 0 )  ,  Map( func.args( 0 ).nm -> x , func.args( 1 ).nm -> y ) )
                
        
                    outImg.setRGB( x , y , inst.eval( func.e( 0 ) ).toInt )
                }

            } else {

                for ( x <- 0 until width ; y <- 0 until height ) {

                    // int
                    /*00000000 00000000 00000000 11111111
                      ^ Alpha  ^ Red    ^ Green  ^ Blue */ 
                    val colour : Int = inImg.getRGB( x , y )
                    //val b : Int = ( colour ) & 0x000000FF
                    //val g : Int = ( colour >> 8 ) & 0x0000FF00
                    //val r : Int = ( colour >> 16 ) & 0x00FF0000
                    //val a : Int = ( colour >> 24 ) & 0xFF000000
                    val b : Int = colour & 0x000000FF
                    val g : Int = ( colour & 0x0000FF00 ) >> 8
                    val r : Int = ( colour & 0x00FF0000 ) >> 16
                    val a : Int = ( colour & 0xFF000000 ) >> 24
                    //a = min( 0 , a )

                    println( " ----------------------------------------------------------------- " )
                    println( "1. Type Int | at x: " + x + " , " + " y: " + y + " is: " + colour ) 
                    println( "2. Type Int | red: " + r + " - green: " + g + " - blue: " + b + " - alpha: " + a)

                    val c : Color = new Color( colour )

                    println( "3. Type Color | at x: " + x + " , " + " y: " + y + " is: " + c ) 

                    val red = c.getRed
                    val green = c.getGreen
                    val blue = c.getBlue
                    val alpha = c.getAlpha

                    println( "4. Type Color | red: " + red + " - green: " + green + " - blue: " + blue + " - alpha: " + alpha )

                    //func.e.px.color
                    //val inst = new Evaluate( exprBrighter  ,  Map( func.args(0).nm -> x , func.args(1).nm -> y , func.args(2).nm -> colour ) )
                    //val inst = new Evaluate( func.e(1)  ,  Map( func.args(0).nm -> x , func.args(1).nm -> y , func.args(2).nm -> r , func.args(3).nm -> g , func.args(3).nm -> b ) ) 

                    println ( "  ")
                    val newInts = new Array[ Int ]( 3 )
                    for ( x <- 0 until 3 ) { 

                        // fixed; only three channels ( List has no size method )
                        //check that each channel does not go over 255

                        val evaluation = new Evaluate( func.e( x )  ,  Map( func.args( 0 ).nm -> x , func.args( 1 ).nm -> y , func.args(2).nm -> r , func.args(3).nm -> g , func.args(4).nm -> b ) )
                        val num : Int =  evaluation.eval( func.e( x ) ).toInt

                        newInts( x ) = min( num , 255 ) // not proportional
                        //newInts( x ) = num
                        //println( "NewInts Array( " + x + " ) = " + newInts( x ) ) 
                        //func.args( 2 ) = Var("Red Channel")

                        val printer: ExprPrinter = new ExprPrinter( func.e( x ) )
                        println( "Evaluating Expr : " + printer.print( func.e( x ) ) + " = " + newInts( x ) )
                    }

                    val newR = newInts( 0 )
                    val newG = newInts( 1 )
                    val newB = newInts( 2 )

                    println( "5. Type Int | red: " + newR + " - green: " + newG + " - blue: " + newB + " - alpha: " + a )
                
                    //pack them into one 32bit integer
                    /*00000000 00000000 00000000 11111111
                      ^ Alpha  ^ Red    ^ Green  ^ Blue */ 

                    val fnl : Int = ( alpha << 24 ) | ( newR << 16 ) | ( newG << 8 ) | newB
                    println( "final color: " + fnl )
                    outImg.setRGB( x , y , fnl )

                }
            }

            outImg

        } // end of def realize

        ///////////////////////////////////////////////////////////////////////////////////////////
        //lesson 1
        ///////////////////////////////////////////////////////////////////////////////////////////

        //realize( 800 , 600 , gradient , null ) 

        ///////////////////////////////////////////////////////////////////////////////////////////
        //lesson 2
        ///////////////////////////////////////////////////////////////////////////////////////////

        //val inImg : BufferedImage = ImageIO.read( new File("C:\\Users\\LilliamI\\Pictures\\Img\\Tiny-wings-icon.jpg" ) )
        val inImg : BufferedImage = ImageIO.read( new File("C:\\Users\\LilliamI\\Pictures\\Img\\image.jpg" ) )
        /*val testload = inImg
        println( "Saving load test jpg" )
        ImageIO.write( testload, "jpg", new File("loadingTest.jpg") )*/

        val brighterImg = realize( inImg.getWidth , inImg.getHeight , brighter , inImg )

        println( "Saving result test jpg" )
        ImageIO.write( brighterImg, "png", new File( "brighterTest.png" ) )

        ///////////////////////////////////////////////////////////////////////////////////////////
        //Panels
        ///////////////////////////////////////////////////////////////////////////////////////////

        //val img = new BufferedImage (200,200,BufferedImage.TYPE_INT_ARGB)
        val panel = new Panel {

            override def paint( g : Graphics2D ) {
                g.drawImage( brighterImg , 0 , 0 , null )
            }

            preferredSize = new Dimension( brighterImg.getWidth , brighterImg.getHeight )
        }

        val frame = new MainFrame {
            title = "Halide"
            contents = panel
            centerOnScreen
        }

        //not saving correctly
        //frame is good
        frame.open

    } // end of def main

} // end of object main