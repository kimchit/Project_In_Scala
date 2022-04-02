package exe0

//Atara Ginsburg 315263442
// Kimchit Choen 211762455
//Group Number 150060.5782
import java.io._
import scala.io.Source._

object exe0 extends App {
  //counters for sales and purchases
  var purchases=0.0;
  var sales=0.0;

 // val pathName= "C:\\Users\\Owner\\IdeaProjects\\Ex0";
  val pathName ="C:\\Users\\user\\IdeaProjects\\projectInScala\\src\\main\\scala\\exe0"
  val name= pathName.split("\\\\").last;
  //create outPut file and save in fileObject
  val fileObject= new File( name+".asm")
  val printWriter = new PrintWriter(fileObject); // Passing reference of file to the printwriter

  // adds a string "###BUY <ProductName>### <Amount>*<Price>" to end of the output file
  def HandleBuy(ProductName:String,Amount:Int,Price:Float): Unit =
  {
    val l=s"### BUY <$ProductName> ###\n";
    printWriter.append(l);
    printWriter.append((Amount*Price).toString+"\n");
    purchases+= (Amount*Price);
  }
  // adds a string "$$$CELL<ProductName>$$$ <Amount>*<Price>" to end of the output file
  def HandleCell(ProductName:String,Amount:Int,Price:Float): Unit =
  {
    val l=s"$$$$$$ CELL <$ProductName>$$$$$$\n";
    printWriter.append(l);
    printWriter.append((Amount*Price).toString+"\n");
    sales+= (Amount*Price);
  }

  val directory=new File(pathName);
  if (directory.exists && directory.isDirectory)
  {  //loop over files, choose whichever end with vm
    val Files=directory.listFiles.filter(_.getName.endsWith(".vm"));
    Files.foreach// loops over files in "files"
    {
      file=>
        val fileName=file.getName.split('.').toArray;
        printWriter.write(fileName(0)); // print into output file "Ex0", files name
        printWriter.write("\n");
        val lines= fromFile(file.getName).getLines;//reads the text from the file
        lines.foreach
        { line=>
          val words= line.split(" ").toArray;
          if(line.startsWith("buy"))
            HandleBuy(words(1),words(2).toInt,words(3).toFloat);
          //HandleBuy(words.head+1, (words.head+2).toInt,(words.head+3).toFloat);
          else if (line.startsWith("cell"))
            HandleCell(words(1),words(2).toInt,words(3).toFloat);
        }
    }
  }
  //print to the screen and output file the total sales and purchases
  printWriter.write(s"Totall:\npurchases:$purchases\nsales:$sales");
  println(s"Totall:\npurchases:$purchases\nsales:$sales");
  // println(f"$purchases%1.2f");
  // println(f"$sales%1.2f");
  printWriter.close() // Closing printwriter for the file "Ex0"


}