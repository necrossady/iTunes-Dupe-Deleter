import java.io.File
import java.lang.SecurityException
import java.util.Arrays
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex._

/* iTunesDupeDeleter.scala
 *
 * Cassady Brunette
 *
 * Small scala program to delete the duplicates from an iTunes folder.
 * iTunes renames duplicate files specifically. The files are named:
 *  dd OriginalFileName.ext
 *  dd OriginalFileName 1.ext
 *  dd OriginalFileName 2.ext
 * and the program deletes them in that fashion.
 * The program will have an issue in the example instance:
 *  03 SongTitle.mp3
 *  03 SongTitle 2.mp3
 * where these two files are different songs. Realistically this would not happen,
 *  as these would somehow need to be the same track number on the same album;
 *  nevertheless, the program can be easily modified to also account for filesize.
 *
 * I'll probably change it later to check an entire library.
 */

object iTunesDupeDeleter {
    // Java File with Scala-styled constructor
    trait sFile extends File
    object sFile { def apply(arg1:String) = new File(arg1)}
    case class FMatch   (fname : String, fext : String, f : File)
    
    // iTunes files are named in this format:
    val fileNameP = """(\d* ?.*)(\.[\d\w]*)""".r
        
    // Exactly what it says on the tin
    val folderToFileList : File => () => List[File] = { (folder : File) => () => { folder.listFiles.toList }     }
        
    // I suppose it needs a way to run
    def main (args : Array[String]) : Unit = {
        def fileListToString (files : List[File]) : String = { files match{
                case Nil => ""
                case x :: Nil => x.getPath
                case x :: xs => x.getPath + "\n" + fileListToString(xs)
            }
        }
        def process (file : File) = {
            println("Working...")
            val res = deleter(file)
            if (res == Nil) println("Done")
            else {
                println("Error - The following files could not be deleted:\n")
                println(fileListToString(res))
                println("\nPlease make sure no files are open in another program " +
                        "and try running the program again as admin.")
            }
        }
        def validateFolder(path : String) : (Boolean, File) = {
            val f = sFile(path)
            if (f.exists && f.isDirectory) (false, f)
            else { println(f"Sorry $f%s is not a folder."); (true, f) }
        }
        var needGoodInput = true
        var userInput : String = ""
        var file = sFile(".//")
        
        if (args.length > 0) {
            for (arg <- args) {
                val tmp = validateFolder(arg)
                if(!tmp._1){
                    needGoodInput = false;
                    process(tmp._2)
                }
            }
        }
        while (needGoodInput){
            userInput = StdIn.readLine("Please provide file path to folder with dupes: ")
            val res = validateFolder(userInput)
            needGoodInput = res._1
            file = res._2
        }
        
        process(file)
    }
    
    
    def regexMatcher (file : File) : FMatch = {
        var fm = fileNameP.findAllIn(file.getName)
        
        // Interesting story: This code didn't work for the longest time. It said
        //  "No match available"... even when I was testing things I knew fit the pattern!
        //  I decided to add "println(fm)" to see if if it was actually an empty Match
        //  Iterator... and then it worked!
        // It seems the compiler tried optimizing this code in a weird way that broke it,
        //  so I needed to access fm in some way before calling its group.
        // That is why this random, not used code is here:
        fm.toString
            
        FMatch(fm.group(1), fm.group(2), file)
    }
    
    // deleter starts the process of recursively deleting the dupes.
    // Returns a list of files which could not be deleted.
    def deleter (file : File) : List[File] = {
        def deleterAux1 (files : List[File], acc : List[File]) : List[File] = {
            files match {
                case Nil        => acc
                case x :: Nil   => {
                    if (x.isDirectory) acc ::: deleter(x)
                    else acc
                }
                case x :: xs    => {
                    if (x.isDirectory) deleterAux1(xs, acc ::: deleter(x))
                    else deleterAux2(xs, regexMatcher(x), acc)
                }
            }
        }
    
        // @tailrec 
        def deleterAux2 (files : List[File], oldFileMatch : FMatch, acc : List[File]) : List[File] = {
            files match {
                case Nil => acc
                case x :: xs => {
                    if (x.isDirectory) deleterAux1(xs, acc ::: deleter(x)); 
                    else {
                        var fm : FMatch = null
                        try{
                            fm = checkAndDeleteDupes(x, oldFileMatch)
                        } catch { // If x should have been deleted, oldFileMatch will catch the same dupes:
                            case se : SecurityException => // leave fm null
                        } 
                        fm match {
                            case null   => deleterAux2(xs, fm, fm.f :: acc)
                            case _      => deleterAux2(xs, fm, acc)
                        }
                    }
                }
            }
        }  
        
        if (!file.isDirectory) file :: Nil
        else folderToFileList(file).apply match {
            case Nil        => Nil
            case x :: Nil   => {
                if (x.isDirectory) deleter(x)
                else Nil
            }
            case x :: xs    => { 
                if (x.isDirectory) deleterAux1(xs, deleter(x))
                else deleterAux2(xs, regexMatcher(x), Nil)
            }   
        }        
    }
    
    
    // Deletes the duplicate files of the previous file in the folder.
    // As of now, it may not work properly with files of multiple extensions
    //  in the same folder; though in a properly sorted library, this shouldn't
    //  happen. I may fix that later.
    // Java sorts "01 Filename 1.ext" before "01 Filename.ext" so the code here is counterintuitive.
    // Returns newFileMatch. 
    @throws(classOf[SecurityException])
    def checkAndDeleteDupes (newFile : File, oldFileMatch : FMatch) : FMatch = {
        val newFileMatch = regexMatcher(newFile)
        var wasDupe : Boolean = false;

        // duplicate files should have the same size or else they aren't actually the same
        if (oldFileMatch.f.length == newFile.length){
            val oldName = oldFileMatch.fname;
            val newName = newFileMatch.fname;
            // If a file is "ABC123etc dd.ext" it may be a duplicate.
            val dupeP = """(.*)( \d+)""".r
            var oldMatcher = dupeP.findAllIn(oldName)
            if (!oldMatcher.isEmpty){
                val newMatcher = dupeP.findAllIn(newName)
                // If the new file is ALSO "ABC123etc dd.ext" ...
                if (!newMatcher.isEmpty && oldMatcher.group(1) == newMatcher.group(1))
                    wasDupe = true
                else{ // if the new file is "ABC123etc.ext" ...
                    val newP = f"$newName%s \\d+".r
                    wasDupe = !newP.findAllIn(oldName).isEmpty
                }
            }
        }
    
        if (wasDupe) oldFileMatch.f.delete
        newFileMatch
    }
}