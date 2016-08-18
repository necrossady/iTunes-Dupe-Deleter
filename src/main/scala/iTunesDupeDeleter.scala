import java.io.File
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
    // Use a trait to get Files with Scala-styled constructor
    trait sFile extends File
    object sFile { def apply(arg1:String) = new File(arg1)}
    case class FMatch   (fname : String, fext : String, f : File)
    
    // iTunes files are named in this format:
    val fileNameP = """(\d* ?.*)(\.[\d\w]*)""".r
        
    // I suppose it needs a way to run
    def main (args : Array[String]) : Unit = {
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
                    val lister = folderToFileList(tmp._2)
                    println("Working...")
                    deleter(lister())
                }
            }
        }
        while (needGoodInput){
            userInput = readLine("Please provide file path to folder with dupes: ")
            val res = validateFolder(userInput)
            needGoodInput = res._1
            file = res._2
        }
        val lister : () => List[File] = folderToFileList(file)
        println("Working...")
        deleter(lister())
        println("Done")
    }
    
    // Does something fairly obvious.
    val folderToFileList : File => () => List[File] = {
        (folder : File) => () => { folder.listFiles.toList }
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
    // Returns false if folder is empty, else true
    def deleter (files : List[File]) : Boolean = {
        files match {
            case Nil        => false 
            case x :: Nil   => true
            case x :: xs    => deleterAux(xs, regexMatcher(x))
        }
    }
    
    // deleterAux recursively traverses the List of Files.
    // If the List is empty, it returns true to say it is done.
    // If it is not empty, it calls checkAndDeleteDupes
    @tailrec 
    def deleterAux (files : List[File], oldFileMatch : FMatch) : Boolean = {
        files match {
            case Nil => true
            case x :: xs => deleterAux(xs, checkAndDeleteDupes(x, oldFileMatch))
        }
    }
    
    // Deletes the duplicate files of the previous file in the folder.
    // As of now, it may not work properly with files of multiple extensions
    //  in the same folder; though in a properly sorted library, this shouldn't
    //  happen. I may fix that later.
    // Java sorts "01 Filename 1.ext" before "01 Filename.ext" so the code here is counterintuitive.
    // Returns (newFileName, true) if the file was a dupe and deleted. 
    // Returns (newFileName, false) if the file was a unique file.
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