import java.io.File
import java.io.FileFilter
import org.scalatest._
import org.scalacheck.Gen
import scala.annotation.tailrec


class iTunesDupeDeleterTest extends UnitSpec {

    val TESTS : Map[Int, Tag] = 
        (for (i <- (1 to 9).toList) yield {
            object T extends Tag ("dupeDeleter%02d".format (i))
        (i, T)
        }).toMap
        
    import iTunesDupeDeleter._
    
    property ("Deleter - empty folder", TESTS (1)) {
        assert({
            val testDir = """.\testDir01"""
            val dir = makeFolder(testDir)
            val res = deleter(dir)
            deleteDirOnExit(dir)
            res.isEmpty
        } === true
        )        
    }
    
    property ("Deleter - folder with one file", TESTS (2)) {
        assert({
            val testDir = """.\testDir02"""
            val dir = makeFolder(testDir)
            sFile(f"$testDir%s\\01 Filename.file").createNewFile
            val res = deleter(dir)
            val fName = dir.listFiles.toList(0).getName
            deleteDirOnExit(dir)
            fName
        } === "01 Filename.file"
        )
    }
    
    
    property ("Regex Matcher", TESTS(3)){
        val testDir = """.\testDir03"""
        val dir = makeFolder(testDir)
        val f = sFile(f"$testDir%s\\01 Filename.file")
        f.createNewFile
        assert({
            regexMatcher(f)
        } === FMatch("01 Filename", ".file", f)
        )
        deleteDirOnExit(dir)
    } 
   
    property ("Deleter - folder with dupes", TESTS(4)) {
        assert({
            val testDir = """.\testDir04-1"""
            val dir = generateFolderContents(testDir, true, ".test4")
            deleter(dir)
            deleteDirOnExit(dir)
            
            for (file <- dir.listFiles.toList) yield file.getName
        } === {
            val testDir = """.\testDir04-2"""
            val dir = generateFolderContents(testDir, false, ".test4")
            deleteDirOnExit(dir)
            
            for( file <- dir.listFiles.toList) yield file.getName
        }
        )
    }
    
    property ("Deleter - folder with empty folders", TESTS(5)) {
        assert({
            val testDir = """.\testDir05-1"""
            val dir = makeFolder(testDir)
            val dirs = makeEmptyFolders(testDir, 3)
            
            deleter(dir)
            deleteDirOnExit(dir)
            
            for (file <- dir.listFiles.toList) yield file.getName
        } === {
            val testDir = """.\testDir2-2"""
            val dir = makeFolder(testDir)
            val dirs = makeEmptyFolders(testDir, 3)
            
            deleteDirOnExit(dir)
            
            for (file <- dir.listFiles.toList) yield file.getName
        }
        )
    }
    
    property ("Deleter - folder with dupes and empty folder", TESTS(6)) {
        assert({
            val testDir = """.\testDir06-1"""
            val dir = generateFolderContents(testDir, true, ".test6")
            val dir1 = makeFolder(testDir + "\\dir1")
            
            deleter(dir)
            deleteDirOnExit(dir)
            
            (for (file <- dir.listFiles.toList) yield file.getName, for(file <- dir1.listFiles.toList) yield file.getName)
        } === {
            val testDir = """.\testDir06-2"""
            val dir = generateFolderContents(testDir, false, ".test6")
            val dir1 = makeFolder(testDir + "\\dir1")
            
            deleteDirOnExit(dir)
            
            (for (file <- dir.listFiles.toList) yield file.getName, for(file <- dir1.listFiles.toList) yield file.getName)
        }
        )
    }
    
    
    
    property ("Deleter - folder with 1 folder of dupes", TESTS(7)) {
        assert({
            val upperDirPath = """.\testDir07-1"""
            val upperDir = makeFolder(upperDirPath)
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", true, ".test1")
            
            deleter(upperDir)
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files)
        } === {
            val upperDirPath = """.\testDir07-2"""
            val upperDir = makeFolder(upperDirPath)
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", false, ".test1")
            
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files)
        }
        )
    }
    
    property ("Deleter - folder with folders of dupes", TESTS(8)) {
        assert({
            val upperDirPath = """.\testDir08-1"""
            val upperDir = makeFolder(upperDirPath)
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", true, ".test1")
            val testDir2 = generateFolderContents(upperDir.getPath + "\\dir2", true, ".test2")
            
            deleter(upperDir)
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            val dir2Files = for (file <- testDir2.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files, dir2Files)
        } === {
            val upperDirPath = """.\testDir08-2"""
            val upperDir = makeFolder(upperDirPath)
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", false, ".test1")
            val testDir2 = generateFolderContents(upperDir.getPath + "\\dir2", false, ".test2")
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            val dir2Files = for (file <- testDir2.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files, dir2Files)
        }
        )
    }
    
    property ("Deleter - folder with dupes and folders of dupes", TESTS(9)) {
        assert({
            val upperDirPath = """.\testDir09-1"""
            val upperDir = generateFolderContents(upperDirPath, true, ".test")            
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", true, ".test1")
            val testDir2 = generateFolderContents(upperDir.getPath + "\\dir2", true, ".test2")
            
            deleter(upperDir)
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            val dir2Files = for (file <- testDir2.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files, dir2Files)
        } === {
            val upperDirPath = """.\testDir09-2"""
            val upperDir = generateFolderContents(upperDirPath, false, ".test")
            val testDir1 = generateFolderContents(upperDir.getPath + "\\dir1", false, ".test1")
            val testDir2 = generateFolderContents(upperDir.getPath + "\\dir2", false, ".test2")
            deleteDirOnExit(upperDir)
            
            val upperDirFiles = for (file <- upperDir.listFiles.toList) yield file.getName
            val dir1Files = for (file <- testDir1.listFiles.toList) yield file.getName
            val dir2Files = for (file <- testDir2.listFiles.toList) yield file.getName
            (upperDirFiles, dir1Files, dir2Files)
        }
        )
    }
    
    
    def generateFolderContents (loc : String, dupes : Boolean, ext : String) : File = {
        val dir = makeFolder(loc)
        for (x <- 1 to 10) {
            sFile(f"$loc%s\\$x%02d FN$ext%s").createNewFile
            if (dupes && x % 2 == 0){
                sFile(f"$loc%s\\$x%02d FN 1$ext%s").createNewFile
                if (x % 3 == 0){
                    sFile(f"$loc%s\\$x%02d FN 2$ext%s").createNewFile
                }
            }
            // Not implemented yet:
            // if (x % 5 == 0) sFile(f"$loc%s\\$x%02d Filename.txt").createNewFile
        }
        dir
    }

    def makeEmptyFolders(folderPath : String, foldersToMake : Int) : List[File] = {
        @tailrec
        def makeEmptyFoldersAux(folderPath : String, num : Int, cnt : Int, acc : List[File]) : List[File] = {
            if (cnt <= num) 
                makeEmptyFoldersAux(folderPath, num, cnt+1, makeFolder(f"$folderPath%s\\dir$cnt%s") :: acc)
            else acc
        }
        
        makeEmptyFoldersAux(folderPath, foldersToMake, 1, Nil)
    }
    
    def makeFolder (folderPath : String) : File = {
        def deleteDir (folder : File) : Unit = {
            for (file <- folder.listFiles.toList) {
                if (file.isDirectory) deleteDir(file)
                file.delete
            }
            folder.delete
        }
        
        val folder = sFile(folderPath)
        if (!folder.mkdir){
            deleteDir(folder)
            folder.mkdir
        }
        folder
    }
        
    
    def deleteDirOnExit (file : File) : Unit = {
        file.deleteOnExit
        val folders = file.listFiles(new FileFilter(){def accept(f : File) : Boolean = { f.isDirectory }}).toList
        for (folder <- folders) deleteDirOnExit(folder)
            deleteDirOnExitAux (file.listFiles.toList)   
    }
    
    def deleteDirOnExitAux (files: List[File]) : Unit = {
        files match{
            case Nil        => return
            case x :: xs    => {x.deleteOnExit; deleteDirOnExitAux(xs)}
        }
    }
    
}