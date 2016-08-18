import java.io.File
import org.scalatest._
import org.scalacheck.Gen


class iTunesDupeDeleterTest extends UnitSpec {

    val TESTS : Map[Int, Tag] = 
        (for (i <- (1 to 4).toList) yield {
            object T extends Tag ("dupeDeleter%02d".format (i))
        (i, T)
        }).toMap
        
    import iTunesDupeDeleter._
    
    property ("Deleter - empty folder", TESTS (1)) {
        assert({
            val testDir = """.\testDir"""
            val dir = sFile(testDir)
            makeFolder(dir)        
            val res = deleter(lister(dir))
            deleteDir(dir)
            res
        } === false
        )        
    }
    
    property ("Deleter - folder with one file", TESTS (2)) {
        assert({
            val testDir = """.\testDir"""
            val dir = sFile(testDir)
            makeFolder(dir)
            sFile(f"$testDir%s\\01 Filename.file").createNewFile
            val res = deleter(lister(dir))
            val fName = dir.listFiles.toList(0).getName
            deleteDir(dir)
            fName
        } === "01 Filename.file"
        )
    }
    
    
    property ("Regex Matcher", TESTS(3)){
        val testDir = """.\testDir"""
        val dir = sFile(testDir)
        makeFolder(dir)
        val f = sFile(f"$testDir%s\\01 Filename.file")
        f.createNewFile
        assert({
            regexMatcher(f)
        } === FMatch("01 Filename", ".file", f)
        )
        deleteDir(dir)
    } 
   
    property ("Deleter - folder with dupes", TESTS(4)) {
        assert({
            val testDir = """.\testDir"""
            val dir = generateFolderContents(testDir, true)
            deleter(lister(dir))
            val files = dir.listFiles.toList
            for (file <- files) yield file.getName
        } === {
            val testDir2 = """.\testDir2"""
            val dir2 = generateFolderContents(testDir2, false)
            val res1 = for( file <- dir2.listFiles.toList) yield file.getName
            deleteDir(dir2)
            res1
        }
        )
    }
    
    def generateFolderContents (loc : String, dupes : Boolean) : File = {
        val dir = sFile(loc)
        makeFolder(dir)
        for (x <- 1 to 12) {
            sFile(f"$loc%s\\$x%02d Filename.file").createNewFile
            if (dupes && x % 2 == 0){
                sFile(f"$loc%s\\$x%02d Filename 1.file").createNewFile
                if (x % 3 == 0){
                    sFile(f"$loc%s\\$x%02d Filename 2.file").createNewFile
                }
            }
            // Not implemented yet:
            // if (x % 5 == 0) sFile(f"$loc%s\\$x%02d Filename.txt").createNewFile
        }
        dir
    }
    
    def lister : File => List[File] = {
        (folder : File) => folder.listFiles.toList
    }
    
    def makeFolder (folder : File) : Unit = {
        if (!folder.mkdir){
            deleteDir(folder)
            folder.mkdir
        }
    }
    
    def deleteDir (folder : File) : Unit = {
        val files = folder.listFiles.toList
        for (file <- files) {
            if (file.isDirectory) deleteDir(file)
            file.delete
        }
        folder.delete
    }

}