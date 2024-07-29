package q2

object StudentRecords {

  import scala.io.StdIn.readLine

  object StudentRecords {

    def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
      if (name.trim.isEmpty) {
        (false, Some("Name cannot be empty"))
      } else {
        try {
          val marksInt = marks.toInt
          val totalMarksInt = totalMarks.toInt
          if (marksInt < 0 || totalMarksInt < 0) {
            (false, Some("Marks and total marks must be positive integers"))
          } else if (marksInt > totalMarksInt) {
            (false, Some("Marks cannot exceed total possible marks"))
          } else {
            (true, None)
          }
        } catch {
          case _: NumberFormatException => (false, Some("Marks and total marks must be integers"))    //catch error
        }
      }
    }

    def getStudentInfo(): (String, Int, Int, Double, Char) = {
      println("Enter student name:")
      val name = readLine()

      println("Enter student marks:")
      val marks = readLine()

      println("Enter total possible marks:")
      val totalMarks = readLine()

      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)    //boolean tuple

      if (!isValid) {
        println(s"Error: ${errorMessage.get}")
        getStudentInfo()
      } else {
        val marksInt = marks.toInt
        val totalMarksInt = totalMarks.toInt
        val percentage = (marksInt.toDouble / totalMarksInt) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        (name, marksInt, totalMarksInt, percentage, grade)    //return tuple
      }
    }

    def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
      val (name, marks, totalMarks, percentage, grade) = record   //extract from record tuple
      println(s"Name: $name")
      println(s"Marks: $marks")
      println(s"Total Marks: $totalMarks")
      println(f"Percentage: $percentage%.2f%%")
      println(s"Grade: $grade")
    }

    def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
      var validData = false
      var studentInfo: (String, Int, Int, Double, Char) = null

      while (!validData) {
        println("Enter student name:")
        val name = readLine()

        println("Enter student marks:")
        val marks = readLine()

        println("Enter total possible marks:")
        val totalMarks = readLine()

        val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

        if (!isValid) {
          println(s"Error: ${errorMessage.get}")
        } else {
          val marksInt = marks.toInt
          val totalMarksInt = totalMarks.toInt
          val percentage = (marksInt.toDouble / totalMarksInt) * 100
          val grade = percentage match {
            case p if p >= 90 => 'A'
            case p if p >= 75 => 'B'
            case p if p >= 50 => 'C'
            case _ => 'D'
          }
          studentInfo = (name, marksInt, totalMarksInt, percentage, grade)
          validData = true
        }
      }
      studentInfo
    }

    def main(args: Array[String]): Unit = {
      val studentInfo = getStudentInfoWithRetry()
      printStudentRecord(studentInfo)
    }
  }


}
