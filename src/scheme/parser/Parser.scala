package scheme.parser

object Parser {

  def parse(input: List[String]): List[Any] = {
    parse(input, List(), List(), List())
  }
  
  def parse(input: List[String], currentList : List[Any], stack : List[List[Any]], result: List[Any]): List[Any] = {
      
      if (input.length == 0 && currentList.size > 0)
      {
       //println(currentList)
       result ++ currentList
      }
      if (input.length == 0)
      {
    	  result
      }
      else if (input.head.equals(""))
      {
        parse(input.tail, currentList, stack, result)
      }
      else if(input.head.equals("("))
      {
      	parse(input.tail, List() , if (currentList.isEmpty) {stack} else {stack :+ currentList}, result)
      }
      else if (input.head.equals(")") && stack.size == 0)
      {
    	  currentList
      }
      else if (input.head.equals(")"))
      {
    	  parse(input.tail, stack.last :+ currentList, stack.take(stack.size - 1) , result)
      }
      else 
      {
        parse(input.tail, currentList :+ input.head, stack, result)
      }
    }
  
  def tokenize(s : String) = {
    s.replace("(", " ( ").replace(")", " ) ").split("\\s+").toList
  }
  
}