package com.github.kmizu.treep.lexer

final case class Token(kind: String, lexeme: String, line: Int, col: Int)

object Lexer:
  private val keywords = Set(
    "def","const","let","return","if","else","while","for","match","case",
    "struct","module","import","in","returns","true","false"
  )

  def tokenize(input: String, file: String = "<stdin>"): List[Token] =
    val len = input.length
    val buf = scala.collection.mutable.ListBuffer.empty[Token]
    var i = 0
    var line = 1
    var col = 1

    def peek(n: Int = 0): Char = if (i + n < len) input.charAt(i + n) else 0.toChar
    def emit(kind: String, lex: String, l: Int, c: Int): Unit = buf += Token(kind, lex, l, c)
    def adv(): Char =
      val ch = peek()
      i += 1
      if ch == '\n' then { line += 1; col = 1 } else { col += 1 }
      ch

    def skipWhitespace(): Unit =
      var done = false
      while !done do
        peek() match
          case ch if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' => adv()
          case '/' if peek(1) == '/' =>
            while { val c = peek(); c != 0 && c != '\n' } do adv()
          case _ => done = true

    while i < len do
      skipWhitespace()
      val startL = line
      val startC = col
      val ch = peek()
      if ch == 0 then ()
      else if ch.isLetter || ch == '_' then
        val sb = new StringBuilder
        sb.append(adv())
        while { val c = peek(); c.isLetterOrDigit || c == '_' } do sb.append(adv())
        val word = sb.toString
        val kind = if keywords.contains(word) then word.toUpperCase else "IDENT"
        emit(kind, word, startL, startC)
      else if ch.isDigit then
        val sb = new StringBuilder
        sb.append(adv())
        while { val c = peek(); c.isDigit } do sb.append(adv())
        emit("INT", sb.toString, startL, startC)
      else if ch == '"' then
        adv() // consume opening quote
        val sb = new StringBuilder
        var closed = false
        while i < len && !closed do
          peek() match
            case '\\' =>
              adv(); // escape
              val e = adv()
              e match
                case 'n' => sb.append('\n')
                case '"' => sb.append('"')
                case '\\' => sb.append('\\')
                case other => sb.append(other)
            case '"' => adv(); closed = true
            case 0 => closed = true
            case c => sb.append(adv())
        emit("STRING", sb.toString, startL, startC)
      else
        // operators and punctuations (check multi-char first)
        val two = s"${ch}${peek(1)}"
        val three = if i + 2 < len then s"${ch}${peek(1)}${peek(2)}" else ""
        val matched = three match
          case "->" => false // handled in two-char
          case _ => false
        if !matched then
          two match
            case "==" | "!=" | "<=" | ">=" | "&&" | "||" | "->" | "=>" =>
              emit(two, two, startL, startC); adv(); adv()
            case _ =>
              ch match
                case '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | ';' | '.' =>
                  emit(ch.toString, ch.toString, startL, startC); adv()
                case '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '!' =>
                  emit(ch.toString, ch.toString, startL, startC); adv()
                case _ =>
                  // unknown char: emit ERROR token and advance
                  emit("ERROR", ch.toString, startL, startC)
                  adv()

    buf += Token("EOF", "", line, col)
    buf.toList
