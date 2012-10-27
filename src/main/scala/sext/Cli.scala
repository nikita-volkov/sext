package sext

/**
 * Utilities for command-line applications
 */
object Cli {
  /**
   * Process the input args string-array while providing default values for unspecified args.
   *
   * Sample usage:
   *
   * @example {{{
   *   val (intArg, longArg, stringArg) = Cli.parseArgs(args, Seq("1", "100", "someString")){ 
   *       case Array(a, b, c) => (a.toInt, b.toLong, c)
   *     }
   * }}}
   * @param args The value of `args` field provided by [[scala.App]]
   * @param defaults Default values for unspecified args
   * @param process A function which will process a complete list of args with defaults provided for skipped args
   * @tparam Z Result of `process` function
   * @return Result of `process` function
   */
  def parseArgs
    [ Z ]
    ( args     : Array[String], 
      defaults : Seq[String] )
    ( process  : Array[String] => Z )
    : Z
    = if( args.length <= defaults.length )
        process(args ++ defaults.drop(args.length))
      else 
        throw new Exception("Too many args")


}