
public class Compiler
{
    public static void Main(string[] arg)
    {        
        Scanner scanner = new Scanner(arg[0]);
        Parser parser = new Parser(scanner);        
        parser.Parse();
        System.Console.WriteLine(parser.errors.count + " errors detected");

        foreach (Parser.IDeclaration decl in parser.dependecies)
        {
            System.Console.WriteLine(decl.ToString());
        }
    }
}