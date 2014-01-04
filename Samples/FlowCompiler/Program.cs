using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Xml;

namespace FlowCompiler
{
    class Program
    {
        static void Main(string[] args)
        {
            Environment.ExitCode = 0;

            using (StreamWriter w = new StreamWriter(Console.OpenStandardError()))
            {
                Compiler compiler = null;

                try
                {
                    //System.Windows.Forms.MessageBox.Show(args[0]);
                    //System.Windows.Forms.MessageBox.Show(args[1]);
                    compiler = new Compiler(args[0], args[1], w);
                    compiler.Compile();
                }
                catch (Exception ex)
                {
                    try
                    {
                        compiler.Flush();
                    }
                    catch (Exception flushException)
                    {
                    }
            
                    w.WriteLine(string.Format("{0}(17,20): error CS0168: A unexpected exception occured in the build process.", args.Length > 1 ? args[0] : "FlowCompiler.exe"));
                    w.Flush();
                    Environment.Exit(0);
                }
                finally
                {
                    (compiler as IDisposable).Dispose();
                }
            }
        }
    }


    static class TopologyExtensions
    {
        #region Topological Sort

        public static IEnumerable<T> TSort<T>(this IEnumerable<T> source, Func<T, IEnumerable<T>> dependencies)
        {
            var sorted = new List<T>();
            var visited = new HashSet<T>();
            foreach (var item in source)
                Visit(item, visited, sorted, dependencies);
            return sorted;
        }

        private static void Visit<T>(T item, HashSet<T> visited, List<T> sorted, Func<T, IEnumerable<T>> dependencies)
        {
            if (!visited.Contains(item))
            {
                visited.Add(item);

                var v = dependencies(item);
                if (v != null ) {
                    foreach (var dep in v)
                        Visit(dep, visited, sorted, dependencies);
                }
                
                sorted.Add(item);
            }
        }

        #endregion
    }

    static class VariableDeclarations
    {
        static public ArgumentDeclaration[] Parse(string value)
        {
            List<string> arguments = new List<string>();

            var regex = new Regex(@"(?:\(\s*([_a-zA-Z]\w*)\s*\:\s*[_a-zA-Z]\w*\s*\))|([_a-zA-Z]\w*)");
            var matches =  regex.Matches(value);
            foreach(Match match in matches) 
            {
                if (match.Groups[1].Success)
                {
                    arguments.Add(match.Groups[1].Value);
                }
                else if (match.Groups[2].Success)
                {
                    arguments.Add(match.Groups[2].Value);
                }
                else
                {
                    System.Diagnostics.Debug.Assert(false);
                }                             
            }


            return arguments.Select(p => new ArgumentDeclaration() { Name = p }).ToArray();           
        }
    }


    public class ArgumentDeclaration
    {
        public string Name { get; set; }
    };

    public class FunctionDeclaration
    {
        public string Name { get; set; }
        public List<ArgumentDeclaration> Arguments { get; set; }
    };

    class Compiler : IDisposable
    {
        Dictionary<string, FunctionDeclaration> functions = new Dictionary<string, FunctionDeclaration>();
        string inputFile = string.Empty;
        string outputFile = string.Empty;
        XmlDocument document = new XmlDocument();
        TextWriter writer = null;
        TextWriter errors = null;

        public Stream GenerateStreamFromString(string s)
        {
            MemoryStream stream = new MemoryStream();
            StreamWriter writer = new StreamWriter(stream);
            writer.Write(s);
            writer.Flush();
            stream.Position = 0;
            return stream;
        }

        public void Flush()
        {
            writer.Flush();
        }

        private void WriteError(string filename, string message)
        {
            errors.WriteLine(string.Format("{0}(17,20): error CS0168: {1}", filename, message));
            errors.Flush();
        }

        public bool Compile()
        {
            XmlNodeList list = document.SelectNodes("/Flow/Connection");
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                string sourceNode; string targetNode;
                string sourceSocket; string targetSocket;
                TryParse(list[i].Attributes["Source"].Value, out sourceNode, out sourceSocket);
                TryParse(list[i].Attributes["Target"].Value, out targetNode, out targetSocket);

                if (document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", sourceNode)) == null)
                {
                    System.Diagnostics.Debug.Assert(false);
                }
                else if (document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", targetNode)) == null)
                {
                    System.Diagnostics.Debug.Assert(false);
                }


            }


            //document.RemoveChild();


            XmlNode o = document.SelectSingleNode("/Flow");
            if (o == null)
            {
                WriteError(inputFile, string.Format("The specified input file is not valid flow-data"));
            }
            else
            {
                if (o.Attributes["Namespace"] != null && String.IsNullOrEmpty(o.Attributes["Namespace"].Value) == false)
                {
                    writer.WriteLine(String.Format("module {0}", o.Attributes["Namespace"].Value));
                }

                writer.WriteLine();
                writer.WriteLine("open Casanova");
                writer.WriteLine("open Casanova.Input");
                writer.WriteLine("open Casanova.Core");
                writer.WriteLine("open Casanova.Coroutines");
                writer.WriteLine("open Casanova.Utilities");
                writer.WriteLine("open Casanova.Math");
                writer.WriteLine("open Casanova.Game");
                writer.WriteLine("open Casanova.Drawing");
                writer.WriteLine("open Casanova.StandardLibrary");
                writer.WriteLine("open Casanova.StandardLibrary.Core");
                writer.WriteLine("open Casanova.StandardLibrary.Physics");
                writer.WriteLine("open Casanova.StandardLibrary.UI.Controls");                              



                WriteClassHierarchy();

                WriteGameEntryPoint();
            }

            return true;
        }

        public Compiler(string inputFile, string outputFile, TextWriter errorStream)
        {
            this.inputFile = inputFile;
            this.outputFile = outputFile;
            errors = errorStream;
            writer = new StreamWriter(outputFile, false, Encoding.UTF8);
            document.Load(inputFile);


                      
        }

        private bool TryParse(string value, out string nodeidx, out string nodetarget)
        {
            string[] v = value.Split( new char[] { '.'}, 2, StringSplitOptions.None );
            if( v.Length == 2 ) 
            {
                nodeidx = v[0];
                nodetarget = v[1];
                return true;
            }
            else
            {
                nodeidx = null;
                nodetarget = null;
                return false;
            }

        }

        private void WriteClassHierarchy()
        {
            XmlNodeList list = document.SelectNodes("/Flow/Node[@Type='Entity']");
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);

                if (i == 0)
                {
                    //TODO: Handle null exception
                    int numNodes = ComputeClassMembers(x);
                    if( numNodes > 0 )
                        writer.WriteLine(string.Format("type [<{1}>] {0} = {{", x.Attributes["Name"].Value, x.Attributes["Inherits"].Value));
                    else
                        writer.WriteLine(string.Format("type [<{1}>] {0} = new() = {{}}", x.Attributes["Name"].Value, x.Attributes["Inherits"].Value));
                    WriteClassMemberHierarchy(x);
                    WriteClassMemberFunctions(x, numNodes);
                }
                else
                {
                    //TODO: Handle null exception
                    int numNodes = ComputeClassMembers(x);                    
                    if (numNodes > 0)
                        writer.WriteLine(string.Format("and [<{1}>] {0} = {{", x.Attributes["Name"].Value, x.Attributes["Inherits"].Value));
                    else
                        writer.WriteLine(string.Format("and [<{1}>] {0} = new() = {{}}", x.Attributes["Name"].Value, x.Attributes["Inherits"].Value));
                    WriteClassMemberHierarchy(x);
                    WriteClassMemberFunctions(x, numNodes);
                }
            }
        }

        private void WriteGameEntryPoint()
        {
            writer.WriteLine("");
            writer.WriteLine("let rec start_game (args : StartGameArgs) =");
            writer.WriteLine("    let physics = PhysicsWorld.Create()");


            /*
            writer.WriteLine("    let ball =");
            writer.WriteLine("      {");
            writer.WriteLine("        Circle    = physics.CreateBall(0.0f<m>, 0.0f<m>, 50.0f<m>, 1.0f<kg>, 1.0f, 0.0f, true)");
            writer.WriteLine("        Sprite    = Sprite.Create(");
            writer.WriteLine("          args.DefaultLayer,");
            writer.WriteLine("          Vector2<pixel>.Zero,");
            writer.WriteLine("          Vector2<pixel>(50.0f * 2.2f<pixel>, 50.0f * 2.2f<pixel>),");
            writer.WriteLine("          @\"BouncingBalls\\ball\")");
            writer.WriteLine("      }");
            */

            string selectedWorldName = string.Empty;
            List<string> worldList = WriteVariables();
            if (worldList.Count > 1)
            {
                WriteError(inputFile, "There are no [<CasanovaWorld>] type instanciated");
                return;
            }
            else if (worldList.Count == 0)
            {
                writer.WriteLine("    /// Generated: The main script does nothing");
                writer.WriteLine("    let world = yield_");
                selectedWorldName = "world";
                WriteError(inputFile, "Program never assigns world, please assign 'world' to a valid [CasanovaWorld] annotated type");
            }
            else
            {               
                selectedWorldName = worldList[0];
            }







                        /*
  /// We create a static body for the ground. Walls are still, so the physics bodies for them are static.
  let ground = 
    {
      
      Sprite    = Sprite.Create(
                    args.DefaultLayer,
                    Vector2<pixel>.Zero,
                    Vector2<pixel>(1000.f<pixel>, 10.0f<pixel>),
                    0.0f<rad>,
                    Vector2<pixel>.Zero,
                    @"BouncingBalls\wall", 
                    Color.White, 
                    true)
    }
                          */


 


            writer.WriteLine("    // Internal requirement. Leave *before* scripts.");
            writer.WriteLine("    let inline (!) x = immediate_lookup x");   

            WriteCalls();

            WriteThreads();

            
            var mainThreads = WriteThreadNames().Where(e => { return e.Item2.Attributes["Priority"].Value == "Main"; });
            int mainThreadsCount = mainThreads.Count();
            if (mainThreadsCount == 0)
            {
                writer.WriteLine("    /// Generated: The main script does nothing");
                writer.WriteLine("    let main = yield_");
            }
            else if (mainThreadsCount == 1)
            {
                foreach (Tuple<string, XmlNode> x in mainThreads)
                {
                    writer.WriteLine("    /// Generated: The main script does nothing");
                    writer.WriteLine(String.Format("    let main = {0}", x.Item1));
                }
            }



            StringBuilder builder = new StringBuilder();
            //builder.AppendLine(string.Format("        yield_"));

            StringBuilder builders = new StringBuilder();
            foreach (Tuple<string, XmlNode> x in WriteThreadNames()) 
            {
                if (x.Item2.Attributes["Priority"].Value == "Input")
                {
                    if (IsThreadSingle(x.Item2))
                    {
                        builders.AppendLine(string.Format("    let input = List.append input [{0}]", x.Item1));
                    }
                    else
                    {
                        builders.AppendLine(string.Format("    let input = List.append input {0}", x.Item1));
                    }
                }                
            }           



            writer.WriteLine("    /// The input script does nothing");
            writer.WriteLine("    let input = []");
            writer.Write(builders.ToString());
            writer.WriteLine("    /// The main script does nothing");
            writer.WriteLine("    {0}, main, input", selectedWorldName);
            writer.WriteLine("");

            writer.WriteLine("[<CasanovaEntryPoint>]");
            writer.WriteLine("let Run() =");
            writer.WriteLine("    let game = Game.Create(start_game, 1024, 768, false, \"Bouncing Ball\") ");
            writer.WriteLine("    game.Run()");   
        }


        private IEnumerable<Tuple<string, XmlNode>> WriteThreadNames()
        {
            XmlNodeList list = document.SelectNodes("/Flow/Node[@Type='Loaded']");
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);



                string nodeid = x.Attributes["Id"].Value;
                string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
                foreach (XmlNode d in document.SelectNodes(format))
                {
                    string srcNodeIdx, srcNodeSocket;
                    if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                    {
                        XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                        System.Diagnostics.Debug.Assert(node != null);
                        if (node.Attributes["Type"].Value == "Thread")
                        {
                            yield return new Tuple<string, XmlNode>( ConvertThreadName(node), node);
                        }
                    }
                }
            }
        }

        private IEnumerable<XmlNode> GetConnectionsOutgoing(XmlNode x, String socket, Predicate<XmlNode> predicate)
        {
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.{1}']", nodeid, socket);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (predicate.Invoke(node) == true)
                    {
                        yield return node;
                    }
                }
            }
        }

        private IEnumerable<XmlNode> GetConnectionsIncoming(XmlNode x, String socket, Predicate<XmlNode> predicate)
        {
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Source='{0}.{1}']", nodeid, socket);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Target"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (predicate.Invoke(node) == true)
                    {
                        yield return node;
                    }
                }
            }
        }


        private bool HasVariableNodeViewDependecies(XmlNode x)
        {
            System.Diagnostics.Debug.Assert(x.Attributes["Type"].Value == "Variable");
            string v = x.Attributes["Inherits"].Value;
            switch (v)
            {
                case "SpriteLayer":
                    return true;
                case "Canvas":
                    return true;
                default:
                    return false;
            }
        }

        private int ComputeClassMembers(XmlNode x)
        {
            List<XmlNode> variableList = new List<XmlNode>();
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
            XmlNodeList list = document.SelectNodes(format);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Variable")
                    {
                        variableList.Add(node);
                    }
                }
            }

            return variableList.Count;
        }
        
        private void WriteClassMemberHierarchy(XmlNode x)
        {
            List<XmlNode> variableList = new List<XmlNode>();
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);            
            XmlNodeList list = document.SelectNodes(format);
            foreach (XmlNode d in document.SelectNodes(format)) {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Variable") {
                        variableList.Add(node);
                    }
                }
            }

            variableList = variableList.OrderBy(p => HasVariableNodeViewDependecies(p) ? -1 : 0).ToList();

            foreach (XmlNode node in variableList)
            {
                writer.WriteLine(string.Format("    {0}              : {1}",
                    node.Attributes["Name"].Value,
                    node.Attributes["Inherits"].Value
                ));
            }
        }

        private void WriteClassMemberFunctions(XmlNode x, int numVariables)
        {
            //TODO: Convert to fully quallified class name.
            string fullyQuallifiedClassName = x.Attributes["Name"].Value;

            int count = 0;
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Rule") {
                        count++;
                    } else if (node.Attributes["Type"].Value == "Accessor") {
                        count++;
                    }
                }
            }


            if (count > 0)
            {
                if( numVariables > 0 )
                    writer.WriteLine("} with");
                else
                    writer.WriteLine("with");

                foreach (XmlNode d in document.SelectNodes(format))
                {
                    string srcNodeIdx, srcNodeSocket;
                    if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                    {
                        XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                        System.Diagnostics.Debug.Assert(node != null);

                        if (node.Attributes["Type"].Value == "Rule")
                        {
                            if (node.Attributes["Name"].Value.EndsWith("'"))
                            {
                                WriteError(inputFile, string.Format("Rule identifier \"{0}\" is incorrectly formatted", 
                                        node.Attributes["Name"].Value            
                                    ));
                            }

                            writer.WriteLine(string.Format("    static member {0}'(world:World,self:{1},dt:float32<s>) = ", node.Attributes["Name"].Value, fullyQuallifiedClassName));

                            string identifier = string.Empty;

                            

                            //Detect advanched usage, maybe should use roslyng to parse the statements.
                            if (node.Attributes["Inherits"].Value.Contains("(") || node.Attributes["Inherits"].Value.Contains(")"))
                            {
                                identifier = node.Attributes["Inherits"].Value;
                            }
                            else if (node.Attributes["Inherits"].Value.Contains("<") || node.Attributes["Inherits"].Value.Contains(">"))
                            {
                                identifier = node.Attributes["Inherits"].Value;
                            }                           
                            else
                            {
                                if (node.Attributes["Inherits"].Value.StartsWith("!") == true)
                                {
                                    string escaped = node.Attributes["Inherits"].Value;
                                    escaped = escaped.Remove(0, 1);
                                    identifier = string.Format("!self.{0}", escaped);
                                }
                                else
                                {
                                    identifier = string.Format("self.{0}", node.Attributes["Inherits"].Value);
                                }                                
                            }

                            
                            
                            //node.Attributes["Inherits"].Value


                            var sequences = GetConnectionsOutgoing(node, "Out", element => element.Attributes["Type"].Value.StartsWith("Sequence"));
                            if (sequences.Count() > 0)
                            {
                                writer.WriteLine(string.Format("        let s = {0}", identifier));
                                foreach (XmlNode sequenceNode in sequences)
                                {
                                    writer.WriteLine(string.Format("        let s = seq{{ for {1} in s do ", identifier, sequenceNode.Attributes["Name"].Value));
                                    writer.WriteLine(string.Format("                        if {0} then  ", sequenceNode.Attributes["Condition"].Value));
                                    writer.WriteLine(string.Format("                           yield {0} }}", sequenceNode.Attributes["Name"].Value));
                                }
                                writer.WriteLine(string.Format("        s", identifier));
                            }
                            else
                            {
                                writer.WriteLine(string.Format("        {0}", identifier));
                            }
                        }
                        else if (node.Attributes["Type"].Value == "Accessor")
                        {
                            string identifier = string.Empty;
                            if (node.Attributes["Inherits"].Value.StartsWith("!") == true)
                            {
                                string escaped = node.Attributes["Inherits"].Value;
                                escaped = escaped.Remove(0, 1);
                                identifier = string.Format("!self.{0}", escaped);
                            }
                            else
                            {
                                identifier = string.Format("self.{0}", node.Attributes["Inherits"].Value);
                            }  


                            writer.WriteLine(string.Format("    member self.{0} = {1}",
                                node.Attributes["Name"].Value, identifier));
                            //node.Attributes["Inherits"].Value
                        }                        
                    }
                }

                writer.WriteLine();
            }
            else
            {
                if( numVariables > 0 )
                    writer.WriteLine("}");
                writer.WriteLine();
            }
        }

        private void WriteThreads()
        {
            XmlNodeList list = document.SelectNodes("/Flow/Node[@Type='Loaded']");
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);
                


                string nodeid = x.Attributes["Id"].Value;
                string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
                foreach (XmlNode d in document.SelectNodes(format))
                {
                    string srcNodeIdx, srcNodeSocket;
                    if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                    {
                        XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                        System.Diagnostics.Debug.Assert(node != null);
                        if (node.Attributes["Type"].Value == "Thread")
                        {
                            WriteThreads(node);
                        }
                    }
                }


            }
        }

        private void WriteThreads(XmlNode x)
        {
            string name = ConvertThreadName(x);
            

            writer.WriteLine(string.Format("    /// The {0} script is generated", name));
            writer.WriteLine(string.Format("    let {0} = ", name));


            int count = 0;
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Yield")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Repeat")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "FunctionCall")
                    {
                        count++;
                    }                        
                    else if (node.Attributes["Type"].Value == "Wait")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "KeyDown")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "KeyUp")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "When")
                    {
                        count++;
                    }
                }
            }


            bool insideMainThread = false;
            bool insideCoroutineBlock = false;

            if (x.Attributes["Priority"].Value == "Main")
            {
                insideMainThread = true;
            }
            else
            {
                insideMainThread = false;
            }  

            if (count > 1)
            {
                if (x.Attributes["Priority"].Value == "Main")
                {
                    writer.WriteLine(string.Format("      co {{", name));
                    insideCoroutineBlock = true;
                }
                else
                {
                    writer.WriteLine(string.Format("      [", name));
                }                
            }

            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Yield")
                    {
                        writer.WriteLine(insideCoroutineBlock ? "        do! yield_" : "        yield_");
                    }
                    else if (node.Attributes["Type"].Value == "Wait")
                    {
                        writer.WriteLine(insideCoroutineBlock ? "        do! wait {0:#0.0#}f<s>" : "        wait {0:#0.0#}f<s>",
                            AttributeAsFloat(node, "Delay"));
                    }
                    else if (node.Attributes["Type"].Value == "Repeat")
                    {
                        writer.WriteLine(insideCoroutineBlock ? "        do! repeat_ (" : "        repeat_ (");
                        writer.WriteLine("          co { ");
                        //writer.WriteLine("             do! yield_");
                        WriteInlines(node, true);
                        writer.WriteLine("          } )");
                    }
                    else if (node.Attributes["Type"].Value == "FunctionCall")
                    {
                        if (node.Attributes["Object"] == null || node.Attributes["Object"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Object)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            if (insideMainThread)
                            {
                                string callName = ConvertToCallName(node);
                                writer.WriteLine("        do! {0}_CO()", callName);
                            }
                            else
                            {
                                string callName = ConvertToCallName(node);
                                writer.WriteLine("        co {{ do! {0}_CO() }}", callName);
                            }
                        }
                    }
                    else if (node.Attributes["Type"].Value == "KeyPress")
                    {
                        if (node.Attributes["Key"] == null || node.Attributes["Key"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Key)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("        wait_key_press Keys.{0} => ", node.Attributes["Key"].Value));
                            writer.WriteLine("          co {");
                            WriteInlines(node, true);
                            writer.WriteLine("          }");
                        }
                    }
                    else if (node.Attributes["Type"].Value == "When")
                    {
                        if (node.Attributes["Condition"] == null || node.Attributes["Condition"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Condition)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("        wait_condition (fun () -> ({0})) => ", node.Attributes["Condition"].Value));
                            writer.WriteLine("          co {");
                            WriteInlines(node, true);
                            writer.WriteLine("          }");
                        }
                    }
                    else if (node.Attributes["Type"].Value == "KeyDown")
                    {
                        if (node.Attributes["Key"] == null || node.Attributes["Key"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Key)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("        wait_key_down Keys.{0} => ", node.Attributes["Key"].Value));
                            writer.WriteLine("          co {");                            
                            WriteInlines(node, true);
                            writer.WriteLine("          }");
                        }                        
                    }
                    else if (node.Attributes["Type"].Value == "KeyUp")
                    {
                        if (node.Attributes["Key"] == null || node.Attributes["Key"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Key)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("        wait_key_up Keys.{0}      => ", node.Attributes["Key"].Value));
                            writer.WriteLine("          co {");                            
                            WriteInlines(node, true);
                            writer.WriteLine("          }");
                        }
                    }
                }
            }

            if (count > 1)
            {
                if (x.Attributes["Priority"].Value == "Main")
                {
                    writer.WriteLine(string.Format("      }}", name));
                }
                else
                {
                    writer.WriteLine(string.Format("      ]", name));
                }
            }


            if (count == 0)
            {
                writer.WriteLine(string.Format("        yield_", name));
            }

        }


        private List<string> WriteVariables()
        {
            //Gather all variable nodes in the list...

            List<string> worldList = new List<string>();
            List<XmlNode> variableList = new List<XmlNode>();
            variableList = variableList.Distinct().ToList();
            XmlNodeList list = document.SelectNodes("/Flow/Node[@Type='Loaded']");
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);
                string nodeid = x.Attributes["Id"].Value;
                string format = string.Format("/Flow/Connection[@Target='{0}.Game']", nodeid);
                foreach (XmlNode d in document.SelectNodes(format))
                {
                    string srcNodeIdx, srcNodeSocket;
                    if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket)) {
                        XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                        System.Diagnostics.Debug.Assert(node != null);
                        if (node.Attributes["Type"].Value == "Variable") {
                            variableList.Add(node);


                            if (node.Attributes["Inherits"] != null && node.Attributes["Inherits"].Value != null && node.Attributes["Inherits"].Value.Length > 0)
                            {
                                if (node.Attributes["Name"].Value == "world") 
                                {
                                    worldList.Add(node.Attributes["Name"].Value);                                        
                                }
                            }
                        }
                    }
                }
            }


            List<XmlNode> topologicalSort = variableList.TSort(
                 delegate(XmlNode node)
                 {
                     Console.WriteLine("Find dependecies for {0} {1}", node.Attributes["Name"].Value, node.Attributes["Type"].Value);
                     List<XmlNode> dependecies = null;                     
                     var func = GetConnectionsOutgoing(node, "Out", e => e.Attributes["Type"].Value == "Data");
                     foreach (XmlNode data in func) {                         
                         string inherits = data.Attributes["Inherits"] != null ? data.Attributes["Inherits"].Value : string.Empty;
                         if ( !String.IsNullOrWhiteSpace(inherits)) {
                             List<string> unresolvedDependecies = new List<string>();
                             if (Regex.IsMatch(inherits, @"\[(?:\s*\w+\s*;)*\s*\w+\s*]"))
                             {
                                 //user supplied data in the form of [ variable; variable; variable; ]
                                 //parse it with regex and sort topologically.
                                 var matches = Regex.Matches(inherits, @"\s*(\w+)\s*");
                                 foreach (Match match in matches)
                                     unresolvedDependecies.Add(match.Groups[1].Value);
                             }
                             else
                             {
                                 Func<IEnumerable<Parser.IDeclaration>, bool> dependecyInspector = null;
                                 dependecyInspector = x => 
                                 {
                                     foreach (Parser.IDeclaration decl in x)
                                     {
                                         Parser.MethodDeclaration method = decl as Parser.MethodDeclaration;
                                         Parser.MultiplyDeclaration mult = decl as Parser.MultiplyDeclaration;
                                         if (method != null)
                                         {
                                             Parser.MethodDeclaration first = method;
                                             for (Parser.MethodDeclaration cur = method; cur != null; first = cur, cur = cur.Namespace) ;
                                             if ( !(String.Equals(first.Value, "true", StringComparison.OrdinalIgnoreCase) || String.Equals(first.Value, "false", StringComparison.OrdinalIgnoreCase) || String.Equals(first.Value, "null", StringComparison.OrdinalIgnoreCase))) {
                                                 unresolvedDependecies.Add(first.Value);
                                             }                                             
                                             //iterate over all method dependecies.
                                             if( method.Arguments != null ) 
                                                 dependecyInspector(method.Arguments);                                             
                                         }
                                         else if (mult != null)
                                         {
                                             Parser.IDeclaration[] v = { mult.A, mult.B };
                                             dependecyInspector( v );
                                         }
                                     }

                                     return false;    
                                 };


                                 Scanner scanner = new Scanner(GenerateStreamFromString(inherits));
                                 Parser parser = new Parser(scanner);
                                 parser.errors.errorStream = new StreamWriter(new MemoryStream());
                                 parser.Parse();
                                 if (parser.errors.count == 0)
                                 {
                                     dependecyInspector(parser.dependecies);
                                 }                                                                  
                             }

                             if (unresolvedDependecies.Count > 0) 
                             {
                                 if( dependecies == null ) dependecies = new List<XmlNode>();
                                 foreach (string dep in unresolvedDependecies) {
                                     XmlNode xnode = variableList.Where(x => x.Attributes["Name"] != null &&
                                         x.Attributes["Name"].Value == dep).FirstOrDefault();
                                     if (xnode != null)
                                     {
                                         Console.WriteLine("{0} for {1} {2}", node.Attributes["Name"].Value, dep, xnode.Attributes["Id"].Value);
                                         dependecies.Add(xnode);
                                     }
                                 }
                             }                             
                         }
                     }


                     return dependecies;
                 }
             ).ToList();


            foreach (XmlNode node in topologicalSort)
            {
                WriteVariables(node);
            }

            return worldList;
        }

        private void WriteVariables(XmlNode x)
        {
            if (x.Attributes["Name"] == null || x.Attributes["Name"].Value == string.Empty)
            {
                WriteError(inputFile, string.Format("Variable {0} attribute 'Name' is empty", 
                    x.Attributes["Id"].Value ));
            }
            else if (x.Attributes["Inherits"] == null || x.Attributes["Inherits"].Value == string.Empty)
            {
                WriteError(inputFile, string.Format("Variable {0} attribute 'Inherits' is empty",
                    x.Attributes["Id"].Value));
            }
            else
            {
                int count = 0;

                List<XmlNode> dataList = new List<XmlNode>();
                string nodeid = x.Attributes["Id"].Value;
                string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
                foreach (XmlNode d in document.SelectNodes(format))
                {
                    string srcNodeIdx, srcNodeSocket;
                    if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket)) {
                        XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                        System.Diagnostics.Debug.Assert(node != null);
                        if (node.Attributes["Type"].Value == "Data") {
                            dataList.Add(node);
                            count++;
                        }
                    }
                }
                


                string inheritedValues = x.Attributes["Inherits"] != null ? x.Attributes["Inherits"].Value : string.Empty;
                if (inheritedValues == "Function")
                {                                      
                    //Parse the arguments
                    string arguments = x.Attributes["Arguments"] == null || String.IsNullOrWhiteSpace(x.Attributes["Arguments"].Value) ? string.Empty : x.Attributes["Arguments"].Value;
                    //Add a function declaration to the meta data and perform some additional data-checks.
                    FunctionDeclaration function = new FunctionDeclaration();
                    function.Name = x.Attributes["Name"].Value;
                    function.Arguments = new List<ArgumentDeclaration>();
                    function.Arguments.AddRange( VariableDeclarations.Parse(arguments) );


                    if (functions.ContainsKey(function.Name))
                    {
                        WriteError(inputFile, string.Format("Variable {0} attribute 'Name' is already defined",
                                    x.Attributes["Name"].Value));
                    }
                    else
                    {
                        writer.WriteLine(string.Format("    let {0}() {1} =",
                            x.Attributes["Name"].Value,
                            arguments));

                        writer.WriteLine("      {");

                        foreach (XmlNode node in dataList)
                        {
                            if (node.Attributes["Type"].Value == "Data")
                            {
                                writer.WriteLine(string.Format("        {0}    = {1}",
                                    node.Attributes["Name"].Value, node.Attributes["Inherits"].Value));
                            }
                        }

                        writer.WriteLine("      }");

                        functions.Add(function.Name, function);
                    }
                }
                else if (inheritedValues == "Assign")
                {
                    string arguments = x.Attributes["Arguments"] == null || String.IsNullOrWhiteSpace(x.Attributes["Arguments"].Value) ? string.Empty : x.Attributes["Arguments"].Value;

                    if (String.IsNullOrWhiteSpace(arguments))
                    {
                        writer.WriteLine(string.Format("    let {0} =", x.Attributes["Name"].Value));
                    }
                    else
                    {
                        writer.WriteLine(string.Format("    let {0} {1} =",
                            x.Attributes["Name"].Value,
                            arguments));
                    }
           
                    foreach (XmlNode node in dataList)
                    {
                        if (node.Attributes["Name"] == null)
                        {
                            writer.WriteLine(string.Format("        {0}",
                                node.Attributes["Inherits"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("        {0}    = {1}",
                                node.Attributes["Name"].Value, node.Attributes["Inherits"].Value));
                        }
                    }
                }     
                else 
                {
                    writer.WriteLine("    /// Generated class");
                    writer.WriteLine(string.Format("    let {0} =", x.Attributes["Name"].Value));
                    writer.WriteLine("      {");

                    int recordExpressions = 0;
                    foreach (XmlNode node in dataList)
                    {
                        if (node.Attributes["Type"].Value == "Data")
                        {
                            if (node.Attributes["Name"] == null || string.IsNullOrWhiteSpace(node.Attributes["Name"].Value))
                            {
                                WriteError(inputFile, string.Format("Variable {0} attribute 'Name' is empty",
                                    node.Attributes["Id"].Value));
                            }
                            else if (node.Attributes["Inherits"] == null || string.IsNullOrWhiteSpace(node.Attributes["Inherits"].Value))
                            {
                                WriteError(inputFile, string.Format("Variable {0} attribute 'Inherits' is empty",
                                    node.Attributes["Id"].Value));
                            }
                            else
                            {
                                recordExpressions += 0;
                                writer.WriteLine(string.Format("        {0}    = {1}",
                                    node.Attributes["Name"].Value, node.Attributes["Inherits"].Value));
                            }
                        }
                    }

                    writer.WriteLine("      }");

                    if (count == 0)
                    {
                        WriteError(inputFile, string.Format("Variable {0} is empty, specify one or more record expressions",
                            x.Attributes["Name"].Value));
                    }
                }             
            }            
        }




        private bool IsThreadSingle(XmlNode x)
        {
            string name = ConvertThreadName(x);
            int count = 0;
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "Yield")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "KeyDown")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "KeyUp")
                    {
                        count++;
                    }
                }
            }

            return count <= 1;
        }



        private void WriteCalls()
        {           
            XmlNodeList list = document.SelectNodes("/Flow/Node[@Type='FunctionCall']");
            List<XmlNode> topologicalSort = NodeSelector(list).TSort(
                delegate(XmlNode node) {

                    List<XmlNode> dependecies = null;
                    bool isNodeSpecial = node.Attributes["Object"].Value == "Load" || node.Attributes["Object"].Value == "Save" || node.Attributes["Object"].Value == "Quit";
                    if (isNodeSpecial == false) 
                    {
                        var func = GetConnectionsOutgoing(node, "Params", e => e.Attributes["Type"].Value == "FunctionCall");
                        if (func.Count() == 1) {
                            if (dependecies == null) dependecies = new List<XmlNode>();
                            dependecies.Add(func.First());
                        }
                    }


                    return dependecies;                    
                }
            ).ToList();


            /*
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);
                WriteCalls(x);
            }
            */

            foreach (XmlNode node in topologicalSort)
                WriteCalls(node);
        }

        private void WriteCalls(XmlNode x)
        {
            string name = ConvertToCallName(x);

            if (x.Attributes["Object"] == null)
            {
                WriteError(inputFile, string.Format("Variable {0} attribute 'Object' is empty",
                    x.Attributes["Id"].Value));
            }
            else
            {
                FunctionDeclaration functionDecl = null;
                functions.TryGetValue(x.Attributes["Object"].Value, out functionDecl);

                if (x.Attributes["Object"].Value == "Quit")
                {
                    writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                    writer.WriteLine(string.Format("    let {0}() = ", name));
                    writer.WriteLine(string.Format("        args.{0}", x.Attributes["Object"].Value));
                    writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                    writer.WriteLine(string.Format("    let {0}_CO() = ", name));
                    writer.WriteLine(string.Format("        args.{0}", x.Attributes["Object"].Value));
                }
                else if (x.Attributes["Object"].Value == "Load" || x.Attributes["Object"].Value == "Save")
                {
                    writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                    writer.WriteLine(string.Format("    let {0}() = ", name));
                    writer.WriteLine(string.Format("        args.{0}(\"BouncingBall\")", x.Attributes["Object"].Value));
                    writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                    writer.WriteLine(string.Format("    let {0}_CO() = ", name));
                    writer.WriteLine(string.Format("        args.{0}(\"BouncingBall\")", x.Attributes["Object"].Value));
                }
                else
                {
                    if (System.Diagnostics.Debugger.IsAttached)
                    {
                        if (name == "M1B84DB44")
                            System.Diagnostics.Debugger.Break();
                    }

                    string concattnatedValue = CreatePatternMatchedArgumentsFromDeclaration(x, functionDecl);
                    string value = x.Attributes["Object"].Value;
                    if (/*value.Contains(".")*/ functionDecl == null)    //For now assume dot means values go into the parenthesis
                    {
                        writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                        writer.WriteLine(string.Format("    let {0}() = ", name));
                        writer.WriteLine(String.Format("        {1}({0})", /*callName*/ concattnatedValue, x.Attributes["Object"].Value));

                        List<string> coroutineCallers = GetCoroutineCallers();
                        var num = GetConnectionsIncoming(x, "Wait", e => 
                            coroutineCallers.Contains(e.Attributes["Type"].Value)
                        );
                        if (num.Count() > 0)
                        {
                            writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                            writer.WriteLine(string.Format("    let {0}_CO() = ", name));
                            writer.WriteLine("        co {");
                            //writer.WriteLine(String.Format("          do {1}({0}())", callName, x.Attributes["Object"].Value));
                            writer.WriteLine(String.Format("          do {1}({0})", /*callName*/ concattnatedValue, x.Attributes["Object"].Value));
                            writer.WriteLine("          do! yield_");
                            writer.WriteLine("        }");
                        }
                    }
                    else                                       
                    {
                        writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                        writer.WriteLine(string.Format("    let {0}() = ", name));
                        writer.WriteLine(String.Format("        {1}() {0}", /*callName*/ concattnatedValue, x.Attributes["Object"].Value));

                        List<string> coroutineCallers = GetCoroutineCallers();
                        coroutineCallers.Add("Repeat");
                        var num = GetConnectionsIncoming(x, "Wait", e => coroutineCallers.Contains(e.Attributes["Type"].Value));
                        if (num.Count() > 0)
                        {
                            writer.WriteLine(string.Format("    /// The {0} script is generated", name));
                            writer.WriteLine(string.Format("    let {0}_CO() = ", name));
                            writer.WriteLine("        co {");
                            //writer.WriteLine(String.Format("          do {1}({0}())", callName, x.Attributes["Object"].Value));
                            writer.WriteLine(String.Format("          do {1}() {0}", /*callName*/ concattnatedValue, x.Attributes["Object"].Value));
                            writer.WriteLine("          do! yield_");
                            writer.WriteLine("        }");
                        }
                    }
                } 
            }                               
        }

        private string CreatePatternMatchedArgumentsFromDeclaration(XmlNode x, FunctionDeclaration functionDecl)
        {
            string concattnatedValue = string.Empty;

            if (functionDecl == null)
            {
                StringBuilder arguments = new StringBuilder();
                foreach (XmlNode x2 in GetConnectionsOutgoing(x, "Params", e => e.Attributes["Type"].Value == "Data" || e.Attributes["Type"].Value == "FunctionCall"))
                {
                    if (x2.Attributes["Type"].Value == "Data")
                    {
                        arguments.Append('(');
                        arguments.Append(x2.Attributes["Inherits"].Value);
                        arguments.Append(')');
                        arguments.Append(',');
                    }
                    else if (x2.Attributes["Type"].Value == "FunctionCall")
                    {
                        string callName = ConvertToCallName(x2);
                        arguments.Append('(');
                        arguments.Append(callName);
                        arguments.Append('(');
                        arguments.Append(')');
                        arguments.Append(')');
                        arguments.Append(',');
                    }
                }

                if (arguments.Length > 0) arguments.Remove(arguments.Length - 1, 1);
                concattnatedValue = arguments.ToString();
            }
            else
            {
                //Gather all nodes by name
                StringBuilder exception = new StringBuilder();
                StringBuilder arguments = new StringBuilder();
                Dictionary<string, XmlNode> nodes = new Dictionary<string, XmlNode>();
                foreach (XmlNode x2 in GetConnectionsOutgoing(x, "Params", e => e.Attributes["Type"].Value == "Data" || e.Attributes["Type"].Value == "FunctionCall"))
                {
                    if (x2.Attributes["Name"] != null)
                        nodes.Add(x2.Attributes["Name"].Value, x2);
                }

                foreach (ArgumentDeclaration argDecl in functionDecl.Arguments)
                {
                    XmlNode x2 = null;
                    if (nodes.TryGetValue(argDecl.Name, out x2))
                    {
                        if (x2.Attributes["Type"].Value == "Data")
                        {
                            arguments.Append('(');
                            arguments.Append(x2.Attributes["Inherits"].Value);
                            arguments.Append(')');
                            arguments.Append(' ');
                        }
                        else if (x2.Attributes["Type"].Value == "FunctionCall")
                        {
                            string callName = ConvertToCallName(x2);
                            arguments.Append('(');
                            arguments.Append(callName);
                            arguments.Append('(');
                            arguments.Append(')');
                            arguments.Append(')');
                            arguments.Append(' ');
                        }
                    }
                    else
                    {
                        arguments.Append('(');
                        arguments.Append(')');
                        arguments.Append(' ');
                    }
                }

                concattnatedValue = arguments.ToString();
            }
            return concattnatedValue;
        }

        private static List<string> GetCoroutineCallers()
        {
            List<string> coroutineCallers = new List<string>();
            coroutineCallers.Add("Repeat");
            coroutineCallers.Add("Thread");
            coroutineCallers.Add("KeyDown");
            coroutineCallers.Add("KeyUp");
            coroutineCallers.Add("When");
            return coroutineCallers;
        }        

        private void WriteInlines(XmlNode x, bool insideCorotineStatement)
        {
            int count = 0;
            string nodeid = x.Attributes["Id"].Value;
            string format = string.Format("/Flow/Connection[@Target='{0}.Out']", nodeid);
            foreach (XmlNode d in document.SelectNodes(format))
            {
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "FunctionCall")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Wait")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "AwaitAny")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "AwaitAll")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Set")
                    {
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Yield")
                    {
                        count++;
                    } 
                }
            }

            if (count > 1)
            {
                //TODO: Prologue  
            }

            count = 0;
            string prefixStatementsDelimiter = "do! ";
            string prefixStatements = "do! ";

            /*
            if (x.Attributes["Type"].Value == "AwaitAll")
            {
                prefixStatements = "do! runscript ";
            }
            */

            if (x.Attributes["Type"].Value == "AwaitAny") {
                prefixStatementsDelimiter = ".||> ";
            }
            else if (x.Attributes["Type"].Value == "AwaitAll") {
                prefixStatementsDelimiter = ".&&> ";
            }

            foreach (XmlNode d in document.SelectNodes(format))
            {
                string prefix = count == 0 ? prefixStatements : prefixStatementsDelimiter;
                string srcNodeIdx, srcNodeSocket;
                if (TryParse(d.Attributes["Source"].Value, out srcNodeIdx, out srcNodeSocket))
                {                    
                    XmlNode node = document.SelectSingleNode(string.Format("/Flow/Node[@Id='{0}']", srcNodeIdx));
                    System.Diagnostics.Debug.Assert(node != null);
                    if (node.Attributes["Type"].Value == "FunctionCall")
                    {
                        string name = ConvertToCallName(node);
                        writer.WriteLine(string.Format("             {1} {0}_CO()", name, prefix));
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Yield")
                    {
                        writer.WriteLine(string.Format("             yield_"));
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "Set")
                    {
                        if (node.Attributes["Name"] == null || String.IsNullOrWhiteSpace(node.Attributes["Name"].Value))
                        {
                            //error
                        }
                        else if (node.Attributes["Value"] == null || String.IsNullOrWhiteSpace(node.Attributes["Value"].Value))
                        {
                            //error
                        }
                        else
                        {
                            string n = node.Attributes["Name"].Value;
                            string v = node.Attributes["Value"].Value;
                            v = v.Replace("self", n);
                            writer.WriteLine(string.Format("             {2} {0} := {1}", n, v, prefix == "do! " ? "do " : prefix));
                            count++;
                        }                        
                    }
                    else if (node.Attributes["Type"].Value == "Let")
                    {
                        if (node.Attributes["Name"] == null || String.IsNullOrWhiteSpace(node.Attributes["Name"].Value))
                        {
                            //error
                        }
                        else if (node.Attributes["Value"] == null || String.IsNullOrWhiteSpace(node.Attributes["Value"].Value))
                        {
                            //error
                        }
                        else
                        {
                            string n = node.Attributes["Name"].Value;
                            string v = node.Attributes["Value"].Value;
                            v = v.Replace("self", n);
                            writer.WriteLine(string.Format("             {2} {0} <- {1}", n, v, prefix == "do! " ? "do " : prefix));
                            count++;
                        }                        
                    }                    
                    else if (node.Attributes["Type"].Value == "Wait")
                    {
                        string name = ConvertToCallName(node);
                        float delay = AttributeAsFloat(node, "Delay");
                        writer.WriteLine(string.Format("             {1} wait {0:#0.0#}f<s>", delay, prefix));
                        count++;                     
                    }
                    else if (node.Attributes["Type"].Value == "AwaitAny")
                    {
                        string name = ConvertToCallName(node);
                        //writer.WriteLine(string.Format("             do! wait_key_up Keys.Back .||> wait 0.2f<s>", delay));
                        WriteInlines(node, insideCorotineStatement);
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "AwaitAll")
                    {
                        string name = ConvertToCallName(node);
                        //writer.WriteLine(string.Format("             do! wait_key_up Keys.Back .||> wait 0.2f<s>", delay));
                        WriteInlines(node, insideCorotineStatement);
                        count++;
                    }
                    else if (node.Attributes["Type"].Value == "KeyUp")
                    {
                        if (node.Attributes["Key"] == null || node.Attributes["Key"].Value == string.Empty)
                        {
                            WriteError(inputFile, string.Format("Node {0} is missing an argument (Key)",
                                node.Attributes["Id"].Value));
                        }
                        else
                        {
                            writer.WriteLine(string.Format("             {1} wait_key_up Keys.{0}      => ", node.Attributes["Key"].Value, prefix));
                            writer.WriteLine("                   co {");
                            writer.WriteLine("                      do! yield_");
                            //WriteInlines(node, true);
                            writer.WriteLine("                   }");
                            count++;
                        }
                    }
                }
            }

            if (count > 1)
            {
                //TODO: Epilogue
            }
            else if( count == 0 )
            {
                if (insideCorotineStatement == true)
                {
                    writer.WriteLine(string.Format("             do! yield_")); 
                }
                else
                {
                    writer.WriteLine(string.Format("             yield_"));                    
                }
            }
        }

        private void WriteInline(XmlNode x)
        {

        }


        private IEnumerable<XmlNode> NodeSelector(XmlNodeList list)
        {
            for (int i = 0, j = list.Count - 1; i < list.Count; i++)
            {
                XmlNode x = list.Item(i);
                yield return x;
            }            
        }

        #region Attribute Conversions

        private static float AttributeAsFloat(XmlNode x, string name)
        {
            float v = 0.0f;
            float.TryParse(x.Attributes[name].Value, out v);
            return v;
        }

        #endregion

        #region Name Conversions

        private static string ConvertThreadName(XmlNode x)
        {
            string name = string.Empty;
            string v = x.Attributes["Designer.Name"].Value;
            string s = x.Attributes["Id"].Value.ToLowerInvariant();
            if (v == s)
            {
                name = "T" + v.ToUpperInvariant();
            }
            else
            {
                name = v;
            }
            return name;
        }

        private static string ConvertToCallName(XmlNode x)
        {
            string name = string.Empty;
            string v = x.Attributes["Designer.Name"].Value;
            string s = x.Attributes["Id"].Value.ToLowerInvariant();
            if (v == s)
            {
                name = "M" + v.ToUpperInvariant();
            }
            else
            {
                name = v;
            }
            return name;
        }

        #endregion

        #region IDisposable Members

        void IDisposable.Dispose()
        {
            if( writer != null )
                writer.Dispose();
            writer = null;
        }

        #endregion
    };


}
