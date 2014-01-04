using System.Collections;
using System.Collections.Generic;



using System;



public class Parser {
	public const int _EOF = 0;
	public const int _ident = 1;
	public const int _intCon = 2;
	public const int _realCon = 3;
	public const int _charCon = 4;
	public const int _stringCon = 5;
	public const int maxT = 19;

	const bool T = true;
	const bool x = false;
	const int minErrDist = 2;
	
	public Scanner scanner;
	public Errors  errors;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;

public List<IDeclaration> dependecies = new List<IDeclaration>();

	public interface IDeclaration
	{
		
	};

	public enum LiteralKind
	{
		Uknown,
		Float,
		Integer,
		String,
		Char
	}

	public class LiteralDeclaration : IDeclaration
	{
		public LiteralKind Kind { get; set; }
		public string Value { get; set; }
		public string Unit { get; set; }

		public override string ToString()
		{
			if( Unit != null )
			{
				return Value + "<" + Unit + ">";
			}
			else
			{
				return Value;
			}
		}
	};

	public class MultiplyDeclaration : IDeclaration
	{
		public string Operator { get; set; }
		public IDeclaration A { get; set; }
		public IDeclaration B { get; set; }

		public override string ToString()
		{
			return " ( " + A.ToString() + " " + Operator +  " " + B.ToString() + " ) ";
		}
	} 

	public class MethodDeclaration : IDeclaration
	{
		public string Value { get; set; }
		public MethodDeclaration Namespace { get; set; }
		public MethodDeclaration Generic { get; set; }
		public bool IsMethod { get; set; }


		public override string ToString()
		{
			string prefix = Namespace == null ? string.Empty : Namespace.ToString() + ".";

			string inner = string.Empty;
			if( Arguments != null )
			{
				foreach( IDeclaration decl in Arguments ) {
					inner += decl.ToString();
					inner += ", ";
				}

				if( inner.EndsWith(", ") )
				{
					inner = inner.Substring(0, inner.Length - 2);
				}
			}
			
			

			if( Generic != null )
			{
				if( IsMethod )
				{
					return prefix + Value + "<" + Generic.ToString() + ">" + "(" + inner + ")";
				}
				else
				{
					return prefix + Value + "<" + Generic.ToString() + ">";
				}
			}
			else
			{
				if( IsMethod )
				{
					return prefix + Value + "(" + inner + ")";
				}
				else
				{
					return prefix + Value;
				}
			}			
		}


		public IDeclaration[] Arguments { get; set; }
		public MethodDeclaration() { Value = string.Empty; }
	};



	public Parser(Scanner scanner) {
		this.scanner = scanner;
		errors = new Errors();
	}

	void SynErr (int n) {
		if (errDist >= minErrDist) errors.SynErr(la.line, la.col, n);
		errDist = 0;
	}

	public void SemErr (string msg) {
		if (errDist >= minErrDist) errors.SemErr(t.line, t.col, msg);
		errDist = 0;
	}
	
	void Get () {
		for (;;) {
			t = la;
			la = scanner.Scan();
			if (la.kind <= maxT) { ++errDist; break; }

			la = t;
		}
	}
	
	void Expect (int n) {
		if (la.kind==n) Get(); else { SynErr(n); }
	}
	
	bool StartOf (int s) {
		return set[s, la.kind];
	}
	
	void ExpectWeak (int n, int follow) {
		if (la.kind == n) Get();
		else {
			SynErr(n);
			while (!StartOf(follow)) Get();
		}
	}


	bool WeakSeparator(int n, int syFol, int repFol) {
		int kind = la.kind;
		if (kind == n) {Get(); return true;}
		else if (StartOf(repFol)) {return false;}
		else {
			SynErr(n);
			while (!(set[syFol, kind] || set[repFol, kind] || set[0, kind])) {
				Get();
				kind = la.kind;
			}
			return StartOf(syFol);
		}
	}

	
	void C() {
		IDeclaration decl = null; 
		expression(out decl);
		dependecies.Add(decl); 
	}

	void expression(out IDeclaration exprs) {
		exprs = null; 
		if (StartOf(1)) {
			condExpr(out exprs);
		} else if (StartOf(2)) {
			primary(out exprs);
		} else SynErr(20);
	}

	void template(out MethodDeclaration decl) {
		decl = new MethodDeclaration(); decl.Value += la.val; 
		Expect(1);
		if (la.kind == 6) {
			Get();
			MethodDeclaration decl2 = null;  
			template(out decl2);
			decl.Generic = decl2; 
			Expect(7);
		}
	}

	void unit(LiteralDeclaration decl) {
		Expect(6);
		decl.Unit = la.val; MethodDeclaration decl2 = null; 
		template(out decl2);
		Expect(7);
	}

	void arguments(MethodDeclaration decl) {
		IDeclaration decl1 = null; List<IDeclaration> expressions = new List<IDeclaration>(); 
		expression(out decl1);
		expressions.Add(decl1); 
		while (la.kind == 8) {
			Get();
			expression(out decl1);
			expressions.Add(decl1); 
		}
		decl.Arguments = expressions.ToArray(); 
	}

	void functionCall(out MethodDeclaration decl) {
		MethodDeclaration decl1 = null, decl2 = null; 
		template(out decl1);
		while (la.kind == 9) {
			Get();
			template(out decl2);
			decl2.Namespace = decl1; decl1 = decl2; 
		}
		decl = decl1; 
		if (la.kind == 10) {
			Get();
			if (StartOf(1)) {
				arguments(decl);
			}
			Expect(11);
			decl.IsMethod = true;  
		}
	}

	void literal(out IDeclaration exprs) {
		exprs = null; 
		if (la.kind == 3) {
			LiteralDeclaration decl = new LiteralDeclaration(); decl.Value = la.val;  
			Get();
			if (la.kind == 6) {
				unit(decl);
			}
			decl.Kind = LiteralKind.Float; 
			exprs = decl; 
		} else if (la.kind == 2) {
			LiteralDeclaration decl = new LiteralDeclaration(); decl.Value = la.val;  
			Get();
			if (la.kind == 6) {
				unit(decl);
			}
			decl.Kind = LiteralKind.Integer; 
			exprs = decl; 
		} else if (la.kind == 4) {
			LiteralDeclaration decl = new LiteralDeclaration(); decl.Value = la.val;  
			Get();
			decl.Kind = LiteralKind.Char; 
			exprs = decl; 
		} else if (la.kind == 5) {
			LiteralDeclaration decl = new LiteralDeclaration(); decl.Value = la.val;  
			Get();
			decl.Kind = LiteralKind.String; 
			exprs = decl; 
		} else SynErr(21);
	}

	void unaryExpr(out IDeclaration exprs) {
		if (la.kind == 12 || la.kind == 13) {
			if (la.kind == 12) {
				Get();
			} else {
				Get();
			}
		}
		primary(out exprs);
	}

	void primary(out IDeclaration exprs) {
		exprs = null; 
		if (StartOf(3)) {
			literal(out exprs);
		} else if (la.kind == 10) {
			Get();
			expression(out exprs);
			Expect(11);
		} else if (la.kind == 1) {
			method(out 	exprs);
		} else SynErr(22);
	}

	void addExpr(out IDeclaration exprs) {
		IDeclaration a = null, b = null; exprs = null; string op = string.Empty; 
		unaryExpr(out a);
		exprs = a; 
		while (la.kind == 12 || la.kind == 13) {
			op = la.val; 
			if (la.kind == 12) {
				Get();
			} else {
				Get();
			}
			unaryExpr(out b);
			var c = new MultiplyDeclaration() { Operator = op, A = a, B = b }; a = c; 
		}
		exprs = a; 
	}

	void multiplyExpr(out IDeclaration exprs) {
		IDeclaration a = null, b = null; exprs = null; string op = string.Empty; 
		addExpr(out a);
		exprs = a; 
		while (la.kind == 14 || la.kind == 15 || la.kind == 16) {
			op = la.val; 
			if (la.kind == 14) {
				Get();
			} else if (la.kind == 15) {
				Get();
			} else {
				Get();
			}
			addExpr(out b);
			var c = new MultiplyDeclaration() { Operator = op, A = a, B = b }; a = c; 
		}
		exprs = a; 
	}

	void condExpr(out IDeclaration exprs) {
		IDeclaration a = null, b = null; exprs = null; string op = string.Empty; 
		multiplyExpr(out a);
		exprs = a; 
		while (StartOf(4)) {
			op = la.val; 
			if (la.kind == 17) {
				Get();
			} else if (la.kind == 18) {
				Get();
			} else if (la.kind == 6) {
				Get();
			} else {
				Get();
			}
			multiplyExpr(out b);
			var c = new MultiplyDeclaration() { Operator = op, A = a, B = b }; a = c; 
		}
		exprs = a; 
	}

	void method(out IDeclaration exprs) {
		MethodDeclaration decl = null; exprs = null; 
		functionCall(out decl);
		exprs = decl; 
	}



	public void Parse() {
		la = new Token();
		la.val = "";		
		Get();
		C();
		Expect(0);

	}
	
	static readonly bool[,] set = {
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x},
		{x,T,T,T, T,T,x,x, x,x,T,x, T,T,x,x, x,x,x,x, x},
		{x,T,T,T, T,T,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x},
		{x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x},
		{x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,T,T,x, x}

	};
} // end Parser


public class Errors {
	public int count = 0;                                    // number of errors detected
	public System.IO.TextWriter errorStream = Console.Out;   // error messages go to this stream
	public string errMsgFormat = "-- line {0} col {1}: {2}"; // 0=line, 1=column, 2=text

	public virtual void SynErr (int line, int col, int n) {
		string s;
		switch (n) {
			case 0: s = "EOF expected"; break;
			case 1: s = "ident expected"; break;
			case 2: s = "intCon expected"; break;
			case 3: s = "realCon expected"; break;
			case 4: s = "charCon expected"; break;
			case 5: s = "stringCon expected"; break;
			case 6: s = "\"<\" expected"; break;
			case 7: s = "\">\" expected"; break;
			case 8: s = "\",\" expected"; break;
			case 9: s = "\".\" expected"; break;
			case 10: s = "\"(\" expected"; break;
			case 11: s = "\")\" expected"; break;
			case 12: s = "\"+\" expected"; break;
			case 13: s = "\"-\" expected"; break;
			case 14: s = "\"*\" expected"; break;
			case 15: s = "\"/\" expected"; break;
			case 16: s = "\"%\" expected"; break;
			case 17: s = "\"==\" expected"; break;
			case 18: s = "\"!=\" expected"; break;
			case 19: s = "??? expected"; break;
			case 20: s = "invalid expression"; break;
			case 21: s = "invalid literal"; break;
			case 22: s = "invalid primary"; break;

			default: s = "error " + n; break;
		}
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}

	public virtual void SemErr (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}
	
	public virtual void SemErr (string s) {
		errorStream.WriteLine(s);
		count++;
	}
	
	public virtual void Warning (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
	}
	
	public virtual void Warning(string s) {
		errorStream.WriteLine(s);
	}
} // Errors


public class FatalError: Exception {
	public FatalError(string m): base(m) {}
}
