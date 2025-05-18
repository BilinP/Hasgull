{-|
Module      : Docs.FAQ
Description : Frequently Asked Questions about the design of the Hasgull language.

This documentation page addresses several questions regarding the design of the Hasgull language.

= A short explanation of what the language is
Somewhat Haskell-like language with a form of typeclassing. Users can create abstract functions and then later implement them for types, these can be our primitives, such as Int, Boolean, or Void, or it can be a user-defined struct. Functions can have the same names but are different.

=How do I compile your compiler?
To compile, you'll need to use Stack. First, run __stack build__ to build the project (if necessary). Pretty much no other special setup is needed. Other then installing the current version of Stack and Haskell. You can find the installation instructions for Stack at https://docs.haskellstack.org/en/stable/README/ abnd the Haskell installation instructions at https://www.haskell.org/downloads/. 

=How do I run your compiler?
Then use __stack exec Hasgull-exe firstarg secondarg__ to run the compiler. The compilation process will only begin if firstarg is a file with the .gull extension and exactly two command-line arguments are provided. The second argument is the name of the output file. The compiler will generate a JavaScript file with the same name as the second argument, but with a .js extension.




Example wrong usage:

@
stack exec Hasgull-exe test.py ant 
@
This will fail since test.py is not a .gull file.

@
stack exec Hasgull-exe sample.gull output flag 
@
This will fail since there are too many arguments.

Example correct usage:

@
stack exec Hasgull-exe sample.gull output
@
This will create a file named output.js with the compiled JavaScript code. where if you have Node.js installed, you can run the output file with: 
    
@
node {output.js} (file name)
@

=How do I run your tests?
To run the test cases (already defined in our code) for each component of our compiler in our code, you can use pattern matching for that specific test part:

For Tokenizer

@
stack test-arguments="--pattern=Tokenizer"
@

For Parser

@
stack test --test-arguments="--pattern=Parser"
@

Code Generation

@
stack test --test-arguments="--pattern=Generation"
@

Or to just run them all

@
stack test 
@

Each stage builds upon the previous one. For example, the Parser depends on the Tokenizer to produce tokens before parsing, and Code Generation depends on both the Tokenizer and the Parser to generate the final output.





=Code snippets in your language highlighting features and edge cases, along with relevant explanations

For example, to demonstrate our typeclass system, which works similarly to Java's interface mechanism, we define a base trait with a method called add that takes another value of the same type and returns a result of that type. Then we use an impl block for a specific type to define the actual method implementation. Structs can also be used as valid types in this system, following the same trait and impl. We also have functions that take parameters and return values

@
trait Addable {
method add(other: Self): Self;
}
trait Printable {
method print(): Void;
}
struct IntWrapper {
value: Int
}
impl Addable for Int {
method add(other: Int): Int {
return self + other;
}
}


impl Addable for IntWrapper {
method add(other: IntWrapper): IntWrapper {
return new IntWrapper { value: self.value + other.value };
}

}

impl Printable for Boolean {
    method print(): Void{
        println(1);
    }
}

impl Printable for Int {
method print(): Void {
println(self);
}
}

impl Printable for IntWrapper {
method print(): Void {
println(self.value);
}
}

func bob(a: Int, x:Int): Int {
    if( a < x){
        return a;
    } else{
        return x;
    }

}
@

Which then compiles into this.

@
class IntWrapper {
  constructor(value) {
    this.value = value;
  }
}

Number.prototype.add = function(other) {return this+other; };IntWrapper.prototype.add = function(other) {return new IntWrapper(this.value+other.value); };Boolean.prototype.print = function() {console.log(1);};Number.prototype.print = function() {console.log(this);};IntWrapper.prototype.print = function() {console.log(this.value);};function bob(a, x) {if( a<x) {return a; } else {return x; }}
@

The javascript output is messy, but if the programmer really cares about that, then they can clean it up on their own time or easily format the code .


However, we do somewhat have a checker if the user actually created a trait to be later given definition with impl for a type. 
If you just do impl Addable for Int { ……} and try to compile. It will spit out out:

@
Hasgull-exe.EXE: Unknown trait: Addable
CallStack (from HasCallStack):
@

If statement in between a while stmt that compiles into javascript.

@
INPUT:
while(x < 5){
    a3 = a3 + 5;
    println(a3);
    if(a3 == 17){
        break;
    }
    x = x + 1;
}

OUTPUT:
while(x<5) {a3=a3+5;  console.log(a3); if( a3===17) {break;} x=x+1; }
@


You can do binary expressions with different types and will compile, but what happens after is really up to javascript.

@
INPUT (with the above code for typeclasses):

let a5: Boolean = true;
let a2: IntWrapper = new IntWrapper { value: 7 };
let a3: Int = a1.add(2);
let a4: IntWrapper = a2.add(new IntWrapper { value: 3 });
a3.print();
a4.print();
a5=a5+a1

OUTPUT:
let a5 = true;let a2 = new IntWrapper(7);let a3 = a1.add(2);let a4 = a2.add(new IntWrapper(3));a3.print();a4.print();a5=a5+1
@

Due to no type checker, you can add two variables of different typing and assign it to another type. In this case, we add a voiid type variable to a boolean variable and assign it to an int variable. This is undefined behavior because the runtime is dependent on the javascript runtime.

@
INPUT:
let a: Boolean = true;
let x: Int = 10;
let y: Int = 20;
let fup: Void = theWhiler(x, y);
let goo: Void = theL();
fup;
goo;
y=a+fup;

OUTPUT:
let a = true;let x = 10;let y = 20;let fup = theWhiler(x,y);let goo = theL();fup;goo;y=a+fup; 
@


A cool feature of our language is its ability to ignore comments, thanks to a tokenizer that’s designed to skip over them. This allows programmers to include comments in the source language for clarity, while the compiled output is clean and free of comments, ensuring execution without the clutter. 


@
removeComments :: String -> String
removeComments [] = []
removeComments ('/' : '/' : rest) = removeComments (dropWhile (/= '\n') rest)
removeComments (keep : check) = keep : removeComments check
@

In this code it first pattern matches for a  "\/\/" and then drops until it sees the new line character. One obvious limitation edge case would be multiple line comments\/End of line comments which could be fairly easy to add looking for if its looks for  \/* and the end *\/ but at that moment we thought based on this being quirk and the time factor we decided to just keep it as is. 

== Why this language, and why this language design?

* __William__:  
  "I thought it would be easy, seeing as I worked mostly with Java since that was the target language, but didn't expect how different Haskell would be to code it in."

* __Sebastian__:  
  "I was mostly interested in the meta language, because I’ve never really touched a functional language, and seeing that the compiler was written in Haskell really intrigued me. It definitely is a very interesting language, and doing nothing but JavaScript and Java for the past two years really did wear me down from Object-Oriented languages. Don’t get me wrong, megaparsec is a really, really handy library for creating a parser, but I just want to try a functional language rather than stick with Java or JavaScript."

* __Angel__:  
  "It seemed fun and I wanted to work with Haskell. I was interested in the functional programming paradigm and it seemed like a great way to get a deep dive into how it worked. The idea behind TypeClasses and ADT is novel to me and it reduced a bunch of boilerplate."

* __Bilin__:  
  "Honestly, Haskell wouldn't have been my first choice, but since everyone else wanted to use it, I decided to go with the flow. Looking back now, it actually seems like one of the easier languages to implement a compiler in; thanks to features like pattern matching and the fact that it's not OOP, we're basically able to do each part of the compiler in one file. The main reason we chose this language design is that, after selecting Haskell, this was pretty much the only project that actually referenced Haskell."

== What kind of problems might this language solve, and why was it designed this way?

Hasgull is designed for front-end projects, due to the target language being JavaScript. Its goal was to emulate TypeClasses allowing methods to be reused across various types. Time constraints meant that the generics, ADT, and recursive types aspects were omitted. As a result, the intended TypeClass system ended up resembling Java interfaces but for a specific group of Types and Structs. The language enforces that those implementing Traits must write a method for that type.

This method enforcement increases code performance and predictability. By bringing types to JavaScript and enforcing this Trait system, JavaScript benefits from monomorphic optimizations, which can improve performance. In addition, enforcing proper implementations makes the code less error-prone and more predictable.

== Known limitations

* __Lack of typechecking__

    *No real typechecker exists beyond verifying if a trait has been implemented for a given `impl prog def`.
    *Example: `let x: Boolean = true;` followed by `y = x * 2;` is valid syntax from the compiler’s perspective, but the output JavaScript might not behave as the programmer intends.
    *The compiler does not verify type correctness beyond trait implementation.

* __No runtime variable initialization checks:__

    *The compiler doesn't track whether a variable (like identifier i in the AST) was initialized during code generation.
    *Due to time constraints, scaffolding for variable tracking wasn't implemented.

* __Limited primitive types:__

    *Only three primitive data types are supported.
    *There are no character or string primitives.

* __Poor higher-order function support:__

    *Although parsing for higher-order functions exists, the syntax is awkward and not recommended for use.

* __No native support for arrays or lists:__

    *No support for arrays or lists:

== Knowing what you know now, what would you do differently?

* __Sebastian__: One big thing honestly would be to put a more critical eye towards our productions because our current productions are traitor-like. When we started working on the tokenizer and parser, we ended up adding extra symbols and statements simply because it felt strange not to have things like a greater-than operator or a for-loop I feel that in terms of the higher order type, we probably couldn’t do different grammar just so that it could better be translated, because it’s such a weird format in comparison to the other types that we couldn't fully get it operational.Keep the target language, because honestly it did somewhat make the translation part a bit easier because we could just convert statements and expressions straight into the javascript equivalent. Plus, with javascript being dynamically typed, and allowing for weird expressions like add a boolean and integer together, it somewhat was like a typechecker. For the communication, I think it was fine, we first just talked about how we were going to implement each phase and then in those phases, we said what parts were going to be done. We would have a bounty board where someone says they will do the parsing for types, or statements, or defining our tokens

* __Angel__: I had trouble with our development environment at first that I kind of just powered through. The linter didn’t work well so I kept running into indenting issues that held me back at certain times. I should have fixed or addressed it early on because it would have gotten rid of something I did not have to think about. The target language being JavaScript was a good choice because besides the Trait and Implementation details it was pretty much a one to one in terms of transferring it over. The communication was fine, but I think we should have followed your initial advice and split into groups into 2 people per section. As at certain points 2 would have been enough to fulfill certain aspects of the project and would have increased our productivity. I think we could have been better about sharing resources and the overall goal of the language. We learned as we went along as was needed

* __Bilin__: Knowing what I know now, I would have approached the project task assignment differently. Starting work well before the deadline would have helped avoid the time crunch we faced. Our approach was to set up a task board and let team members choose what they wanted to work on, but a more structured method of assigning tasks might have improved our workflow, especially since I believe some people did more than others. I wouldn’t change much about the development tools we used, but one thing I wish we had tried was implementing a bottom-up parser (which we did try to implement with happy but had to scrap it for megaparser) which uses grammars reduction as the foundation. From what I’ve read, it tends to be faster and could have optimized our parser. As for the target language, I’d still choose Haskell. It made many things easier once I got used to it. That said, if you had asked me before this class, I probably would’ve picked a language we were all familiar with to make implementation smoothly, but can’t really complain with the amount of work we were able to accomplish with a new language none of us had experience with





= An explanation of the state of your compiler 
The compiler itself works pretty much 95%. While we don’t have any typechecker, and the higher order type couldn’t really make the cut for code generation(We don’t have enough time honestly to create a special case for it in the generator, and looking at our syntax for function definitions, I’m not really sure how to best translate it- Sebastian). Our tokenizer, parser, and generator work. You can go stack build to build it, then do stack exec Hasgull-exe input.gull outputName and it will compile that code in the input.gull into a /outputName.js/ file with equivalent javascript code.


=Information about what you've learned on the project
*__Angel__:This project taught me a lot about the process of building a compiler as well as benefits of different programming language paradigms. I learned how to build a Lexor, Parser, and CodeGenerator. The time that can be saved by choosing a language that supports pattern matching and Algebraic Data Types. We did not have to make unnecessary classes. That Decisions have to be made due to time constraints and it can just be blamed on the programmer. 

*__Sebastian__: This project was pretty much my sole experience learning haskell, which while strange, is honestly quite a neat language. The megaparsec library too, which is a really robust library for creating parsers that was really useful for us. For the code generation portion, a lot of it was learning just how to best translate into javascript, because we did really have to think about how the base language would look in javascript to run how the programmer would run it.

*__Bilin__: This project, especially the main goal of creating a compiler, taught me a lot. I never realized what a compiler does in the back and this class and project taught me the key components such as Tokenizer, Parser, Type Checker (I created a mock-up but didn’t have enough time to fully implement it), and Code Generator. Using Haskell for the first time was challenging. It was difficult to understand certain concepts without relying on example code or using AI tools to explain things. Despite the initial learning curve, our team was happy with the decision to use Haskell. It allowed us to avoid unnecessary classes which we often saw in the class examples. At one point, we considered making the output from the Code Generator more human-readable. However, we realized that the purpose of a compiler is to generate output for machines to understand, not humans. Because of that, we decided to keep what we have and drop any further plans for  “readable code”. Overall, this project came with a steep learning curve, but it was worth it. I even got to learn how to use GitHub Pages and GitHub Actions to automate the build documentation and updating/publishing for the site.
-}
module Docs.FAQ where
