This is the first commit of my Swahili translator. It uses a recursive descent parser to parse English sentences and convert them to their Swahili equivalent. It is written in Mathematica, and can be loaded into Mathematica as an external package.

To anyone who doesn't know how to load a package into their Mathematica session, here's a quick tutorial:

```
AppendTo[$Path,"/path/to/something/Swahili-Translator/Mfasiri"];
AppendTo[$Path,"path/to/something/Swahili-Translator/Msingi"];

Needs["Mfasiri`"]
Needs["Msingi`"]


(* now the packages are loaded into Mathematica, but you need to create the database. For this, go to the Msingi.m file located at Msingi/Msingi/Msingi.m and change the directory of $DatabaseDirectory to something in your computer *)

CreateMsingiDatabase[]

(* if you get errors, call *)

UpdateMsingiDatabase[]

(* and there it is! now you can call things like *)

Tafsiri["I am a student."]
```
