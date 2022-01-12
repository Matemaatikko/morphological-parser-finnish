Morphological parser for Finnish (Suomenkielen morfologinen jäsennin)
=====================================================================

The project is based on the wordlist from https://kaino.kotus.fi/sanat/nykysuomi/. The programming language used on the project is <b>Scala 3</b>.

Usage example
=======
```
import morph_fin.parser._

val parser = new FinMorphParser
parser.parse("juoksevamme") // == List(ParsingResult(juosta,vAPar:Nom:P:Poss-P1:Act), ParsingResult(juosta,vAPar:Gen:S:Poss-P1:Act), ParsingResult(juokseva,Noun:Gen:S:Poss-P1), ParsingResult(juokseva,Noun:Nom:P:Poss-P1))
```

Terminology
===========
General
-------
- <b> Inflection: </b> (Taivutus) word formation, in which a word is modified to express different grammatical categories.
- <b> Conjugation: </b> Inflection of verbs.
- <b> Declension: </b> Inflection of non-verbs.
- <b> Lemma: </b> (Perusmuoto)  canonical form, dictionary form or citation form of a word.

Noun
----
- <b> Case: </b> (Sijamuoto) Used to express the grammatical case of a word. Examples: Nominative, Genitive, Partitive
- <b> Number: </b> (Luku) Used to express whether word is in plural or singular form.

Verbs
-----
- <b> Voice: </b> (Pääluokka) The relationship between the action (or state) that the verb expresses and the participants identified by its arguments (subject, object, etc.).

  - Active, Passive
- <b> Modus: </b> (Tapaluokka) The use of verbal inflections that allow speakers to express their attitude toward what they are saying.

  - Indicative, Pontential, Conditional, Imperative
- <b> Tempus: </b> (Aikamuoto)  Express time reference.

  - Present, Imperfect, Perfect, Pluperfect
- <b> Person: </b> (Persoona) Distinction between deictic references to participant(s) in an event; typically the distinction is between the speaker (first person), the addressee (second person), and others (third person)
- <b> Nominal form: </b> (Nominaalimuoto) Form of conjugation of verbs that are similar to nouns and can have noun inflections. 

  - Infinitive, Participle

Files
=====
Kotus - wordlist
-----------------------
###Finnish:
For a finnish explanation see: https://kaino.kotus.fi/sanat/nykysuomi/.

###English:
```
<s></s>     lemma 
<tn></tn>   inflection class number
<av></av>   consonant gradation type
```

In nouns <b>lemma</b> is singular nominative form of a word. In some cases, it can also be in plural form.
For verbs <b>lemma</b> is 1. infinitive form of a word. 

<b>Inflection class number</b> tells the inflection class of a word. 
Example inflection for each inflection class can be found from [Rules](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/rules). 

````
1-49    nouns
50      compound word with a prefix has no declension in cases 
51      compound word with a prefix has declension in cases 
52-76   verbs
77-78   verbs with partial conjugation
99      no inflection
101     pronoun
-       most compound words, adverbs and some other indeclinable words don't have a class number
````

<b>Consonant gradation type </b> 
````
#letter #strong form:weak form
A       kk : k
B       pp : p
C       tt : t
D       k : –
E       p : v
F       t : d
G       nk : ng
H       mp : mm
I       lt : ll
J       nt : nn
K       rt : rr
L       k : j
M       k : v
````

Generated files
---------------
Based on the kotus-wordlist we generated two files containing all words 
and the perfect inflection information for those words in an easy-to-use format. 
Implementing a morphological parser using these files is quite straightforward. 


###words.txt
List of words in the form: <b>[lemma]</b> <b>[root]</b> <b>[inflection class]</b>. 
Inflections from inflection class are added to a root via concatenation.

File location: [Words](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/results/words.txt)

Example:
```
ainoalaatuinen	ainoalaatui	N0
ainoalaatuisuus	ainoalaatuisuu	N1
ainoiskappale	ainoiskappale	N13
ainokainen	ainokai	N0
ainu	ainu	N26
ainutkertainen	ainutkertai	N0
ainutkertaisuus	ainutkertaisuu	N1
ainutlaatuinen	ainutlaatui	N0
ainutlaatuisuus	ainutlaatuisuu	N1
```

###inflections.txt
Connects morphological information and ending to inflection class.

File location: [Inflections](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/results/inflections.txt)

Example:
```
N0	Noun:Ade:S:Sup:Poss-S2	simmallasi
N0	Noun:Par:S:Sup:Poss-S1	sintani
N0	Noun:Abl:P:Sup:Poss-3	simmiltaan
N0	Noun:Ine:S:Sup:Poss-S2	simmassasi
N0	Noun:Ess:S:Sup	simpana
N0	Noun:Ess:S:Poss-P2	snanne
N0	Noun:All:S:Comp:Poss-P2	semmallenne
```

###Morphological abbreviations
```
Noun    Noun
Fin       Finite form of a verb
AInf      A-Infinite 
AInfL     A-Infinite long form 
EInf      E-Infinite 
MaInf     Ma-Infinite
Inf4      -minen Infinite 
Inf5      -maisilla Infinite
Inf6      -vina Infinite
Inf7      -ttavissa Infinite
vAPar     -vA Participle
nUtPar    -nUt Participle
tU        -tU Participle
agePar    Agent Participle
negPar    Negative Participle

Nom       Nominative
Gen       Genitive
Acc       Accusative
Par       Participle
Ine       Inessive
Ela       Elative
Ill       Illative
Ade       Adessive
Abl       Ablative
All       Allative
Ess       Essive
Tra       Translative
Ins       Instructive
Abe       Abessive
Com       Comitative

S         Singular
P         Plural

Neg       Negative

Act       Active
Pass      Passive

Ind       Indicative
Con       Conditional
Pot       Potential
Impera    Imperative

Pre       Present
Imp       Imperfect
Per       Perfect
Plp       Pluperfect

Gen       Used to group -nUt, -neet, -ttU type endings used in Perfect, Pluperfect 
          (and negative Imperfect) inflections. 

S1        Singular first
S2        Singular second
S3        Singular third

P1        Plural first
P2        Plural second
P3        Plural third

Comp      Comparative
Sup       Superlative

Poss-S1   Possessivesuffix of singular first
Poss-S2   Possessivesuffix of singular second
Poss-P1   Possessivesuffix of plural first
Poss-P2   Possessivesuffix of plural second
Poss-3    Possessivesuffix of third person

Adv       -sti Adverb
```


Grammatical simplifications
===========================

Our aim is to write a morphological analyser for Finnish, 
meaning that we want to divide Finnish words into morphological classes based solely on the syntax. 
For that reason, our morphological classification of Finnish words differs in some places 
from traditional Finnish grammars. 


We don't count <b>accusative</b> as a grammatical case for nouns. 
(Except on pronouns, where it can differ from genitive and nominative forms.) 

We don't separate <b>adjectives</b> from other nouns. 
We allow comparative, superlative and adverb constructions for all nouns.

In some cases, we have grouped morphologically similar looking words and inflections into single category.
For example, <i>juokseminen</i> (Verb 4. Infinitive) and <i>juokseminen</i> (Noun) are combined to 
<i>juokseminen</i> (Verb 4. Infinitive) by allowing all nominal inflections for 4. Infinite. 
Similarly, we don't construct comparative and superlative-forms for -sti-adverb-constructions, since those forms are
morphologically equivalent with Comp:P:Ins and Sup:P:Ins respectively.


Lisence
=======
The project is published under: <b> Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) </b>
[Lisence](https://creativecommons.org/licenses/by-nc/4.0/). 

© Juho Salmensuu 2021