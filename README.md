Morphological parser (finnish) 
==============================
(Suomenkielen morfologinen jäsennin)

Programming language: <b>Scala 3</b>

Project is based on wordlist from https://kaino.kotus.fi/sanat/nykysuomi/.

Files
=====
Kotus - wordlist
-----------------------
###Finnish:
For finnish explanation see: https://kaino.kotus.fi/sanat/nykysuomi/.

###English:
```
<s></s>     lemma 
<tn></tn>   inflection class number
<av></av>   consonant gradation type
```

In nomines <b>lemma</b> is singular nominative form of a word. In some cases it can also be in plural form.
For verbs <b>lemma</b> is 1. Infinitive form of a word. 

<b>Inflection class number</b> tells the inflection class of a word. 
Example inflection for each inflection classes can be found from [Rules](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/rules). 

````
1-49    nomines
50      compound word with prefix has no declension in cases 
51      compound word with prefix has declension in cases 
52-76   verbs
77-78   verbs with partial conjugation
99      no inflection
101     pronoun
-       some compound words, adverbs and other words are missing class number
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

Updated kotus - wordlist
-----------------------
Based on kotus-wordlist we generate file that contains inflection data for all compound words. See [document](https://github.com/Matemaatikko/morphological-parser-finnish/blob/master/src/main/files/result/kotus_updated.txt).

````
paranthesis means that body is optional.
inflection =  B:inflection-class-number(:L:gradation-letter)
-------------
W:lemma:inflection                               word
U:lemma                                          pronoun
N:lemma                                          no inflection
P:prefix                                         prefix
e:value                                          suffix with inflection class parsing failed (will be treated as indeclinable)
E:lemma(:inflection)                             word with inflection class parsing failed (will be treated as indeclinable)
S:lemma:inflection                               suffix
1:lemma:P:prefix:S:suffix:inflection             compound word with prefix having no inflection in cases
2:lemma:P:prefix:inflection:S:suffix:inflection  compound word with prefix having inflection in cases
````

Generated inflections
-----------------------
Based on kotus - wordlist we generate list that maps words to lemma and bending information. 
[Documents](https://github.com/Matemaatikko/morphological-parser-finnish/blob/master/src/main/files/result/).


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
- <b> Nominal form: </b> (Nominaalimuoto) Form of conjugation of verbs which are similar to nouns and can have noun inflections. 

  - Infinitive, Participle

Language summary
=======================
Our aim is to write morphological analyser for finnish, 
meaning that we want to divide finnish words to morphological classes based solely on the syntax. 
For that reason, our morphological classification of finnish words differs in some places 
from traditional finnish grammars. For example, we don't count <b>accusative</b> as grammatical case on nouns. 
(We still count it on pronouns as it differs from genitive and nominative.) Next, we will provide language specification
used to write the morphological analyser.

Adjectives
----------
We do not make separation between adjectives and other nouns. There is no syntaxical rules that can be used to determine
when noun is adjective and when it is not. For that reason, we left that job for sentence analysis. 
Our analyser recognises comparation forms from all nouns.

###Comparative
Ending -mpi is added genitive body, where ending -n is removed (kaunis: kauniin -> kauniimpi).

<b>Exception</b>: If word has only two syllables, and it ends with -An , then 'a' is replaced with 'e'. 
Example: halpa: halvan -> halvempi

<b>Exception words</b>: 
````
"kiva" => "kivempi", "kivampi" 
"liila" => "liilempi", "liilampi"
"metka" => "metkempi", "metkampi"
"risa" => "risempi", "risampi"
````


###Superlative

Adverbs
-------
Adverb classification is similarly left for sentence analysis. Our analyser recognises -sti ending words from all nouns.


Consonant gradation
-------------------


Missing properties (Upcoming)
=============================
- inflection of pronouns
- Possessive suffixes
- Comparation of adjectives (Requires separation of adjectives from the list)
- Clitics (Liitepartikkelit)
- Nominal forms of verbs: declension

Copyright
=========
Published under: <b> Creative Commons NonCommercial license:
[Lisence](https://creativecommons.org/licenses/by-nc/4.0/legalcode). </b>

© Juho Salmensuu 2021