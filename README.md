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
<tn></tn>   bending class number
<av></av>   consonant gradation type
```

In nomines <b>lemma</b> is singular nominative form of a word. In some cases it can also be in plural form.
For verbs <b>lemma</b> is 1. Infinitive form of a word. 

<b>Bending class number</b> tells the bending class of a word. 
Example bendings for each bending classes can be found from [Rules](https://github.com/Matemaatikko/morphological-parser-finnish/tree/master/src/main/files/rules). 

````
1-49    nomines
50      compound word with prefix has no bending in cases 
51      compound word with prefix has bending in cases 
52-76   verbs
77-78   verbs with partial bending
99      no bending
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
G       nk : ng [ k :  ]
H       mp : mm
I       lt : ll
J       nt : nn
K       rt : rr
L       k : j
M       k : v
````

Updated kotus - wordlist
-----------------------
We reformat kotus-list to contain more information about bending. [Document](https://github.com/Matemaatikko/morphological-parser-finnish/blob/master/src/main/files/result/kotus_updated.txt).

````
paranthesis means that body is optional.
bending =  B:bending-class-number(:L:gradation-letter)
-------------
W:lemma:bending                              word
U:lemma                                      pronoun
N:lemma                                      no bending
P:prefix                                     prefix
e:value                                      suffix with bending class parsing failed (will be treated as unbending)
E:lemma(:bending)                            word with bending class parsing failed (will be treated as unbending)
S:lemma:bending                              suffix
1:lemma:P:prefix:S:suffix:bending            compound word with prefix having no bending in cases
2:lemma:P:prefix:bending:S:suffix:bending    compound word with prefix having bending in cases
````

Bending - wordlist
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
- <b> Nominal forms: </b> (Nominaalimuodot) Form of conjugation of verbs which are similar to nouns and can have noun inflections. 

  - Infinitive, Participle

Language specifications
=======================

Consonant gradation
-------------------


Incorrect bending (To be fixed)
=============================
- The following words have incorrect bending:
  - hapan
  - jälsi
  - viedä
- Some numerals have incorrect bending. Example: 'kahdeksan'

Missing properties (Upcoming)
=============================
- Bending of pronouns
- Possessive suffixes
- Comparation of adjectives (Requires separation of adjectives from the list)
- Clitics (Liitepartikkelit)
- Nominal forms of verbs: declension

Copyright
=========
Published under: <b> Creative Commons NonCommercial license:
[Lisence](https://creativecommons.org/licenses/by-nc/4.0/legalcode). </b>

© Juho Salmensuu 2021